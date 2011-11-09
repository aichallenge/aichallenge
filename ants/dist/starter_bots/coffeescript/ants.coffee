LAND_TYPES = {"WATER", "LAND", "ANT", "DEAD", "FOOD", "HILL"}

# a holder for game information - map size, etc.
CONFIG =
  turntime : 0
  rows : 0
  cols : 0
  turn : 0

# a list of constants of possible config commands provided
CONFIG_COMMANDS = [
   "loadtime", "turntime", "rows", "cols",
   "turn", "turns", "viewradius2",
   "attackradius2", "spawnradius2"
]


class Game
  constructor: -> @MAP = new Map()

  class Map
    constructor: ->
      # a holder for all water seen since the start
      # each turn the game engine introduces only water
      # which hadn't been seen so far
      @seen_water = []

    reset: ->
      # set all the map to "is_land"
      for x in [0...CONFIG.rows]
        for y in [0...CONFIG.cols]
          if y is 0
            @[x] = []
          @[x][y] = new Location [x, y], type=LAND_TYPES.LAND

      # add the water seen so far
      for w in @seen_water
        @[w.x][w.y] = w

    search: (fn) ->
      result = []
      for x in [0...CONFIG.rows]
        for y in [0...CONFIG.cols]
           if fn(@[x][y])
             result.push @[x][y]
      return result

  class Location
    constructor: (data, @type) ->
      [@x, @y] = data

  class Ant extends Location
    constructor: (data, @type=LAND_TYPES.ANT, @is_alive=yes) ->
      [@x, @y, @owner] = data

  class Hill extends Location
    constructor: (data, @type=LAND_TYPES.HILL) ->
      [@x, @y, @owner] = data

  # The main game loop
  run: (bot) ->
    partialline = ""
    process.stdin.resume()
    process.stdin.setEncoding('utf8')
    process.stdin.on 'data', (chunk) =>
      lines = chunk.split("\n")
      # Append possible partial line
      lines[0] = partialline + lines[0]
      partialline = ""
      # If the last line is complete, there should be an
      # empy string at the end of the array
      if lines[lines.length-1] != ""
        partialline = lines[lines.length-1]
        lines = lines[0...lines.length-1]
      for line in lines
        state = @parse line.trim()
        switch state
          when "turn"
            if CONFIG.turn > 0 then @MAP.reset()
          when "ready"
            @MAP.reset()
            bot.ready()
            @finish_turn()
          when "go"
            @turn_start_time = new Date().getTime()
            bot.do_turn()
            @finish_turn()
          when "end"
            # The game is over.
            process.exit()

  parse: (line) ->
    [command, data...] = line.split /\W+/
    if command in CONFIG_COMMANDS
      CONFIG[command] = parseInt(data[0])
    else if command in ["f", "w", "h", "a", "d"]
      if data.length < 2 then return command # bad data
      data = (parseInt(_) for _ in data)
      [x,y] = [data[0], data[1]]
      switch command
        when "f"
          @MAP[x][y] = new Location data, type=LAND_TYPES.FOOD
        when "w"
          water = new Location data, type=LAND_TYPES.WATER
          @MAP[x][y] = water
          @MAP.seen_water.push water
        when "h"
          @MAP[x][y] = new Hill data
        when "a"
          @MAP[x][y] = new Ant data
        when "d"
          @MAP[x][y] = new Ant data, is_alive=no
    return command

  # gets array of Location objects for the food for this turn
  food: -> @MAP.search (_) -> _.type is LAND_TYPES.FOOD
  # gets array of Location objects for water for ALL turns since the start
  water: -> @MAP.search (_) -> _.type is LAND_TYPES.WATER
  # gets array of Location objects for the dead ants for this turn
  dead: -> @MAP.search (_) -> _.type is LAND_TYPES.ANT and not _.is_alive
  # gets array of Ant objects for the player's ants for this turn
  my_ants: ->
    @MAP.search (_) -> _.type is LAND_TYPES.ANT and _.owner is 0 and _.is_alive
  # gets array of Ant objects for the enemy's ants for this turn
  enemy_ants: ->
    @MAP.search (_) -> _.type is LAND_TYPES.ANT and _.owner isnt 0 and _.is_alive
  enemy_hills: ->
    @MAP.search (_) -> _.type is LAND_TYPES.HILL and _.owner isnt 0
  my_hills: ->
    @MAP.search (_) -> _.type is LAND_TYPES.HILL and _.owner is 0

  # any location which is not water is passable
  passable: (x, y) -> @MAP[x][y].type isnt LAND_TYPES.WATER

  # Puts orders for the ant at MAP[x][y] to head to one of 'N','E','S','W'
  issue_order: (x, y, direction) ->
    console.log("o #{x} #{y} #{direction}")

  finish_turn: ->
    console.log("go")
    process.stdout.flush()

  # returns the Euclidean distance between 2 Location objects
  distance: (loc1, loc2) ->
    dr = Math.min(Math.abs(loc1.x-loc2.x), CONFIG.rows-Math.abs(loc1.x-loc2.x))
    dc = Math.min(Math.abs(loc1.y-loc2.y), CONFIG.cols-Math.abs(loc1.y-loc2.y))
    Math.sqrt(Math.pow(dr, 2) + Math.pow(dc, 2))

  # direction is one of 'N', 'S', 'W', 'E'
  # The function returns a new Location object of the neighbor in the
  # "direction" given. If the ant crossed tha map border, it will show
  # on the other side.
  neighbor: (x, y, direction) ->
    switch direction
      when "N"
        if x-1 < 0 then @MAP[CONFIG.rows-1][y] else @MAP[x-1][y]
      when "S"
        if x+1 > CONFIG.rows-1 then @MAP[0][y] else @MAP[x+1][y]
      when "E"
        if y+1 > CONFIG.cols-1 then @MAP[x][0] else @MAP[x][y+1]
      when "W"
        if y-1 < 0 then @MAP[x][CONFIG.cols-1] else @MAP[x][y-1]

  # A naive algorithm which does not take water on map into account.
  # It will return a direction ('N', 'E', 'W', 'S') for heading from
  # source towards destination (both being Location objects)
  # I have just ported the function from the Python starter bot.
  direction: (source, destination) ->
    [x1, y1, x2, y2] = [source.x, source.y, destination.x, destination.y]
    half_height = parseInt(CONFIG.rows / 2)
    half_width  = parseInt(CONFIG.cols / 2)
    d = []
    if x1 < x2
      if x2 - x1 >= half_height
        d.push("N")
      if x2 - x1 <= half_height
        d.push("S")
    if x2 < x1
      if x1 - x2 >= half_height
        d.push "S"
      if x1 - x2 <= half_height
        d.push "N"
    if y1 < y2
      if y2 - y1 >= half_width
        d.push "W"
      if y2 - y1 <= half_width
        d.push "E"
    if y2 < y1
      if y1 - y2 >= half_width
        d.push "E"
      if y1 - y2 <= half_width
        d.push "W"
    return d

  # Time left since the last "go" command
  time_remaining: ->
    CONFIG.turntime - (new Date().getTime() - @turn_start_time)

  # ------------------------------------------------------------------
  # Library functions for demonstrating the usage of the ants library:

  # returns the nearby food locations sorted by distance
  nearby_food:  (location) ->
    comparing_distances = (loc1, loc2) =>
      @distance location, loc1 > @distance location, loc2
    @food().sort comparing_distances

(exports ? this).Game = Game
(exports ? this).LAND_TYPES = LAND_TYPES
(exports ? this).CONFIG = CONFIG
(exports ? this).CONFIG_COMMANDS = CONFIG_COMMANDS
