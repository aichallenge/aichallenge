CONFIG = require('./ants').CONFIG
game = require('./ants').Game
ants = new game()

directions = ['N', 'E', 'S', 'W']

class Bot
  # You can setup stuff here, before the first turn starts:
  ready: ->

  # Here are the orders to the ants, executed each turn:
  do_turn: -> 
    my_ants = ants.my_ants()
    for ant in my_ants
      for dir in directions
        near_square = ants.neighbor(ant.x, ant.y, dir)
        if ants.passable(near_square.x, near_square.y)
          ants.issue_order(ant.x, ant.y, dir)
          break

ants.run new Bot()
