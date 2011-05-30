game = require('./ants').Game
ants = new game()

directions = ['N', 'E', 'S', 'W']

class Bot
  ready: -> ants.finish_turn()

  do_turn: -> 
    my_ants = ants.my_ants()
    for ant in my_ants
      for dir in directions
        near_square = ants.neighbor(ant.x, ant.y, dir)
        if ants.passable(near_square.x, near_square.y)
          ants.issue_order(ant.x, ant.y, dir)
          break
    ants.finish_turn()

  end: -> 

ants.run new Bot()
