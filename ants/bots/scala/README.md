Scala Starter Pack for Ants
===========================

by [@badgerhunt][1]

How to build
------------

1. If necessary, install [SBT][2]
2. In this directory, execute `sbt update test package`.


How to implement your bot
-------------------------

Create a class (without any package) that extends `Bot`. There is a single method to implement.
    def ordersFrom(gameState: Game): Set[Order]

You also need to create a main class for your Bot. For example:

    object HunterBot extends Application {
      new AntsGame().run(new HunterBot)
    }

Your bot can use the information contained in `gameState` to derive a set of orders. Your bot may maintain state
between calls.

Feel free to modify the starter pack code if necessary. It's all yours.

An example bot (`HunterBot`) is included with the starter pack.




[1]: http://twitter.com/badgerhunt
[2]: http://code.google.com/p/simple-build-tool/        "Simple Build Tool - A build tool for Scala"