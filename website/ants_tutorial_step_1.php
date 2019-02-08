<?php

$title="Ants Tutorial Step 1";
require_once('header.php');

?>

<!--<MarkdownReplacement with="competition-Tutorial-Step-1.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="step-1-avoiding-collisions">Step 1: Avoiding collisions</h2>
<div class="toc">

*   <a href="ants_tutorial.php">Setting Up</a>
*   <a href="ants_tutorial_step_1.php">Step 1: Avoiding Collisions</a>
*   <a href="ants_tutorial_step_2.php">Step 2: Gathering Food</a>
*   <a href="ants_tutorial_step_3.php">Step 3: Not Blocking Hills</a>
*   <a href="ants_tutorial_step_4.php">Step 4: Explore the Map</a>
*   <a href="ants_tutorial_step_5.php">Step 5: Attack the Enemy Hills</a>

</div>

<h3 id="the-plan">The Plan</h3>
<p>In order to prevent collisions, we will need to do a few things:</p>
<ul>
<li>prevent ants from moving onto other ants</li>
<li>prevent 2 ants from moving to the same destination</li>
<li>track information about where all our ants are going</li>
</ul>
<h3 id="the-implementation">The Implementation</h3>
<div class="tab_sync">
<div class="tab_content" title="Python">

To track information about where ants are moving, we are going to use a dictionary.  It is a data structure that will store locations, and then allow us to check if a location has already been stored.  Each key of the dictionary will be a location we are moving to and each value will be the ant location that is moving to the new location.  A location will be a tuple of values consisting of the row and column of the map location.  We can then check the dictionary before making a move to ensure we don't move 2 ants to the same spot.  Every time we move an ant, we need to be sure to update the list.

This check will come in handy later in the tutorial, so we will make a function to attempt moves and check to make sure the move is to an empty location.  It will return a boolean (true or false) to let us know if the move worked.

</div>
<div class="tab_content" title="Java">

To track information about where ants are moving, we are going to use a HashMap.  It is a data structure that will store locations, and then allow us to check if a location has already been stored.  Each key and value of the HashMap will be a Tile object.  A Tile object is the row and column of a location on the map.  The key will be the new location to move to and the value will be the location of the ant moving to the new location.  We can then check the HashMap before making a move to ensure we don't move 2 ants to the same spot.  Every time we move an ant, we need to be sure to update the HashMap.

This check will come in handy later in the tutorial, so we will make a function to attempt moves and check to make sure the move is to an empty location.  It will return a boolean (true or false) to let us know if the move worked.

</div>
</div>

<h3 id="the-code">The Code</h3>
<p>We'll trim down the starter bots comments and put the new code in:</p>
<div class="tab_sync">
<div class="tab_content" title="Python">

    :::python
        def do_turn(self, ants):
            # track all moves, prevent collisions
            orders = {}
            def do_move_direction(loc, direction):
                new_loc = ants.destination(loc, direction)
                if (ants.unoccupied(new_loc) and new_loc not in orders):
                    ants.issue_order((loc, direction))
                    orders[new_loc] = loc
                    return True
                else:
                    return False

            # default move
            for ant_loc in ants.my_ants():
                directions = ('n','e','s','w')
                for direction in directions:
                    if do_move_direction(ant_loc, direction):
                        break


*(Note: Make sure you get the indentation correct.  In Python, indentation determines the code blocks or scope, so it has to be correct.)*

The `do_move_direction` takes an ant location (a tuple of (row, col) ) and a direction ( 'n', 'e', 's' or 'w' ) and tries to perform the move.  This function is located inside a class method (which is also a function) and is okay to do in python.  We are using some predefined functions from the starter bot to help us:

* `ants.destination` takes a location and a direction and returns the destination location for us.  It takes care of the map wrapping around so we don't have to worry about it.

* `ants.unoccupied` takes a location and let's us know if we can move there.  This is better than the previous `ants.passable` because it will not allow us to step on food or other ants.

The `orders` dictionary is used to track what moves we have issued.  In the if statement we have `new_loc not in orders` which will check the dictionary keys for us and help prevent collisions.
</div>
<div class="tab_content" title="Java">

We'll trim down the starter bots comments and put the new code in:


    :::java
        private Map<Tile, Tile> orders = new HashMap<Tile, Tile>();

        private boolean doMoveDirection(Tile antLoc, Aim direction) {
            Ants ants = getAnts();
            // Track all moves, prevent collisions
            Tile newLoc = ants.getTile(antLoc, direction);
            if (ants.getIlk(newLoc).isUnoccupied() && !orders.containsKey(newLoc)) {
                ants.issueOrder(antLoc, direction);
                orders.put(newLoc, antLoc);
                return true;
            } else {
                return false;
            }
        }

        @Override
        public void doTurn() {
            Ants ants = getAnts();
            orders.clear();

            //  default move
            for (Tile myAnt : ants.getMyAnts()) {
                for (Aim direction : Aim.values()) {
                    if (doMoveDirection(myAnt, direction)) {
                        break;
                    }
                }
            }
        }


The `doMoveDirection` function takes an ant location (a Tile object) and a direction (an Aim object of N, E, S or W) and tries to perform the move.  This function is located outside the `doTurn` function, so our reserved tiles HashMap is at the class level and we clear it for each turn.  We are using some predefined functions from the starter bot to help us:

* `ants.getTile(Tile, Aim)` takes a location (Tile object) and a direction (Aim object) and returns the destination location (Tile object) for us.  It takes care of the map wrapping around so we don't have to worry about it.

* `ants.getIlk(Tile)` takes a location (Tile object) and returns the Ilk (a fancy word for type or kind).  We then call the `isUnoccupied()` function of the Ilk object to see if it is free to move to.

* `Ilk.isUnoccupied` takes a location and let's us know if we can move there.  This is better than the previous `Ilk.isPassable` because it will not allow us to step on food or other ants.

The `orders` HashMap is used to track what moves we have issued.  In the if statement we have `!orders.containsKey(newLoc)` which will check the HashMap for us and help prevent collisions.
</div>
</div>

<h3 id="the-results">The Results</h3>
<p>Let's run the bot again and see how we do.</p>
<div class="codehilite"><pre><span></span><span class="nt">C</span><span class="o">:</span><span class="err">\</span><span class="nt">aichallenge</span><span class="o">&amp;</span><span class="nt">gt</span><span class="o">;</span><span class="nt">tutorial</span><span class="p">.</span><span class="nc">cmd</span>
<span class="nt">running</span> <span class="nt">for</span> <span class="nt">60</span> <span class="nt">turns</span>
                  <span class="nt">ant_count</span> <span class="nt">c_turns</span> <span class="nt">climb</span><span class="o">?</span> <span class="nt">cutoff</span> <span class="nt">food</span> <span class="nt">r_turn</span> <span class="nt">ranking_bots</span> <span class="nt">s_alive</span> <span class="nt">s_hills</span> <span class="nt">score</span>  <span class="nt">w_turn</span> <span class="nt">winning</span>
<span class="nt">turn</span>    <span class="nt">0</span> <span class="nt">stats</span><span class="o">:</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>     <span class="nt">0</span>    <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>    <span class="nt">-</span>     <span class="nt">18</span>    <span class="nt">0</span>        <span class="nt">None</span>      <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>  <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">0</span>     <span class="nt">None</span>
<span class="nt">turn</span>    <span class="nt">1</span> <span class="nt">stats</span><span class="o">:</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>     <span class="nt">0</span>    <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>    <span class="nt">-</span>     <span class="nt">16</span>    <span class="nt">1</span>       <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>      <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>  <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">1</span>     <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>
<span class="nt">turn</span>    <span class="nt">2</span> <span class="nt">stats</span><span class="o">:</span>   <span class="cp">[</span><span class="mi">2</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>     <span class="nt">0</span>    <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>    <span class="nt">-</span>     <span class="nt">16</span>    <span class="nt">1</span>       <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>      <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>  <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">1</span>     <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>
<span class="o">...</span>
<span class="nt">turn</span>   <span class="nt">60</span> <span class="nt">stats</span><span class="o">:</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">5</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>     <span class="nt">0</span>    <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>    <span class="nt">-</span>     <span class="nt">12</span>    <span class="nt">1</span>       <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>      <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>  <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">1</span>     <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>
<span class="nt">score</span> <span class="nt">1</span> <span class="nt">1</span>
<span class="nt">status</span> <span class="nt">survived</span> <span class="nt">survived</span>
<span class="nt">playerturns</span> <span class="nt">60</span> <span class="nt">60</span>
</pre></div>


<p>Here is the replay:</p>
<div class="codehilite"><pre><span></span># [ { &quot;embedded&quot;: true, &quot;game&quot;: &quot;1 - Avoiding collisions&quot; }, 600, 600, { &quot;speedFactor&quot;: 0, &quot;speedFastest&quot;: 2, &quot;speedSlowest&quot;: 2, &quot;zoom&quot;: 1 }, &quot;example_games/tutorial.1.replay&quot; ]
</pre></div>


<p>Better, but still not good.  One lone ant got out and got to fight with HunterBot.  We didn't suicide, and that's an improvement.  Plus, we created a helper function that will come in handy later.</p>
<p>If your bot's ants oscillated behind their barrier instead, it is probably due to the ordering of the ants in your loop.  If the NW ant moves first it moves to the North of the SE ant, which can then only move East, South or West.  Otherwise if the SE ant moves first it moves to the East of the NW ant, which can then only move South or West.</p>
<h3 id="next">Next</h3>
<p>On to <a href="ants_tutorial_step_2.php">Step 2: Gathering Food</a></p>
<!--</MarkdownReplacement>-->

<?php

require_once('visualizer_widget.php');
visualize_pre();
require_once('footer.php');

?>