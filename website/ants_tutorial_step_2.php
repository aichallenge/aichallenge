<?php

$title="Ants Tutorial Step 2";
require_once('header.php');

?>

<!--<MarkdownReplacement with="competition-Tutorial-Step-2.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="step-2-gathering-food">Step 2: Gathering Food</h2>
<div class="toc">

*   <a href="ants_tutorial.php">Setting Up</a>
*   <a href="ants_tutorial_step_1.php">Step 1: Avoiding Collisions</a>
*   <a href="ants_tutorial_step_2.php">Step 2: Gathering Food</a>
*   <a href="ants_tutorial_step_3.php">Step 3: Not Blocking Hills</a>
*   <a href="ants_tutorial_step_4.php">Step 4: Explore the Map</a>
*   <a href="ants_tutorial_step_5.php">Step 5: Attack the Enemy Hills</a>

</div>

<h3 id="the-plan">The Plan</h3>
<p>We need more than 2 ants to win this game and there's food right next to the starting ant!  Let's try and gather it.  We'll need to move an ant right next to a food item to gather it.  We also want to be smart about it.  Did you notice that HunterBot sent all his ants after one food item in the last game?  That seems like it could be inefficient.</p>
<p>We are going to implement something similar to a priority queue.  We'll make a list of every ant we have, and then see how far it is from every food item.  We'll then sort the list and send each ant after the closest food, but only one ant per food item.  The other ants will be free to do other important things.  We'll also get rid of the stupid default move that came with the starter bot.</p>
<h3 id="the-implementation">The Implementation</h3>
<div class="tab_sync">
<div class="tab_content" title="Python">

To track information about which food item is already being gathered by an ant, we'll need another dictionary.  It will store the location of the target food as the key, and the location of the ant that is gathering the food as the value.  We can then check the target keys to make sure we don't send two ants to the same food.  We will create another helper function to make a slightly different type of move.  Instead of an ant location and a direction, we will give it an ant location and a target location, and the function will figure out the direction for us.

</div>
<div class="tab_content" title="Java">

To track information about which food item is already being gathered by an ant, we'll need another data structure to store location information.  We'll use another HashMap, so we can store the location of the target food as the key, and the location of the ant that is gathering the food as the value.  We can then check the target keys to make sure we don't send two ants to the same food.  We will create another helper function to make a slightly different type of move.  Instead of an ant location and a direction, we will give it an ant location and a target location, and the function will figure out the direction for us.  The new Route class will store a start and end location, so we can put pair into other data structures.  The ArrayList data structure will help us sort the list of unique routes by distance.

</div>
</div>

<h3 id="the-code">The Code</h3>
<div class="tab_sync">
<div class="tab_content" title="Python">

Create the following function after the `do_move_direction` function:


    :::python
            targets = {}
            def do_move_location(loc, dest):
                directions = ants.direction(loc, dest)
                for direction in directions:
                    if do_move_direction(loc, direction):
                        targets[dest] = loc
                        return True
                return False


Make sure this function has the same indentation as the `do_move_direction` function.  The `targets` dictionary tracks our food targets and ants.  We are using another starter bot function to help us:

* `ants.direction` takes a location and a destination and returns a list of the closest direction "as the crow flies".  If the target is up and to the left, it will return `['n', 'w']` and we should then try and move our ant one of the two directions.  If the target is directly down, it will return `['s']`, which is a list of one item.

Now replace the default move with this:


    :::python
            # find close food
            ant_dist = []
            for food_loc in ants.food():
                for ant_loc in ants.my_ants():
                    dist = ants.distance(ant_loc, food_loc)
                    ant_dist.append((dist, ant_loc, food_loc))
            ant_dist.sort()
            for dist, ant_loc, food_loc in ant_dist:
                if food_loc not in targets and ant_loc not in targets.values():
                    do_move_location(ant_loc, food_loc)


Here we have a list, `ant_dist`, which will store every ant to food combination and the distance as a tuple of `(dist, ant_loc, food_loc)`.  The list is built by a nested loop structure to give us every combination.  Next, we sort the list.  Python lists come with some handy functions to do the sorting for us.  To order a tuple, python will compare the first values of each tuple first, then if they are the same, move on to the second value and so forth.  This is why we stored the distance as the first value, to make sure the shortest distances are first in the list.

Next we loop through the sorted list and check to see if we have any free ants that can gather food.  The `food_loc not in targets` check to see if a food item already has an ant gathering it.  The `ant_loc not in targets.values()` checks to make sure the ant hasn't already been given a task.  If an ant is found, we call `do_move_location` and all the direction and collision stuff is already taken care of for us.

</div>
<div class="tab_content" title="Java">

Create the following class "Route" in a new file called "Route.java":


    :::java
    /**
     * Represents a route from one tile to another.
     */
    public class Route implements Comparable<Route> {
        private final Tile start;

        private final Tile end;

        private final int distance;

        public Route(Tile start, Tile end, int distance) {
            this.start = start;
            this.end = end;
            this.distance = distance;
        }

        public Tile getStart() {
            return start;
        }

        public Tile getEnd() {
            return end;
        }

        public int getDistance() {
            return distance;
        }

        @Override
        public int compareTo(Route route) {
            return distance - route.distance;
        }

        @Override
        public int hashCode() {
            return start.hashCode() * Ants.MAX_MAP_SIZE * Ants.MAX_MAP_SIZE + end.hashCode();
        }

        @Override
        public boolean equals(Object o) {
            boolean result = false;
            if (o instanceof Route) {
                Route route = (Route)o;
                result = start.equals(route.start) && end.equals(route.end);
            }
            return result;
        }
    }


This is a basic class the implements the idea of a tuple or pair.  We add some getter functions (`getStart`, `getEnd`) and some function to make sure it behaves nicely for sorting and using inside other data structures (`compareTo`, `hashCode`, `equals`).

Add the following new function to the MyBot.java file after the `doMoveDirection` function:


    :::java
        private boolean doMoveLocation(Tile antLoc, Tile destLoc) {
            Ants ants = getAnts();
            // Track targets to prevent 2 ants to the same location
            List<Aim> directions = ants.getDirections(antLoc, destLoc);
            for (Aim direction : directions) {
                if (doMoveDirection(antLoc, direction)) {
                    return true;
                }
            }
            return false;
        }


This function will take an ant and a target location, then attempt to do the move.  It is using the `doMoveDirection` from the last step, so it will already make sure we don't step on our own ants.

Replace the default move with the following code:


    :::java
        @Override
        public void doTurn() {
            Ants ants = getAnts();
            orders.clear();
            Map<Tile, Tile> foodTargets = new HashMap<Tile, Tile>();

            // find close food
            List<Route> foodRoutes = new ArrayList<Route>();
            TreeSet<Tile> sortedFood = new TreeSet<Tile>(ants.getFoodTiles());
            TreeSet<Tile> sortedAnts = new TreeSet<Tile>(ants.getMyAnts());
            for (Tile foodLoc : sortedFood) {
                for (Tile antLoc : sortedAnts) {
                    int distance = ants.getDistance(antLoc, foodLoc);
                    Route route = new Route(antLoc, foodLoc, distance);
                    foodRoutes.add(route);
                }
            }
            Collections.sort(foodRoutes);
            for (Route route : foodRoutes) {
                if (!foodTargets.containsKey(route.getEnd())
                        && !foodTargets.containsValue(route.getStart())
                        && doMoveLocation(route.getStart(), route.getEnd())) {
                    foodTargets.put(route.getEnd(), route.getStart());
                }
            }


Here we build a list of every ant to food combination and store the distance.  Then we sort the ArrayList so we get the shortest distances first when looping through the routes.  Next we loop through all possible combinations and if the ant has not been ordered and the food has not been targeted yet, we issue a new order.  We also save a list of target locations to make sure only 1 ant is going for a food item.

</div>
</div>

<h3 id="the-results">The Results</h3>
<p>Let's run the bot again and see how we do.</p>
<div class="codehilite"><pre><span></span><span class="nt">C</span><span class="o">:</span><span class="err">\</span><span class="nt">aichallenge</span><span class="o">&amp;</span><span class="nt">gt</span><span class="o">;</span><span class="nt">tutorial</span><span class="p">.</span><span class="nc">cmd</span>
<span class="nt">running</span> <span class="nt">for</span> <span class="nt">60</span> <span class="nt">turns</span>
                  <span class="nt">ant_count</span> <span class="nt">c_turns</span> <span class="nt">climb</span><span class="o">?</span> <span class="nt">cutoff</span> <span class="nt">food</span> <span class="nt">r_turn</span> <span class="nt">ranking_bots</span> <span class="nt">s_alive</span> <span class="nt">s_hills</span> <span class="nt">score</span>  <span class="nt">w_turn</span> <span class="nt">winning</span>
<span class="nt">turn</span>    <span class="nt">0</span> <span class="nt">stats</span><span class="o">:</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>     <span class="nt">0</span>    <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>    <span class="nt">-</span>     <span class="nt">18</span>    <span class="nt">0</span>        <span class="nt">None</span>      <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>  <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">0</span>     <span class="nt">None</span>
<span class="nt">turn</span>    <span class="nt">1</span> <span class="nt">stats</span><span class="o">:</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>     <span class="nt">0</span>    <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>    <span class="nt">-</span>     <span class="nt">16</span>    <span class="nt">1</span>       <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>      <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>  <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">1</span>     <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>
<span class="nt">turn</span>    <span class="nt">2</span> <span class="nt">stats</span><span class="o">:</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>     <span class="nt">0</span>    <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>    <span class="nt">-</span>     <span class="nt">16</span>    <span class="nt">1</span>       <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>      <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>  <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">1</span>     <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>
<span class="o">...</span>
<span class="nt">turn</span>   <span class="nt">60</span> <span class="nt">stats</span><span class="o">:</span>   <span class="cp">[</span><span class="mi">4</span><span class="p">,</span><span class="mi">6</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>     <span class="nt">0</span>    <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>    <span class="nt">-</span>     <span class="nt">6</span>     <span class="nt">1</span>       <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>      <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>  <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">1</span>     <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>
<span class="nt">score</span> <span class="nt">1</span> <span class="nt">1</span>
<span class="nt">status</span> <span class="nt">survived</span> <span class="nt">survived</span>
<span class="nt">playerturns</span> <span class="nt">60</span> <span class="nt">60</span>
</pre></div>


<p>Here is the replay:</p>
<div class="codehilite"><pre><span></span># [ { &quot;embedded&quot;: true, &quot;game&quot;: &quot;2 - Gathering Food&quot; }, 600, 600, { &quot;speedFactor&quot;: 0, &quot;speedFastest&quot;: 2, &quot;speedSlowest&quot;: 2, &quot;zoom&quot;: 1 }, &quot;example_games/tutorial.2.replay&quot; ]
</pre></div>


<p>Hey, we did pretty good!  All the food that we could see was picked up.  If you look closely at the replay, you can see we still have 3 ants in the hive that can't spawn.  Oops, we better take care of that.  If ants can't get out, they can't help us win.</p>
<h3 id="next">Next</h3>
<p>On to <a href="ants_tutorial_step_3.php">Step 3: Not Blocking Hills</a></p>
<!--</MarkdownReplacement>-->

<?php

require_once('visualizer_widget.php');
visualize_pre();
require_once('footer.php');

?>