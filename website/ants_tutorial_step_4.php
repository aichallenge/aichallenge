<?php

$title="Ants Tutorial Step 4";
require_once('header.php');

?>

<!--<MarkdownReplacement with="competition-Tutorial-Step-4.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="step-4-exploring-the-map">Step 4: Exploring the Map</h2>
<div class="toc">

*   <a href="ants_tutorial.php">Setting Up</a>
*   <a href="ants_tutorial_step_1.php">Step 1: Avoiding Collisions</a>
*   <a href="ants_tutorial_step_2.php">Step 2: Gathering Food</a>
*   <a href="ants_tutorial_step_3.php">Step 3: Not Blocking Hills</a>
*   <a href="ants_tutorial_step_4.php">Step 4: Explore the Map</a>
*   <a href="ants_tutorial_step_5.php">Step 5: Attack the Enemy Hills</a>

</div>

<h3 id="the-plan">The Plan</h3>
<p>If ants can't see any food, they just sit there doing nothing.  HunterBot does this too.  We need a way to get our ants to look for more food.  If we can track all of the locations we haven't seen yet, then we can send free ants to go scout that location.  If they find more food, then the food gathering code should kick in.</p>
<h3 id="the-implementation">The Implementation</h3>
<p>To track information about what hasn't been seen, we are going to create a class level variable.  This will be a list of every location on the map.  Each turn, if we can see the location, then we will remove it from the list.  We can then start sending free ants to the location left in the list.  Once the entire map is explored, the list will be empty and this section of code will be ignored.  Food is more important to get right away, so we'll do this as a second priority.</p>
<h3 id="the-code">The Code</h3>
<div class="tab_sync">
<div class="tab_content" title="Python">

Put this code in the `do_setup` method of the bot.  You should replace the `pass` statement.  This code will only be run once after our bot learns the size of the map.  


    :::python
        self.unseen = []
        for row in range(ants.rows):
            for col in range(ants.cols):
                self.unseen.append((row, col))


Notice we are using `self.unseen`; `self` is a reference to our bot class and we will need to use it to reference our unseen list in the `do_turn` method.  We create a list with a nested loop for every combination of row and column values.  *(Note: this could be a large list and not very memory efficient.  This is just the easiest way to make the code look nice.  You'll probably want to try and use a different technique for a real bot.)*

Add the following code after the gather food section and before the unblocking hill section:


    :::python
        # explore unseen areas
        for loc in self.unseen[:]:
            if ants.visible(loc):
                self.unseen.remove(loc)
        for ant_loc in ants.my_ants():
            if ant_loc not in orders.values():
                unseen_dist = []
                for unseen_loc in self.unseen:
                    dist = ants.distance(ant_loc, unseen_loc)
                    unseen_dist.append((dist, unseen_loc))
                unseen_dist.sort()
                for dist, unseen_loc in unseen_dist:
                    if do_move_location(ant_loc, unseen_loc):
                        break


First we trim the list of unseen squares by looping through every one and checking if we can see it.  We use yet another starter bot helper function:

* `ants.visible` takes a location and returns a `True` if it is in the view radius of any ant.  This function is written to be fairly efficient so that calling it multiple times won't cause a big slowdown.  (It can still be improved.)  You shouldn't try and modify a list while you are looping through it, so we use the list copy shortcut `[:]` in the for loop to make sure the list we are looping through is different than the list we are removing locations from.

Next we loop through all the ants and make sure they haven't been given an order yet.  If not, we then create a list of all the squares we haven't seen yet and order them by distance.  This is the same technique we used for the gather food code.  We then go through the list and find the first unseen square that we can start moving toward.  *(Note: at the beginning of a game on a large map, checking the distance to every unseen square is probably very slow.  This could be done better.)*

</div>
<div class="tab_content" title="Java">

Add the `unseenTiles` Set under the order declaration:


    :::java
        private Map<Tile, Tile> orders = new HashMap<Tile, Tile>();

        private Set<Tile> unseenTiles;


The unseenTiles will be a set of all tiles we have not seen during the game.  *(Note: this could be a large list and not very memory efficient.  This is just the easiest way to make the code look nice.  You'll probably want to try and use a different technique for a real bot.)*

Next, add the following code near the top of the `doTurn` function, just below the foodTargets declaration:


    :::java
            // add all locations to unseen tiles set, run once
            if (unseenTiles == null) {
                unseenTiles = new HashSet<Tile>();
                for (int row = 0; row < ants.getRows(); row++) {
                    for (int col = 0; col < ants.getCols(); col++) {
                        unseenTiles.add(new Tile(row, col));
                    }
                }
            }
            // remove any tiles that can be seen, run each turn
            for (Iterator<Tile> locIter = unseenTiles.iterator(); locIter.hasNext(); ) {
                Tile next = locIter.next();
                if (ants.isVisible(next)) {
                    locIter.remove();
                }
            }


The first part initializes the list to every Tile in the game.  It will only run during the first turn.  You shouldn't try and modify a collection while you are looping through it, so we use an iterator object, which allows for safe removal of Tiles while we loop through the list and check the visibility.

Last, add this code section between finding close food and unblocking your own hills:


    :::java
            // explore unseen areas
            for (Tile antLoc : sortedAnts) {
                if (!orders.containsValue(antLoc)) {
                    List<Route> unseenRoutes = new ArrayList<Route>();
                    for (Tile unseenLoc : unseenTiles) {
                        int distance = ants.getDistance(antLoc, unseenLoc);
                        Route route = new Route(antLoc, unseenLoc, distance);
                        unseenRoutes.add(route);
                    }
                    Collections.sort(unseenRoutes);
                    for (Route route : unseenRoutes) {
                        if (doMoveLocation(route.getStart(), route.getEnd())) {
                            break;
                        }
                    }
                }
            }


Here, for every ant that doesn't have an order yet (we are checking the orders HashMap using the `containsValue()` method), we calculate the distance to every other unseen location.  Then we sort the ArrayList so the shortest distances are first.  We are using another help function from the starter bot.

* `ants.isVisible(Tile)` takes a location and returns a `true` if it is in the view radius of any ant.  This function is written to be fairly efficient so that calling it multiple times won't cause a big slowdown.  (It can still be improved.)

This is the same technique we used for the gather food code.  We then go through the list and find the first and closest unseen square that we can start moving toward.  *(Note: at the beginning of a game on a large map, checking the distance to every unseen square is probably very slow.  This could be done better.)*

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
<span class="nt">turn</span>   <span class="nt">60</span> <span class="nt">stats</span><span class="o">:</span>   <span class="cp">[</span><span class="mi">9</span><span class="p">,</span><span class="mi">2</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>     <span class="nt">0</span>    <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>    <span class="nt">-</span>     <span class="nt">0</span>     <span class="nt">1</span>       <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>      <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>  <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">1</span>     <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>
<span class="nt">score</span> <span class="nt">1</span> <span class="nt">1</span>
<span class="nt">status</span> <span class="nt">survived</span> <span class="nt">survived</span>
<span class="nt">playerturns</span> <span class="nt">60</span> <span class="nt">60</span>
</pre></div>


<p>Here is the replay:</p>
<div class="codehilite"><pre><span></span># [ { &quot;embedded&quot;: true, &quot;game&quot;: &quot;4 - Exploring the Map&quot; }, 600, 600, { &quot;speedFactor&quot;: 0, &quot;speedFastest&quot;: 2, &quot;speedSlowest&quot;: 2, &quot;zoom&quot;: 1 }, &quot;example_games/tutorial.4.replay&quot; ]
</pre></div>


<p>Look at that, we got all the food!  Our ants are now roaming around the map so that they can see everything.  Unfortunately we haven't taken out the enemy hill yet, so let's work on that next.</p>
<h3 id="next">Next</h3>
<p>On to <a href="ants_tutorial_step_5.php">Step 5: Attacking Enemy Hills</a></p>
<!--</MarkdownReplacement>-->

<?php

require_once('visualizer_widget.php');
visualize_pre();
require_once('footer.php');

?>