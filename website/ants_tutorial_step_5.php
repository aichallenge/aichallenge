<?php

$title="Ants Tutorial Step 5";
require_once('header.php');

?>

<!--<MarkdownReplacement with="competition-Tutorial-Step-5.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="step-5-attacking-the-enemy-hills">Step 5: Attacking the Enemy Hills</h2>
<div class="toc">

*   <a href="ants_tutorial.php">Setting Up</a>
*   <a href="ants_tutorial_step_1.php">Step 1: Avoiding Collisions</a>
*   <a href="ants_tutorial_step_2.php">Step 2: Gathering Food</a>
*   <a href="ants_tutorial_step_3.php">Step 3: Not Blocking Hills</a>
*   <a href="ants_tutorial_step_4.php">Step 4: Explore the Map</a>
*   <a href="ants_tutorial_step_5.php">Step 5: Attack the Enemy Hills</a>

</div>

<h3 id="the-plan">The Plan</h3>
<p>If we see a hill, we need to do something about it.  Hills are very important, it's the only way you can get points to win the game (besides killing all the other ants and hoping no one else got the hills first.)</p>
<ul>
<li>If we see a hill, send all available ants to attack it.</li>
</ul>
<h3 id="the-implementation">The Implementation</h3>
<p>If you see a hill, chances are you have sent a scouting ant to the enemy base, and chances are he's going to die, because that's where the enemy ants spawn.  This means if you lose vision of the hill, you don't want to forget that it's there.  We'll create another class level variable to help us remember where it is.  If we see more than one hill, each ant will attack the closest one, just like the food gathering code.</p>
<h3 id="the-code">The Code</h3>
<div class="tab_sync">
<div class="tab_content" title="Python">

Put this code in the `do_setup` method:


    :::python
        def do_setup(self, ants):
            self.hills = []


This will be the list of all hills we have found.

Put the following code after the food gathering section and before the map exploration section.


    :::python
            # attack hills
            for hill_loc, hill_owner in ants.enemy_hills():
                if hill_loc not in self.hills:
                    self.hills.append(hill_loc)        
            ant_dist = []
            for hill_loc in self.hills:
                for ant_loc in ants.my_ants():
                    if ant_loc not in orders.values():
                        dist = ants.distance(ant_loc, hill_loc)
                        ant_dist.append((dist, ant_loc, hill_loc))
            ant_dist.sort()
            for dist, ant_loc, hill_loc in ant_dist:
                do_move_location(ant_loc, hill_loc)


In the first part, we loop through all enemy hills, and if we haven't seen it before, we add it to the list.

In the second part, we create a list of all distances from ants to hills, just like we did for gathering food.

Last, we sort the list by closest distance first, then send each ant in to attack.  We aren't checking to see if a target was already chosen, because we want to send in all ants to attack.

</div>
<div class="tab_content" title="Java">

Put this class level variable declaration with the others:


    :::java
        private Set<Tile> enemyHills = new HashSet<Tile>();


This will be the list of all enemy hills we have found.

Add the following code below the food gathering code, and above the exploring code:


    :::java
            // add new hills to set
            for (Tile enemyHill : ants.getEnemyHills()) {
                if (!enemyHills.contains(enemyHill)) {
                    enemyHills.add(enemyHill);
                }
            }
            // attack hills
            List<Route> hillRoutes = new ArrayList<Route>();
            for (Tile hillLoc : enemyHills) {
                for (Tile antLoc : sortedAnts) {
                    if (!orders.containsValue(antLoc)) {
                        int distance = ants.getDistance(antLoc, hillLoc);
                        Route route = new Route(antLoc, hillLoc, distance);
                        hillRoutes.add(route);
                    }
                }
            }
            Collections.sort(hillRoutes);
            for (Route route : hillRoutes) {
                doMoveLocation(route.getStart(), route.getEnd());
            }


In the first part, we loop through all enemy hills, and if we haven't seen it before, we add it to the list.

In the second part, we create a list of all distances from ants to hills, just like we did for gathering food.

Last, we loop through the sorted list by closest distance first, then send each ant in to attack.  We aren't checking to see if a target was already chosen, because we want to send in all ants to attack.  *(Note: we never remove an enemy hill once we have added it. )*

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
<span class="nt">turn</span>   <span class="nt">43</span> <span class="nt">stats</span><span class="o">:</span>   <span class="cp">[</span><span class="mi">7</span><span class="p">,</span><span class="mi">3</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>     <span class="nt">0</span>    <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>    <span class="nt">-</span>     <span class="nt">2</span>     <span class="nt">43</span>      <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>      <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>  <span class="cp">[</span><span class="mi">3</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>   <span class="nt">43</span>     <span class="cp">[</span><span class="mi">0</span><span class="cp">]</span>
<span class="nt">score</span> <span class="nt">3</span> <span class="nt">0</span>
<span class="nt">status</span> <span class="nt">survived</span> <span class="nt">survived</span>
<span class="nt">playerturns</span> <span class="nt">43</span> <span class="nt">43</span>
</pre></div>


<p>Here is the replay:</p>
<div class="codehilite"><pre><span></span># [ { &quot;embedded&quot;: true, &quot;game&quot;: &quot;5 - Attacking Enemy Hills&quot; }, 600, 600, { &quot;speedFactor&quot;: 0, &quot;speedFastest&quot;: 2, &quot;speedSlowest&quot;: 2, &quot;zoom&quot;: 1 }, &quot;example_games/tutorial.5.replay&quot; ]
</pre></div>


<p>There we go; A successful kill!  This concludes the tutorial.</p>
<p>There's probably a lot more that can be done.  Here are a few ideas:</p>
<ul>
<li>The <code>ants.distance</code> and <code>ants.direction</code> functions work using a manhatten distance, which means they assume you can walk on water and they will get you stuck in corners.  Our tutorial map was very open, so it didn't hurt us much.  You'll need to implement some path finding and replace these functions to give more accurate results and help you find your way out of a maze.</li>
<li>We didn't talk about fighting other ants.  The attack rules are such that you can kill an enemy ant without taking losses.  Maybe some logic to find an enemy, send some reinforcements and then move in for the kill would help.</li>
<li>Food will constantly spawn during the game, and it could spawn anywhere.  Keeping your ants spread out to cover the whole map will help you spot new food and gather before the opponent does.  This means our exploring code only works once, not continuously.</li>
<li>You'll lose a point if you don't defend your hill.  It may be good to keep a couple of ants around just in case you get attacked.</li>
<li>Deciding whether to attack a hill or gather food first may change during the game.  You may want to attack a hill first if you have plenty of ants and are trying to be faster than the next closest enemy.  You may want to gather more ants if you are trying to defend you hill.</li>
<li>The data structures using in the tutorial are just to get you started, but may not be the best choice.  Try and balance memory usage with CPU performance to make the best bot.</li>
<li>Watch the games of the top bots, and try figure out what they do.  Then implement those ideas.</li>
</ul>
<h3 id="next">Next</h3>
<ul>
<li>Check out the <a href="http://forums.aichallenge.org/">forums</a> or <a href="irc://irc.freenode.org/aichallenge">IRC</a> <a href="http://webchat.freenode.net/?channels=aichallenge&uio=d4">(webclient)</a> to discuss more ideas with the other contestants</li>
</ul>
<!--</MarkdownReplacement>-->

<?php

require_once('visualizer_widget.php');
visualize_pre();
require_once('footer.php');

?>