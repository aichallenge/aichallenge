<?php

$title="Strategy Guide";
require_once('header.php');

?>

<!--<MarkdownReplacement with="competition-Strategy-Guide.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="strategy-guide">Strategy Guide</h2>
<p>This page is intended to give you a few ideas for climbing the ranks in the leaderboard. In order to minimize the size of examples, it is assumed that the edges are not wrapped for any of the examples.</p>
<h2 id="pathfinding">Pathfinding</h2>
<p>A frequent problem in Ants is going to be finding the shortest path from one your ants to potential objective squares. For example, you might have the following situation and want to find the food square that can be reached fastest:</p>
<div class="codehilite"><pre><span></span>.......
.*..a%*
.......
</pre></div>


<p>The easiest way to do this is with a <a href="http://en.wikipedia.org/wiki/Breadth-first_search">breadth-first search</a> from the ants location, noting the distance to a square as you mark it as visited. The above example would result in the following information:</p>
<div class="codehilite"><pre><span></span>5432123
6321a%4
5432123
</pre></div>


<p>Comparing the two food locations, we see that the left food square can be reached in 3 moves while the right food square can be reached in 4 moves, despite the fact that the distance to the right food square is shorter.</p>
<p>Taking any objective square, you can then use the numbers to trace back a shortest path from the ants starting location. </p>
<p>If you know your objective square for an ant before you start searching, you will find that <a href="http://en.wikipedia.org/wiki/A*_search_algorithm">A* search</a> works quite well.</p>
<p>You can use a breadth-first search for many other things as well, such as simultaneously searching from all ant locations to partition the map into which squares each bot can reach first and finding what squares are visible to a bot.</p>
<h2 id="collecting-food-growing-your-army">Collecting Food / Growing Your Army</h2>
<p>A naive way of collecting food would be to perform a breadth-first search from each food square to find its closest ant and moving that ant towards it. </p>
<p>If you wanted to move ants towards the closest food square first, you could iteratively perform these breadth-first searches from all non-covered food squares simultaneously using the one queue, moving an ant towards the appropriate food square as you find one.</p>
<p>Below are some example cases you might want to consider where the above two methods would not work optimally:</p>
<div class="codehilite"><pre><span></span>b...*..a.*

...bb...
........
........
**..a.*.
</pre></div>
<!--</MarkdownReplacement>-->

<?php

require_once('visualizer_widget.php');
visualize_pre();
require_once('footer.php');

?>