<?php

$title="Getting Started with Python";
require_once('header.php');

?>

<!--<MarkdownReplacement with="competition-Getting-Started-with-Python.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="getting-started-with-python">Getting Started with Python</h2>
<ul>
<li><a href="http://www.python.org/">Python Official Home Page</a></li>
<li><a href="http://docs.python.org/using/index.html">Python Setup and Usage</a></li>
</ul>
<p>Python is a high level programming language.  It uses indentation for block delimiters.  Python has two currently active versions: 2.7.x and 3.2.x.  Version 3 is not backwards compatible with version 2.  (There are some tricks you can do to make code run with both versions, but for the contest, you should pick one or the other.)</p>
<p>Python is pre-installed on most Linux distributions and on Mac OSX, but you probably need to upgrade.  Windows does not come with Python.</p>
<div class="tab_sync">
<div class="tab_content" title="Windows">

* [Download](http://www.python.org/download/) the latest version from the Python site.
* Select the windows installer for either version 2.7.2 or 3.2.2.  Select the X86-64 version if you have a 64-bit computer.
* Run the installer and select all the defaults.  You should have the python executable at C:\Python27\python.exe or C:\Python32\python.exe
* Add C:\Python27 or C:\Python32 to your path.

Test your installation at the command line and make sure you have 2.7.x or 3.2.x:

    python --version

</div>
<div class="tab_content" title="Linux">

### Check current version

    python --version

### Install or upgrade

Ubuntu:

    sudo apt-get install python

or:

    sudo apt-get install python3

</div>
<div class="tab_content" title="Mac">

### Check current version

    python --version

### Install or upgrade

[Download Python](http://www.python.org/download/)

Download the Mac OSX python .dmg file.  Open up the file and drag the program to your applications folder.

</div>
</div>

<p>Now that Python is installed, you should be able to run play_one_game.cmd and test_bot.cmd successfully.  Continue reading if you want to program your bot in Python.</p>
<h3 id="interactive-mode">Interactive Mode</h3>
<p>You can try running Python interactively to practice proper syntax:</p>
<div class="codehilite"><pre><span></span><span class="n">C</span><span class="p">:</span>\<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="n">python</span>
<span class="n">Python</span> <span class="mf">3.2</span><span class="o">.</span><span class="mi">2</span> <span class="p">(</span><span class="n">default</span><span class="p">,</span> <span class="n">Sep</span>  <span class="mi">4</span> <span class="mi">2011</span><span class="p">,</span> <span class="mi">09</span><span class="p">:</span><span class="mi">51</span><span class="p">:</span><span class="mi">08</span><span class="p">)</span> <span class="p">[</span><span class="n">MSC</span> <span class="n">v</span><span class="o">.</span><span class="mi">1500</span> <span class="mi">32</span> <span class="n">bit</span> <span class="p">(</span><span class="n">Intel</span><span class="p">)]</span> <span class="n">on</span> <span class="n">win32</span>
<span class="n">Type</span> <span class="s2">&quot;help&quot;</span><span class="p">,</span> <span class="s2">&quot;copyright&quot;</span><span class="p">,</span> <span class="s2">&quot;credits&quot;</span> <span class="ow">or</span> <span class="s2">&quot;license&quot;</span> <span class="k">for</span> <span class="n">more</span> <span class="n">information</span><span class="o">.</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="kn">import</span> <span class="nn">math</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="nb">dir</span><span class="p">(</span><span class="n">math</span><span class="p">)</span>
<span class="p">[</span><span class="s1">&#39;__doc__&#39;</span><span class="p">,</span> <span class="s1">&#39;__name__&#39;</span><span class="p">,</span> <span class="s1">&#39;__package__&#39;</span><span class="p">,</span> <span class="s1">&#39;acos&#39;</span><span class="p">,</span> <span class="s1">&#39;acosh&#39;</span><span class="p">,</span> <span class="s1">&#39;asin&#39;</span><span class="p">,</span> <span class="s1">&#39;asinh&#39;</span><span class="p">,</span> <span class="s1">&#39;atan&#39;</span><span class="p">,</span> <span class="s1">&#39;atan2&#39;</span><span class="p">,</span> <span class="s1">&#39;atanh&#39;</span><span class="p">,</span> <span class="s1">&#39;ceil&#39;</span><span class="p">,</span> <span class="s1">&#39;copysign&#39;</span><span class="p">,</span> <span class="s1">&#39;cos&#39;</span><span class="p">,</span> <span class="s1">&#39;cos</span>
<span class="n">h</span><span class="s1">&#39;, &#39;</span><span class="n">degrees</span><span class="s1">&#39;, &#39;</span><span class="n">e</span><span class="s1">&#39;, &#39;</span><span class="n">erf</span><span class="s1">&#39;, &#39;</span><span class="n">erfc</span><span class="s1">&#39;, &#39;</span><span class="n">exp</span><span class="s1">&#39;, &#39;</span><span class="n">expm1</span><span class="s1">&#39;, &#39;</span><span class="n">fabs</span><span class="s1">&#39;, &#39;</span><span class="n">factorial</span><span class="s1">&#39;, &#39;</span><span class="n">floor</span><span class="s1">&#39;, &#39;</span><span class="n">fmod</span><span class="s1">&#39;, &#39;</span><span class="n">frexp</span><span class="s1">&#39;, &#39;</span><span class="n">fsum</span><span class="s1">&#39;, &#39;</span><span class="n">gamma</span><span class="s1">&#39;, &#39;</span><span class="n">hypot</span><span class="s1">&#39;, &#39;</span><span class="n">isfin</span>
<span class="n">ite</span><span class="s1">&#39;, &#39;</span><span class="n">isinf</span><span class="s1">&#39;, &#39;</span><span class="n">isnan</span><span class="s1">&#39;, &#39;</span><span class="n">ldexp</span><span class="s1">&#39;, &#39;</span><span class="n">lgamma</span><span class="s1">&#39;, &#39;</span><span class="n">log</span><span class="s1">&#39;, &#39;</span><span class="n">log10</span><span class="s1">&#39;, &#39;</span><span class="n">log1p</span><span class="s1">&#39;, &#39;</span><span class="n">modf</span><span class="s1">&#39;, &#39;</span><span class="n">pi</span><span class="s1">&#39;, &#39;</span><span class="nb">pow</span><span class="s1">&#39;, &#39;</span><span class="n">radians</span><span class="s1">&#39;, &#39;</span><span class="n">sin</span><span class="s1">&#39;, &#39;</span><span class="n">sinh</span><span class="s1">&#39;, &#39;</span><span class="n">sqrt</span><span class="s1">&#39;, &#39;</span><span class="n">tan</span><span class="s1">&#39;,</span>
<span class="s1">&#39;tanh&#39;</span><span class="p">,</span> <span class="s1">&#39;trunc&#39;</span><span class="p">]</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="n">math</span><span class="o">.</span><span class="n">e</span> <span class="o">**</span> <span class="p">(</span><span class="mi">0j</span> <span class="o">*</span> <span class="n">math</span><span class="o">.</span><span class="n">pi</span><span class="p">)</span>
<span class="p">(</span><span class="mi">1</span><span class="o">+</span><span class="mi">0j</span><span class="p">)</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="s2">&quot;python&quot;</span><span class="p">[</span><span class="mi">2</span><span class="p">:</span><span class="o">-</span><span class="mi">1</span><span class="p">]</span>
<span class="s1">&#39;tho&#39;</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="p">[</span><span class="n">x</span><span class="o">**</span><span class="mi">2</span> <span class="k">for</span> <span class="n">x</span> <span class="ow">in</span> <span class="nb">range</span><span class="p">(</span><span class="mi">20</span><span class="p">)</span> <span class="k">if</span> <span class="n">x</span> <span class="o">%</span> <span class="mi">2</span> <span class="o">==</span> <span class="mi">0</span><span class="p">]</span>
<span class="p">[</span><span class="mi">0</span><span class="p">,</span> <span class="mi">4</span><span class="p">,</span> <span class="mi">16</span><span class="p">,</span> <span class="mi">36</span><span class="p">,</span> <span class="mi">64</span><span class="p">,</span> <span class="mi">100</span><span class="p">,</span> <span class="mi">144</span><span class="p">,</span> <span class="mi">196</span><span class="p">,</span> <span class="mi">256</span><span class="p">,</span> <span class="mi">324</span><span class="p">]</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="k">def</span> <span class="nf">division</span><span class="p">(</span><span class="n">dividend</span><span class="p">,</span> <span class="n">divisor</span><span class="p">):</span>
<span class="o">...</span>     <span class="k">return</span> <span class="n">dividend</span><span class="o">//</span><span class="n">divisor</span><span class="p">,</span> <span class="n">dividend</span><span class="o">%</span><span class="n">divisor</span>
<span class="o">...</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="n">quotient</span><span class="p">,</span> <span class="n">remainder</span> <span class="o">=</span> <span class="n">division</span><span class="p">(</span><span class="mi">17</span><span class="p">,</span><span class="mi">5</span><span class="p">)</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="k">print</span><span class="p">(</span><span class="n">quotient</span><span class="p">,</span> <span class="n">remainder</span><span class="p">)</span>
<span class="mi">3</span> <span class="mi">2</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="k">print</span><span class="p">(</span><span class="s2">&quot;{0} remainder {1}&quot;</span><span class="o">.</span><span class="n">format</span><span class="p">(</span><span class="o">*</span><span class="n">division</span><span class="p">(</span><span class="mi">37</span><span class="p">,</span><span class="mi">7</span><span class="p">)))</span>
<span class="mi">5</span> <span class="n">remainder</span> <span class="mi">2</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="kn">import</span> <span class="nn">os</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="n">os</span><span class="o">.</span><span class="n">getcwd</span><span class="p">()</span>
<span class="s1">&#39;C:</span><span class="se">\\</span><span class="s1">&#39;</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="kn">import</span> <span class="nn">antigravity</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="nb">dir</span><span class="p">()</span>
<span class="p">[</span><span class="s1">&#39;__builtins__&#39;</span><span class="p">,</span> <span class="s1">&#39;__doc__&#39;</span><span class="p">,</span> <span class="s1">&#39;__name__&#39;</span><span class="p">,</span> <span class="s1">&#39;__package__&#39;</span><span class="p">,</span> <span class="s1">&#39;antigravity&#39;</span><span class="p">,</span> <span class="s1">&#39;division&#39;</span><span class="p">,</span> <span class="s1">&#39;math&#39;</span><span class="p">,</span> <span class="s1">&#39;os&#39;</span><span class="p">,</span> <span class="s1">&#39;quotient&#39;</span><span class="p">,</span> <span class="s1">&#39;remainder&#39;</span><span class="p">]</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="n">quit</span><span class="p">()</span>
</pre></div>


<h3 id="python-program">Python Program</h3>
<p>Let's create a python program to produce prime numbers using the <a href="http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes">Sieve of Eratosthenes</a>:</p>
<div class="codehilite"><pre><span></span><span class="nt">C</span><span class="o">:</span><span class="err">\</span><span class="o">&amp;</span><span class="nt">gt</span><span class="o">;</span><span class="nt">notepad</span> <span class="nt">prime</span><span class="p">.</span><span class="nc">py</span>
</pre></div>


<p>Contents:</p>
<div class="codehilite"><pre><span></span><span class="kn">import</span> <span class="nn">argparse</span>
<span class="kn">from</span> <span class="nn">math</span> <span class="kn">import</span> <span class="n">sqrt</span><span class="p">,</span> <span class="n">floor</span>

<span class="k">def</span> <span class="nf">primes</span><span class="p">(</span><span class="n">n</span><span class="p">):</span>
    <span class="n">sieve</span> <span class="o">=</span> <span class="p">{</span><span class="n">x</span><span class="p">:</span> <span class="bp">True</span> <span class="k">for</span> <span class="n">x</span> <span class="ow">in</span> <span class="nb">range</span><span class="p">(</span><span class="mi">2</span><span class="p">,</span> <span class="n">n</span><span class="p">)}</span>
    <span class="k">for</span> <span class="n">num</span> <span class="ow">in</span> <span class="nb">range</span><span class="p">(</span><span class="mi">2</span><span class="p">,</span> <span class="n">floor</span><span class="p">(</span><span class="n">sqrt</span><span class="p">(</span><span class="n">n</span><span class="p">))):</span>
        <span class="k">for</span> <span class="n">multiple</span> <span class="ow">in</span> <span class="nb">range</span><span class="p">(</span><span class="n">num</span> <span class="o">**</span> <span class="mi">2</span><span class="p">,</span> <span class="n">n</span><span class="p">,</span> <span class="n">num</span><span class="p">):</span>
            <span class="n">sieve</span><span class="p">[</span><span class="n">multiple</span><span class="p">]</span> <span class="o">=</span> <span class="bp">False</span>
    <span class="k">return</span> <span class="p">[</span><span class="n">x</span> <span class="k">for</span> <span class="n">x</span><span class="p">,</span> <span class="n">is_prime</span> <span class="ow">in</span> <span class="n">sieve</span><span class="o">.</span><span class="n">items</span><span class="p">()</span> <span class="k">if</span> <span class="n">is_prime</span><span class="p">]</span>

<span class="k">def</span> <span class="nf">main</span><span class="p">():</span>
    <span class="n">parser</span> <span class="o">=</span> <span class="n">argparse</span><span class="o">.</span><span class="n">ArgumentParser</span><span class="p">(</span><span class="n">description</span><span class="o">=</span><span class="s1">&#39;List prime numbers.&#39;</span><span class="p">)</span>
    <span class="n">parser</span><span class="o">.</span><span class="n">add_argument</span><span class="p">(</span><span class="s1">&#39;--max&#39;</span><span class="p">,</span> <span class="nb">type</span><span class="o">=</span><span class="nb">int</span><span class="p">,</span> <span class="n">default</span><span class="o">=</span><span class="mi">100</span><span class="p">,</span>
                        <span class="n">help</span><span class="o">=</span><span class="s1">&#39;highest prime number to generate&#39;</span><span class="p">)</span>
    <span class="n">args</span> <span class="o">=</span> <span class="n">parser</span><span class="o">.</span><span class="n">parse_args</span><span class="p">()</span>

    <span class="k">print</span><span class="p">(</span><span class="s1">&#39; &#39;</span><span class="o">.</span><span class="n">join</span><span class="p">(</span><span class="nb">map</span><span class="p">(</span><span class="nb">str</span><span class="p">,</span> <span class="n">primes</span><span class="p">(</span><span class="n">args</span><span class="o">.</span><span class="n">max</span> <span class="o">+</span> <span class="mi">1</span><span class="p">))))</span>

<span class="k">if</span> <span class="vm">__name__</span> <span class="o">==</span> <span class="s1">&#39;__main__&#39;</span><span class="p">:</span>
    <span class="n">main</span><span class="p">()</span>
</pre></div>


<p>Run the program with:</p>
<div class="codehilite"><pre><span></span>python prime.py --max 1337
</pre></div>


<p>At the top, we imported some helper functions.  Argparse is what we use to add the <code>--max</code> command line option to the program.  The other imports are math functions that we import directly into the main namespace.  The next block of code is our sieve function.  It returns a list of prime numbers up to a given maximum.  The third block of code is our main function.  It parses the command line options, then calls our prime number function and prints the out the result.  The last line of code is a guard to prevent any code from executing if we imported our program into another one.  It will only run the main function if it is being executed directly from the command line.</p>
<p>If you don't understand everything in this program, it is worth studying the individual functions and keywords until you do: import/from...import, def, dictionary comprehensions, range, for...in, floor, sqrt, return, list comprehensions, argparse, print, join, map, str.</p>
<h3 id="performace-and-testing">Performace and testing</h3>
<p>We can test the performance of our prime program with the following:</p>
<div class="codehilite"><pre><span></span>python -m cProfile prime.py --max 3021377
</pre></div>


<p>This will output a large list of function calls and their timings.  The profile will become useful if you want to make your bot as fast as possible.  It will help identify the slowest parts of your program that you need to focus on.  If you run a game with playgame.py and specify the <code>-I</code> option, you should get an input file that you can then redirect into your bot program:</p>
<div class="codehilite"><pre><span></span>python -m cProfile MyBot.py &amp;lt; game_lots\0.bot0.input
</pre></div>


<p>We can use our program interactively as well:</p>
<div class="codehilite"><pre><span></span><span class="n">C</span><span class="p">:</span>\<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="n">python</span>
<span class="n">Python</span> <span class="mf">3.2</span><span class="o">.</span><span class="mi">2</span> <span class="p">(</span><span class="n">default</span><span class="p">,</span> <span class="n">Sep</span>  <span class="mi">4</span> <span class="mi">2011</span><span class="p">,</span> <span class="mi">09</span><span class="p">:</span><span class="mi">51</span><span class="p">:</span><span class="mi">08</span><span class="p">)</span> <span class="p">[</span><span class="n">MSC</span> <span class="n">v</span><span class="o">.</span><span class="mi">1500</span> <span class="mi">32</span> <span class="n">bit</span> <span class="p">(</span><span class="n">Intel</span><span class="p">)]</span> <span class="n">on</span> <span class="n">win32</span>
<span class="n">Type</span> <span class="s2">&quot;help&quot;</span><span class="p">,</span> <span class="s2">&quot;copyright&quot;</span><span class="p">,</span> <span class="s2">&quot;credits&quot;</span> <span class="ow">or</span> <span class="s2">&quot;license&quot;</span> <span class="k">for</span> <span class="n">more</span> <span class="n">information</span><span class="o">.</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="kn">import</span> <span class="nn">prime</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="nb">dir</span><span class="p">(</span><span class="n">prime</span><span class="p">)</span>
<span class="p">[</span><span class="s1">&#39;__builtins__&#39;</span><span class="p">,</span> <span class="s1">&#39;__cached__&#39;</span><span class="p">,</span> <span class="s1">&#39;__doc__&#39;</span><span class="p">,</span> <span class="s1">&#39;__file__&#39;</span><span class="p">,</span> <span class="s1">&#39;__name__&#39;</span><span class="p">,</span> <span class="s1">&#39;__package__&#39;</span><span class="p">,</span> <span class="s1">&#39;argparse&#39;</span><span class="p">,</span> <span class="s1">&#39;floor&#39;</span><span class="p">,</span> <span class="s1">&#39;main&#39;</span><span class="p">,</span> <span class="s1">&#39;primes&#39;</span><span class="p">,</span> <span class="s1">&#39;sqrt&#39;</span><span class="p">]</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="n">prime</span><span class="o">.</span><span class="n">primes</span><span class="p">(</span><span class="mi">10</span><span class="p">)</span>
<span class="p">[</span><span class="mi">2</span><span class="p">,</span> <span class="mi">3</span><span class="p">,</span> <span class="mi">5</span><span class="p">,</span> <span class="mi">7</span><span class="p">,</span> <span class="mi">9</span><span class="p">]</span>
<span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span><span class="o">&amp;</span><span class="n">gt</span><span class="p">;</span> <span class="n">quit</span><span class="p">()</span>
</pre></div>


<p>The <code>dir()</code> function tells us everything that is in the <code>prime</code> namespace that we imported.  You can see our own functions along with the other function we imported.</p>
<p>Now that you are familiar with Python, you should head over to the <a href="ants_tutorial.php">Ants Tutorial</a></p>
<!--</MarkdownReplacement>-->

<?php

require_once('visualizer_widget.php');
visualize_pre();
require_once('footer.php');

?>