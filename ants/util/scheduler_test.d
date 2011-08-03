// compile with: dmd -O -release scheduler_test.d 
module scheduler_test;

import std.stdio;
import std.conv;
import std.datetime;
import std.math;
import core.time;
import core.thread;

int main(string[] args)
{
	if (args.length != 2)
	{
		stderr.writefln("The syntax is: %s <sleep time in ms>", args[0]);
		return 1;
	}
	
	long interval = to!long(args[1]);
	stdout.writefln("Running scheduler test with %s ms sleep intervals. Every delay of 10%% or more", interval);
	stdout.writeln ("will be logged in addition to the current maximum delay.");
	stdout.writeln(); 
	stdout.writeln("timestamp                | delay >= 10% | current maximum | new maximum ?"); 
	stdout.writeln("-------------------------+--------------+-----------------+--------------");

	long max = 0;
	TickDuration start = TickDuration.currSystemTick();
	while (true)
	{
		Thread.sleep(dur!"msecs"(interval));
		TickDuration end = TickDuration.currSystemTick();
		long msecs = (end - start).msecs;
		bool maxChanged = max < msecs;
		if (maxChanged)
		{
			max = msecs;
		}
		bool deviation = 10 * msecs >= 11 * interval;

		if (deviation || maxChanged)
		{
			SysTime now = Clock.currTime();
			FracSec fraction = now.fracSec();
			// truncate everything beyond millisecond accuracy
			fraction.msecs(fraction.msecs());
			now.fracSec(fraction);
			stdout.writef("%-24s | ", now.toSimpleString());
		}
		if (deviation && maxChanged)
		{
			stdout.writefln("%9s ms |%13s ms |         yes", msecs, max);
		}
		else if (deviation)
		{
			stdout.writefln("%9s ms |%13s ms |", msecs, max);
		}
		else if (maxChanged)
		{
			stdout.writefln("             |%13s ms |         yes", max);
		}

		start = end;
	}
}
