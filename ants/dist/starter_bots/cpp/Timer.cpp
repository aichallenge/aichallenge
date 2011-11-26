#include "Timer.h"

#ifdef _WIN32
//This code is a modified version of the code from http://www.mindcontrol.org/~hplus/misc/simple-timer.html
#include <windows.h>

struct Timer_DataMembersStruct
{
    double freq_;
    unsigned __int64 baseTime_;
};

void Timer::start()
{
    if (!opaque) {opaque = new Timer_DataMembersStruct();}

    unsigned __int64 pf;
    QueryPerformanceFrequency( (LARGE_INTEGER *) &pf );
    opaque->freq_ = 1.0 / (double)pf;
    QueryPerformanceCounter( (LARGE_INTEGER *) &(opaque->baseTime_) );
}

double Timer::getTime() const
{
    unsigned __int64 val;
    QueryPerformanceCounter( (LARGE_INTEGER *)&val );
    return (val - opaque->baseTime_) * opaque->freq_ * 1000.0;
}

#else //Mac/Linux Timer
#include <sys/time.h>

struct Timer_DataMembersStruct
{
    double startTime;
};

void Timer::start()
{
    if (!opaque) {opaque = new Timer_DataMembersStruct();}

    timeval timer;
    gettimeofday(&timer, 0);
    opaque->startTime = timer.tv_sec + (  timer.tv_usec / 1000000.0 );
}

double Timer::getTime() const
{
    timeval timer;
    gettimeofday(&timer, 0);
    double currentTime = timer.tv_sec + ( timer.tv_usec / 1000000.0 );
    return (currentTime-opaque->startTime)*1000.0;
}
#endif

//These parts are common to all timers
Timer::Timer(): opaque( 0 /*null*/ ) {}
Timer::~Timer() {delete opaque;}
