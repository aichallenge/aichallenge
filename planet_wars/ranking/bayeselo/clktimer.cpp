////////////////////////////////////////////////////////////////////////////
//
// clktimer.cpp
//
// CClockTimer
//
// Remi Coulom
//
// October, 1996
//
////////////////////////////////////////////////////////////////////////////
#include "clktimer.h"

#ifdef CLOCK_FTIME /////////////////////////////////////////////////////////
#define MILLISECONDS

#include <time.h>
#include <sys/timeb.h>

static long lTime0;

static struct CGetTheProgramStartingTime
{
 CGetTheProgramStartingTime()
 {
  struct timeb tb;
  ftime(&tb);
  lTime0 = tb.time;
 }
} ObjectThatGetsTheProgramStartingTime;

static long MilliSeconds()
{
 struct timeb tb;
 ftime(&tb);
 return (tb.time - lTime0) * 1000 + tb.millitm;
}

#elif defined(CLOCK_GETTIMEOFDAY) //////////////////////////////////////////
#define MILLISECONDS

#include <sys/time.h>

static long lTime0;

static struct CGetTheProgramStartingTime
{
 CGetTheProgramStartingTime()
 {
  struct timeval tv;
  gettimeofday(&tv, 0);
  lTime0 = tv.tv_sec;
 }
} ObjectThatGetsTheProgramStartingTime;

static long MilliSeconds()
{
 struct timeval tv;
 gettimeofday(&tv, 0);
 return (tv.tv_sec - lTime0) * 1000 + tv.tv_usec / 1000;
}

#elif defined(CLOCK_PERFCOUNTER) ///////////////////////////////////////////
#define MILLISECONDS

#include <windows.h>
#include "debug.h"

static LARGE_INTEGER StartingTime;
static LARGE_INTEGER Frequency;

static struct CGetTheProgramStartingTime
{
 CGetTheProgramStartingTime()
 {
  FATAL(!QueryPerformanceFrequency(&Frequency));
  QueryPerformanceCounter(&StartingTime);
 }
} ObjectThatGetsTheProgramStartingTime;

static long MilliSeconds()
{
 LARGE_INTEGER Counter;
 QueryPerformanceCounter(&Counter);
 return (long)(((Counter.QuadPart - StartingTime.QuadPart) * 1000) / Frequency.QuadPart);
}

#endif

#ifdef MILLISECONDS //######################################################

////////////////////////////////////////////////////////////////////////////
// Default constructor
////////////////////////////////////////////////////////////////////////////
CClockTimer::CClockTimer()
{
 lPrevious = MilliSeconds();
}

////////////////////////////////////////////////////////////////////////////
// Function to wait for a clock tick
// (needed for accurate timing)
////////////////////////////////////////////////////////////////////////////
void CClockTimer::Wait(long lMilliSeconds)
{
 long t = MilliSeconds();
 while (MilliSeconds() <= t + lMilliSeconds);
 GetInterval();
}

////////////////////////////////////////////////////////////////////////////
// WaitInterval
////////////////////////////////////////////////////////////////////////////
void CClockTimer::WaitInterval(long lMilliSeconds)
{
 while (MilliSeconds() <= lPrevious + lMilliSeconds);
 lPrevious += lMilliSeconds;
}

////////////////////////////////////////////////////////////////////////////
// Returns time elapsed since construction or last call
////////////////////////////////////////////////////////////////////////////
CTime CClockTimer::GetInterval()
{
 const long lTime = MilliSeconds();
 const long lResult = (lTime - lPrevious) / 10;
 lPrevious = lTime - (lTime - lPrevious) % 10;

 return lResult;
}

#else // ###################################################################

#include <time.h>

////////////////////////////////////////////////////////////////////////////
// Default constructor
////////////////////////////////////////////////////////////////////////////
CClockTimer::CClockTimer()
{
 time_t Previous;
 time(&Previous);
 lPrevious = Previous;
}

////////////////////////////////////////////////////////////////////////////
// Function to wait for the beginning of a second
// (needed for accurate timing)
////////////////////////////////////////////////////////////////////////////
void CClockTimer::Wait(long lMilliSeconds)
{
 time_t t, Bidon;

 time (&t);
 while ((long)(time(&Bidon)) <= (long)(t + lMilliSeconds / 1000));
 GetInterval();
}

////////////////////////////////////////////////////////////////////////////
// WaitInterval
////////////////////////////////////////////////////////////////////////////
void CClockTimer::WaitInterval(long lMilliSeconds) {
 Wait(lMilliSeconds); // ???
}

////////////////////////////////////////////////////////////////////////////
// Returns number of seconds since construction or last call
////////////////////////////////////////////////////////////////////////////
CTime CClockTimer::GetInterval()
{
 time_t Time = (time_t)lPrevious;
 time_t Previous;
 time(&Previous);
 lPrevious = Previous;
 return (lPrevious - Time) * 100;
}

#endif // ##################################################################
