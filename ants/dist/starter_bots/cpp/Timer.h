#ifndef TIMER_H_
#define TIMER_H_

#include <sys/time.h>

/*
    struct for checking how long it has been since the start of the turn.
*/
struct Timer
{
    timeval timer;
    double startTime, currentTime;

    Timer()
    {

    };

    //starts the timer
    void start()
    {
        gettimeofday(&timer, NULL);
        startTime = timer.tv_sec+(timer.tv_usec/1000000.0);
    };

    //returns how long it has been since the timer was last started in milliseconds
    double getTime()
    {
        gettimeofday(&timer, NULL);
        currentTime = timer.tv_sec+(timer.tv_usec/1000000.0);
        return (currentTime-startTime)*1000.0;
    };
};

#endif //TIMER_H_
