#ifndef TIMER_H_
#define TIMER_H_

#include <sys/time.h>

/*
    struct for checking how long it has been since the start of the turn.
*/
#ifdef _WIN32 //Windows timer (DON'T USE THIS TIMER UNLESS YOU'RE ON WINDOWS!)
    #include <io.h>
    #include <windows.h>

    struct Timer
    {
        clock_t startTime, currentTime;

        Timer()
        {

        };

        void start()
        {
            startTime = clock();
        };

        double getTime()
        {
            currentTime = clock();

            return (double)(currentTime - startTime);
        };
    };

#else //Mac/Linux Timer
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
#endif


#endif //TIMER_H_
