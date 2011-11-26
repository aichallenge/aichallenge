#pragma once

struct Timer_DataMembersStruct;

struct Timer
{
    Timer();
    ~Timer();

    void start();
    double getTime() const;

    private:
    Timer_DataMembersStruct* opaque;

    //Since it contains a pointer, it must be noncopyable
    Timer(const Timer& other);
    Timer& operator=(const Timer& other);
};

