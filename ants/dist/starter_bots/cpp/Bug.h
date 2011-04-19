#ifndef BUG_H_
#define BUG_H_

#include <fstream>

#ifndef DEBUG
    //#define DEBUG
#endif

/*
    struct for debugging - this is gross but can be used pretty much like an ofstream,
                           except the debug messages are stripped while compiling if
                           DEBUG is not defined.
    example:
        Bug bug;
        bug.open("./debug.txt");
        bug << state << endl;
        bug << "testing" << 2.0 << '%' << endl;
        bug.close();
*/
struct Bug
{
    std::ofstream file;

    Bug()
    {

    };

    //opens the specified file
    inline void open(const std::string &filename)
    {
        #ifdef DEBUG
            file.open(filename.c_str());
        #endif
    };

    //closes the ofstream
    inline void close()
    {
        #ifdef DEBUG
            file.close();
        #endif
    };
};

//output function for endl
inline Bug& operator<<(Bug &bug, std::ostream& (*manipulator)(std::ostream&))
{
    #ifdef DEBUG
        bug.file << manipulator;
    #endif

    return bug;
};

//output function
template <class T>
inline Bug& operator<<(Bug &bug, const T &t)
{
    #ifdef DEBUG
        bug.file << t;
    #endif

    return bug;
};

#endif //BUG_H_
