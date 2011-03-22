#ifndef BUG_H_
#define BUG_H_

#include <fstream>

/*
    struct for debugging - this is gross but can be used pretty much like an ofstream, except
                           the debug messages are stripped while compiling if DEBUG is false.
    example:
        Bug bug;
        bug.open("./debug.txt");
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
        #if DEBUG
            file.open(filename.c_str());
        #endif
    };

    //closes the ofstream
    inline void close()
    {
        #if DEBUG
            file.close();
        #endif
    };
};

//output function for endl
inline Bug& operator<<(Bug &bug, std::ostream& (*manipulator)(std::ostream&))
{
    #if DEBUG
        bug.file << manipulator;
    #endif

    return bug;
};

//template output function
template <class T>
inline Bug& operator<<(Bug &bug, const T &t)
{
    #if DEBUG
        bug.file << t;
    #endif

    return bug;
};

#endif //BUG_H_
