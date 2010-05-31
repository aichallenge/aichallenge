////////////////////////////////////////////////////////////////////////////
//
// CTimeIO.h
//
// Remi Coulom
//
// August, 1996
//
////////////////////////////////////////////////////////////////////////////
#ifndef CTimeIO_Declared
#define CTimeIO_Declared

#include <iosfwd>
class CTime;

std::ostream &operator<<(std::ostream &ostr, const CTime &time);
std::istream &operator>>(std::istream &istr, CTime &time);

#endif
