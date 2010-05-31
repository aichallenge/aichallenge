////////////////////////////////////////////////////////////////////////////
//
// CTimeIO.cpp
//
// Remi Coulom
//
// August 1996
//
////////////////////////////////////////////////////////////////////////////
#include "CTimeIO.h"
#include "chtime.h"

#include <iostream>
#include <iomanip>
#include <sstream>

////////////////////////////////////////////////////////////////////////////
// output operator
////////////////////////////////////////////////////////////////////////////
std::ostream &operator<<(std::ostream &ostr, const CTime &time)
{
 char cOldFill = ostr.fill('0');
 int OldWidth = ostr.width(2);

 ostr << std::setw(2) << time.GetHours() << ':';
 ostr << std::setw(2) << time.GetMinutes() << ':';
 ostr << std::setw(2) << time.GetSeconds() << ',';
 ostr << std::setw(2) << time.GetHundredths();

 ostr.width(OldWidth);
 ostr.fill(cOldFill);

 return ostr;
}

////////////////////////////////////////////////////////////////////////////
// input operator
////////////////////////////////////////////////////////////////////////////
std::istream &operator>>(std::istream &istr, CTime &time)
{
 char szBuffer[12];

 {
  int i = 0;
  for (; i < (int)sizeof(szBuffer) - 1; i++)
  {
   int c = istr.peek();
   if (c == ' ' || c == '\n' || c == EOF)
    break;
   szBuffer[i] = c;
   istr.ignore();
  }
  szBuffer[i] = 0;
 }

 time.Set(szBuffer);
 return istr;
}
