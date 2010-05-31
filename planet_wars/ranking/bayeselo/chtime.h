////////////////////////////////////////////////////////////////////////////
//
// chtime.h
//
// CTime class declaration
//
// Remi Coulom
//
// August 1996
//
////////////////////////////////////////////////////////////////////////////
#ifndef CTime_Declared
#define CTime_Declared

class CTime //time
{
 private: //////////////////////////////////////////////////////////////////
  long lTime; // time in hundredths of a second

 public: ///////////////////////////////////////////////////////////////////
  //
  // Compatibility with the long type
  //
  operator long () const {return lTime;}
  CTime(long l) {lTime = l;}
  CTime operator += (long l) {return lTime += l;}
  CTime operator -= (long l) {return lTime -= l;}
  CTime operator *= (long l) {return lTime *= l;}
  CTime operator /= (long l) {return lTime /= l;}

  //
  // Default constructor
  //
  CTime() {lTime = 0;}

  //
  // Gets for HH:MM:SS,HH
  //
  long GetHours() const {return (lTime / 360000L);}
  long GetMinutes() const {return (lTime / 6000) % 60;}
  long GetSeconds() const {return (lTime / 100) % 60;}
  long GetHundredths() const {return lTime % 100;}

  //
  // Sets
  //
  void Reset() {lTime = 0;}
  void Set(long lHours,
           long lMinutes,
           long lSeconds,
           long lHundredths)
  {
   lTime = lHundredths +
            lSeconds * 100 +
            lMinutes * 6000 +
            lHours * 360000L;
  }
  void Set(const char *psz);
};

#endif
