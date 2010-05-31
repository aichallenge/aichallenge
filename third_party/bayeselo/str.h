////////////////////////////////////////////////////////////////////////////
//
// str.h
//
// CSTR class definition
// (Seven Tag Roster)
//
// R‚mi Coulom
//
// june 1996
//
////////////////////////////////////////////////////////////////////////////
#ifndef STR_H
#define STR_H

#include "date.h" // CDate

class CSTR // str
{
 private: //////////////////////////////////////////////////////////////////
  char szEvent[64];
  char szSite[64];
  CDate date;
  char szRound[32];
  char szWhite[64];
  char szBlack[64];
  int Result;

 public: ///////////////////////////////////////////////////////////////////
  //
  // Possible Results
  //
  enum
  {
   BlackWins, // 0-1
   Draw,      // 1/2-1/2
   WhiteWins, // 1-0
   Unknown,   // *
   Results
  };

  //
  // Default constructor, to prevent dangerous uninitialized data
  //
  CSTR();

  //
  // Sets
  //
  void Reset();
  void SetResult(int i);
  void SetDate(const CDate &dateNew) {date = dateNew;}
  void SetEvent(const char *psz);
  void SetSite(const char *psz);
  void SetRound(const char *psz);
  void SetWhite(const char *psz);
  void SetBlack(const char *psz);

  //
  // Gets
  //
  int GetResult() const {return Result;}
  const CDate &GetDate () const {return date;}
  const char *GetEvent() const {return szEvent;}
  const char *GetSite () const {return szSite;}
  const char *GetRound() const {return szRound;}
  const char *GetWhite() const {return szWhite;}
  const char *GetBlack() const {return szBlack;}
};

#endif
