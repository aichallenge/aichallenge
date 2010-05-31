////////////////////////////////////////////////////////////////////////////
//
// move.h
//
// CMove class definition
//
// Remi Coulom
//
// june 1996
//
////////////////////////////////////////////////////////////////////////////
#ifndef MOVE_H
#define MOVE_H

#include <iostream>

class CPosition;
class CMove;

std::ostream &operator<<(std::ostream &ostr, const CMove &move);

class CMove // move
{
 private: //////////////////////////////////////////////////////////////////
  unsigned char sqFrom;
  unsigned char sqTo;
  unsigned char ucType;
  unsigned char ucSAN;

 public: ///////////////////////////////////////////////////////////////////
  //
  // Move type flags
  //
  enum
  {
   Normal    = 0x00,
   OO        = 0x80,
   OOO       = 0x40,
   ep        = 0x20,
   Promotion = 0x10
  };

  //
  // SAN flags
  //
  enum
  {
   King    = 0x01,
   Queen   = 0x02,
   Rook    = 0x03,
   Bishop  = 0x04,
   Knight  = 0x05,
   Pawn    = 0x06,
   File    = 0x08,
   Rank    = 0x10,
   Capture = 0x20,
   Check   = 0x40,
   Mate    = 0x80,
   Partial = Mate | File | Rank
   
   // Mate without Check is impossible in a fully computed set of SAN flags
   // This is the reason why mate without check is used as an indicator that
   // the full SAN flags have not been computed
   // (see 'int FullSANComputed() const')

  };

  //
  // Notations
  //
  enum
  {
   StandardAlgebraic, // Only when FullSANComputed()
   LongAlgebraic,     // Partial SAN flags required
   Basic,             // Does not use ucSAN. No ppos required for parsing
   ChessBase
  };

  //
  // Maximum characters of move strings (counting '\0' at the end)
  //
  enum {MaxMoveString = 10};
  
  //
  // Constructors
  //
  CMove() {}
  CMove(const char *psz) {ParseString(psz);}
  
  CMove(unsigned char sqFromNew,
        unsigned char sqToNew,
        unsigned char ucTypeNew = Normal,
        unsigned char ucSANNew = 0)
  {
   Set(sqFromNew, sqToNew, ucTypeNew, ucSANNew);
  }

  //
  // Basic sets and gets
  //
  void SetFrom(unsigned char sq) {sqFrom = sq;}
  void SetTo(unsigned char sq) {sqTo = sq;}
  void SetType(unsigned char uc) {ucType = uc;}
  void SetSAN(unsigned char uc) {ucSAN = uc;}
  void AddSAN(unsigned char uc) {ucSAN |= uc;}
  void Set(unsigned char sqFromNew,
           unsigned char sqToNew,
           unsigned char ucTypeNew = Normal,
           unsigned char ucSANNew = 0)
  {
   SetFrom(sqFromNew);
   SetTo(sqToNew);
   SetType(ucTypeNew);
   SetSAN(ucSANNew);
  }

  unsigned char GetFrom() const {return sqFrom;}
  unsigned char GetTo() const {return sqTo;}
  unsigned char GetType() const {return ucType;}
  unsigned char GetSAN() const {return ucSAN;}

  //
  // Other functions
  //
  int FullSANComputed() const
  {
   return ucSAN && (!(ucSAN & Mate) || (ucSAN & Check));
  }
  int ParseString(const char *psz);
  void MakeString(char *psz, int Notation = StandardAlgebraic) const;
  void GNUChessNotation(char *pszString) const;

  //
  // Null move
  //
  static CMove moveNull;
};

#endif
