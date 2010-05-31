////////////////////////////////////////////////////////////////////////////
//
// pgn.h
//
// PGN input/output functions
//
// Remi Coulom
//
// august 1996
//
////////////////////////////////////////////////////////////////////////////
#ifndef PGN_H
#define PGN_H

#include <iostream>

class CGame;
class CJustifiedText;
class CConstGameCursor;
class CPGNLex;
class CSTR;

class CPGN
{
 public : //////////////////////////////////////////////////////////////////
  static const char * const tszResult[];

  enum
  {
   NoMoveNumber = 1,
   NoComment    = 2,
   NoNAG        = 4,
   NoRAV        = 8
  };

  static const char * const tszTag[];

  enum
  {
   TAG_Event,
   TAG_Site,
   TAG_Date,
   TAG_Round,
   TAG_White,
   TAG_Black,
   TAG_Result,
   TAG_SetUp,
   TAG_FEN,
   TAGs
  };
 
  static void WriteMoveText(CJustifiedText &jt,
                            CConstGameCursor &cgc,
                            int Flags = 0);
  static void Write(const CGame &game, std::ostream &ostr, int Flags = 0);
 
  static int Read(CGame &game, CPGNLex &pgnlex);
  static int ReadSTR(CSTR &str, CPGNLex &pgnlex);
  static void WriteTag(int Tag, const char *psz, std::ostream &ostr);
};

#endif
