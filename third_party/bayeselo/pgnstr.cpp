////////////////////////////////////////////////////////////////////////////
//
// pgnstr.cpp
//
// PGN input/output functions
//
// Remi Coulom
//
// August, 1996
//
////////////////////////////////////////////////////////////////////////////
#include <iostream>
#include <sstream>
#include <cstring>

#include "pgn.h"
#include "str.h"      // CSTR
#include "date.h"     // CDate
#include "player.h"   // CPlayer
#include "pgnlex.h"   // CPGNLex

//
// Tag identifiers
//
const char * const CPGN::tszTag[] =
{
 "Event",
 "Site",
 "Date",
 "Round",
 "White",
 "Black",
 "Result",
 "SetUp",
 "FEN"
};

//
// Result strings (CSTR for the enumeration)
//
const char * const CPGN::tszResult[] = {"0-1", "1/2-1/2", "1-0", "*"};

////////////////////////////////////////////////////////////////////////////
// WriteString
////////////////////////////////////////////////////////////////////////////
static void WriteString(const char *psz, std::ostream &ostr)
{
 ostr << '"';
 for(; *psz; psz++)
  if (*psz == '"' || *psz == '\\')
   ostr << '\\' << *psz;
  else
   ostr << *psz;
 ostr << '"';
}

////////////////////////////////////////////////////////////////////////////
// WriteTag
////////////////////////////////////////////////////////////////////////////
void CPGN::WriteTag(int Tag, const char *psz, std::ostream &ostr)
{
 ostr << '[' << tszTag[Tag] << ' ';
 WriteString(psz, ostr);
 ostr << "]\n";
}

////////////////////////////////////////////////////////////////////////////
// ReadTAG
////////////////////////////////////////////////////////////////////////////
static int ReadTAG(CPGNLex &pgnlex)
{
 int i;

 if (pgnlex.WaitForToken(CPGNLex::TOK_Symbol))
  return CPGN::TAGs;

 for (i = CPGN::TAGs; --i >= 0;)
  if (!strcmp(pgnlex.TokenString(), CPGN::tszTag[i]))
   break;

 if (pgnlex.WaitForToken(CPGNLex::TOK_TagClose))
  return CPGN::TAGs;

 return i;
}

////////////////////////////////////////////////////////////////////////////
// ReadSTR
////////////////////////////////////////////////////////////////////////////
int CPGN::ReadSTR(CSTR &str, CPGNLex &pgnlex)
{
 if (pgnlex.WaitForToken(CPGNLex::TOK_TagOpen))
  return 1;

 int TAGS = 0;
 do
 {
  TAGS++;
  switch (ReadTAG(pgnlex))
  {
   case TAG_Event: str.SetEvent(pgnlex.TokenString()); break;
   case TAG_Site:  str.SetSite(pgnlex.TokenString());  break;
   case TAG_Date:  str.SetDate(pgnlex.TokenString());  break;
   case TAG_Round: str.SetRound(pgnlex.TokenString()); break;
   case TAG_White: str.SetWhite(pgnlex.TokenString()); break;
   case TAG_Black: str.SetBlack(pgnlex.TokenString()); break;
   case TAG_Result:
   {
    int j;

    for (j = CSTR::Results; --j >= 0;)
     if (!strcmp(pgnlex.TokenString(), tszResult[j]))
      break;

    if (j >= 0)
     str.SetResult(j);
   }
   break;
   default:
    TAGS--;
  }
 }
 while (pgnlex.ReadNextToken() == CPGNLex::TOK_TagOpen && TAGS < 7);

 return 0;
}
