/////////////////////////////////////////////////////////////////////////////
//
// Rémi Coulom
//
// December, 2004
//
/////////////////////////////////////////////////////////////////////////////
#include "EloDataFromFile.h"
#include "pgnlex.h"
#include "pgn.h"
#include "str.h"
#include "CResultSet.h"

#include <map>
#include <string>
#include <iostream>

/////////////////////////////////////////////////////////////////////////////
// Skip a game
/////////////////////////////////////////////////////////////////////////////
static void SkipGame(CPGNLex &pgnlex)
{
 int Token = pgnlex.GetToken();

 while (Token != CPGNLex::TOK_EOF)
 {
  int TokenPrev = Token;
  Token = pgnlex.ReadNextToken(); 

  if (TokenPrev == CPGNLex::TOK_GameTermination ||
      Token == CPGNLex::TOK_EOF ||
      (TokenPrev != CPGNLex::TOK_TagClose &&
       TokenPrev != CPGNLex::TOK_BOF &&
       Token == CPGNLex::TOK_TagOpen))
   return;
 }

 return;
}

/////////////////////////////////////////////////////////////////////////////
// Read all data for Elo calculation
/////////////////////////////////////////////////////////////////////////////
void EloDataFromFile(CPGNLex &pgnlex,
                     CResultSet &rs,
                     std::vector<std::string> &vNames)
{
 int Players = 0;
 int Ignored = 0;

 //
 // Add current players to the name map
 //
 std::map<std::string, int> NameMap;
 for (int i = vNames.size(); --i >= 0;)
 {
  std::pair<const std::string, int> Pair(vNames[i], i);
  NameMap.insert(Pair);
  Players++;
 }

 //
 // Loop over games
 //
 while (1)
 {
  CSTR str;
  int fTheEnd = CPGN::ReadSTR(str, pgnlex);
  SkipGame(pgnlex);
  
  if (!fTheEnd)
  {
   //
   // Ignore unknown results
   //
   int r = str.GetResult();
   if (r < 0 || r > 2)
    Ignored++;
   else
   {
    std::string sWhite(str.GetWhite());
    std::string sBlack(str.GetBlack());

    {
     std::pair<const std::string, int> Pair(sWhite, Players);
     if (NameMap.insert(Pair).second)
     {
      Players++;
      vNames.push_back(sWhite);
     }
    }
    {
     std::pair<const std::string, int> Pair(sBlack, Players);
     if (NameMap.insert(Pair).second)
     {
      Players++;
      vNames.push_back(sBlack);
     }
    }

    int WhitePlayer = NameMap.find(sWhite)->second;
    int BlackPlayer = NameMap.find(sBlack)->second;

    rs.Append(WhitePlayer, BlackPlayer, r);
   }
  }

  if (rs.GetGames() % 1000 == 0 || fTheEnd)
  {
   std::cerr << rs.GetGames() << " game(s) loaded, ";
   std::cerr << Ignored << " game(s) with unknown result ignored.\r";
  }

  if (fTheEnd)
  {
   std::cerr << '\n';
   break;
  }
 }
}
