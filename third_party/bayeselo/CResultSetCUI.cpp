////////////////////////////////////////////////////////////////////////////
//
// Remi Coulom
//
// December, 2004
//
////////////////////////////////////////////////////////////////////////////
#include "CResultSetCUI.h"
#include "CResultSet.h"
#include "CCondensedResults.h"
#include "CEloRatingCUI.h"
#include "EloDataFromFile.h"
#include "pgnlex.h"
#include "pgn.h"
#include "debug.h"

#include <iostream>
#include <iomanip>
#include <sstream>
#include <algorithm>
#include <fstream>

////////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////////
CResultSetCUI::CResultSetCUI(CResultSet &rsInit,
                             std::vector<std::string> &vecNameInit,
                             CConsoleUI *pcui,
                             int openmode) :
 CConsoleUI(pcui, openmode),
 rs(rsInit),
 vecName(vecNameInit)
{
}

////////////////////////////////////////////////////////////////////////////
// Local prompt
////////////////////////////////////////////////////////////////////////////
void CResultSetCUI::PrintLocalPrompt(std::ostream &out)
{
 out << "ResultSet";
}

////////////////////////////////////////////////////////////////////////////
// Command strings
////////////////////////////////////////////////////////////////////////////
const char * const CResultSetCUI::tszCommands[] =
{
 "?",
 "players",
 "names",
 "results",
 "pgn",
 "reset",
 "addplayer",
 "addresult",
 "removeresult",
 "removeplayer",
 "removerare",
 "pack",
 "readpgn",
 "gen",
 "connect",
 "elo",
 0
};

////////////////////////////////////////////////////////////////////////////
// Compute player width
////////////////////////////////////////////////////////////////////////////
unsigned CResultSetCUI::ComputePlayerWidth() const
{
 unsigned Result = 10;
 for (int i = vecName.size(); --i >= 0;)
  if (vecName[i].size() > Result)
   Result = vecName[i].length();
 return Result;
}

////////////////////////////////////////////////////////////////////////////
// Process commands
////////////////////////////////////////////////////////////////////////////
int CResultSetCUI::ProcessCommand(const char *pszCommand,
                                  const char *pszParameters,
                                  std::istream &in,
                                  std::ostream &out)
{
 enum
 {
  IDC_Help,
  IDC_Players,
  IDC_Names,
  IDC_Results,
  IDC_PGN,
  IDC_Reset,
  IDC_AddPlayer,
  IDC_AddResult,
  IDC_RemoveResult,
  IDC_RemovePlayer,
  IDC_RemoveRare,
  IDC_Pack,
  IDC_ReadPGN,
  IDC_Gen,
  IDC_Connect,
  IDC_Elo
 };

 switch (ArrayLookup(pszCommand, tszCommands))
 {
  case IDC_Help: ///////////////////////////////////////////////////////////
   CConsoleUI::ProcessCommand(pszCommand, pszParameters, in, out);
   out << "ResultSet commands\n";
   out << "~~~~~~~~~~~~~~~~~~\n";
   out << "players ......... list players\n";
   out << "names ........... alphabetical list of player names\n";
   out << "results ......... list game results\n";
   out << "pgn ............. write results in PGN format\n";
   out << '\n';
   out << "reset ........... reset results and players\n";
   out << "addplayer <pn> .. add one player with name <pn>\n";
   out << "addresult w b r . add result (w = white, b = black, r = result)\n";
   out << "                   r = 0 (b wins), 1 (draw), or 2 (w wins)\n";
   out << "removeresult n .. remove result number\n";
   out << "removeplayer n .. remove games of player n\n";
   out << "removerare n .... remove games of players with less than n games\n";
   out << "pack ............ pack players (remove players with 0 games)\n";
   out << "readpgn <file>... read PGN file\n";
   out << "connect [p] [fr]  remove players not connected to p [fr=forbidden result]\n";
   out << '\n';
   out << "elo ............. open Elo-estimation interface\n";
   out << '\n';
  break;

  case IDC_Players: ////////////////////////////////////////////////////////
  {
   CCondensedResults crs(rs);
   std::ios::fmtflags f = out.flags();
   int PlayerWidth = ComputePlayerWidth();

   out.setf(std::ios::right, std::ios::adjustfield);
   out << std::setw(4) << "Num." << ' ';
   out.setf(std::ios::left, std::ios::adjustfield);
   out << std::setw(PlayerWidth) << "Name" << ' ';
   out.setf(std::ios::right, std::ios::adjustfield);
   out << std::setw(6) << "games" << ' ';
   out << std::setw(7) << "score" << ' ';
   out << '\n';

   for (unsigned i = 0; i < vecName.size(); i++)
   {
    out.setf(std::ios::right, std::ios::adjustfield);
    out << std::setw(4) << i << ' ';
    out.setf(std::ios::left, std::ios::adjustfield);
    out << std::setw(PlayerWidth) << vecName[i] << ' ';
    out.setf(std::ios::right, std::ios::adjustfield);
    if (i < unsigned(crs.GetPlayers()))
    {
     out << std::setw(6) << crs.CountGames(i) << ' ';
     out << std::setw(7) << double(crs.Score(i)) / 2 << ' ';
    }
    else
    {
     out << std::setw(6) << 0 << ' ';
     out << std::setw(7) << 0 << ' ';
    }
    out << '\n';
   }

   out.flags(f);
  }
  break;

  case IDC_Names: //////////////////////////////////////////////////////////
  {
   std::vector<std::string> vecSorted = vecName;
   std::sort(vecSorted.begin(), vecSorted.end());
   for (unsigned i = 0; i < vecSorted.size(); i++)
    out << vecSorted[i] << '\n';
  }
  break;

  case IDC_Results: ////////////////////////////////////////////////////////
  {
   std::ios::fmtflags f = out.flags();
   int PlayerWidth = ComputePlayerWidth();

   out.setf(std::ios::right, std::ios::adjustfield);
   out << std::setw(6) << "Game";
   out << std::setw(6) << "White";
   out.setf(std::ios::left, std::ios::adjustfield);
   out << std::setw(PlayerWidth + 1) << " Name of White";
   out.setf(std::ios::right, std::ios::adjustfield);
   out << std::setw(6) << "Black";
   out.setf(std::ios::left, std::ios::adjustfield);
   out << std::setw(PlayerWidth + 1) << " Name of Black";
   out.setf(std::ios::right, std::ios::adjustfield);
   out << std::setw(7) << "Result";
   out << '\n';
   for (int i = 0; i < rs.GetGames(); i++)
   {
    out.setf(std::ios::right, std::ios::adjustfield);
    out << std::setw(6) << i;
    out << std::setw(6) << rs.GetWhite(i);
    out.setf(std::ios::left, std::ios::adjustfield);
    out << ' ' << std::setw(PlayerWidth) << vecName[rs.GetWhite(i)];
    out.setf(std::ios::right, std::ios::adjustfield);
    out << std::setw(6) << rs.GetBlack(i);
    out.setf(std::ios::left, std::ios::adjustfield);
    out << ' ' << std::setw(PlayerWidth) << vecName[rs.GetBlack(i)];
    out.setf(std::ios::right, std::ios::adjustfield);
    out << std::setw(7);
    switch(rs.GetResult(i))
    {
     case 0: out << "0-1"; break;
     case 1: out << "1/2"; break;
     case 2: out << "1-0"; break;
     case 3: out << " * "; break;
    }
    out << '\n';
   }

   out.flags(f);
  }
  break;

  case IDC_PGN: ////////////////////////////////////////////////////////////
  {
   for (int i = 0; i < rs.GetGames(); i++)
   {
    CPGN::WriteTag(CPGN::TAG_White, vecName[rs.GetWhite(i)].c_str(), out);
    CPGN::WriteTag(CPGN::TAG_Black, vecName[rs.GetBlack(i)].c_str(), out);
    CPGN::WriteTag(CPGN::TAG_Result, CPGN::tszResult[rs.GetResult(i)], out);
    out << CPGN::tszResult[rs.GetResult(i)] << "\n\n";
   }
  }
  break;

  case IDC_Reset: //////////////////////////////////////////////////////////
   rs.Reset();
   vecName.clear();
  break;

  case IDC_AddPlayer: //////////////////////////////////////////////////////
   vecName.push_back(pszParameters);
  break;

  case IDC_AddResult: //////////////////////////////////////////////////////
  {
   unsigned White = 0;
   unsigned Black = 0;
   unsigned Result = 0;
   std::istringstream(pszParameters) >> White >> Black >> Result;
   if (White < vecName.size() && Black < vecName.size())
    if (Result <= 2)
     rs.Append(White, Black, Result);
    else
     out << "Error: no such result\n";
   else
    out << "Error: no such player\n";
  }
  break;

  case IDC_RemoveResult: ///////////////////////////////////////////////////
  {
   int n = -1;
   std::istringstream(pszParameters) >> n;
   if (n >= 0 && n < rs.GetGames())
    rs.RemoveGame(n);
   else
    out << "error: bad result number\n";
  }
  break;

  case IDC_RemovePlayer: ///////////////////////////////////////////////////
  {
   int n = -1;
   std::istringstream(pszParameters) >> n;
   if (n >= 0)
    rs.RemovePlayer(n);
  }
  break;

  case IDC_RemoveRare: /////////////////////////////////////////////////////
  {
   int n = -1;
   std::istringstream(pszParameters) >> n;
   rs.RemoveRarePlayers(n);
  }
  break;

  case IDC_Pack: ///////////////////////////////////////////////////////////
   rs.PackPlayers(vecName);
  break;

  case IDC_ReadPGN: ////////////////////////////////////////////////////////
  {
   std::ifstream ifs(pszParameters);
   CPGNLex pgnlex(ifs);
   EloDataFromFile(pgnlex, rs, vecName);
  }
  break;

  case IDC_Gen: ////////////////////////////////////////////////////////////
  {
   int Games = 0;
   std::vector<double> velo;
   std::istringstream is(pszParameters);
   is >> Games;
   out << Games << " games\n";
   while(!is.eof())
   {
    double elo = 0;
    is >> elo;
    velo.push_back(elo);
    out << elo << ' ';
   }
   out << '\n';

   rs.Reset();
   vecName.clear();
   vecName.push_back("player 1");
   vecName.push_back("player 2");
   for (int i = 1000; --i >= 0;)
   {
    rs.Append(0, 1, 2);
    rs.Append(1, 0, 1);
   }
  }
  break;

  case IDC_Connect: ////////////////////////////////////////////////////////
  {
   int Player = 0;
   int ForbiddenResult = 3;
   std::istringstream(pszParameters) >> Player >> ForbiddenResult;
   int NewPlayer = rs.Connect(Player, ForbiddenResult, vecName);
   out << rs.GetPlayers() << " players left\n";
   if (NewPlayer != Player)
    out << "Player " << Player << " is now " << NewPlayer << '\n';
  }
  break;

  case IDC_Elo: ////////////////////////////////////////////////////////////
  {
   CEloRatingCUI ercui(rs, vecName, this);
   ercui.MainLoop(in, out);
  }
  break;

  default: /////////////////////////////////////////////////////////////////
   return CConsoleUI::ProcessCommand(pszCommand, pszParameters, in, out);
 }

 return PC_Continue;
}
