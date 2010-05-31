////////////////////////////////////////////////////////////////////////////
//
// Remi Coulom
//
// December, 2005
//
////////////////////////////////////////////////////////////////////////////
#include "CPredictionCUI.h"
#include "CEloRatingCUI.h"
#include "CResultSetCUI.h"
#include "random.h"

#include <iostream>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <cmath>

////////////////////////////////////////////////////////////////////////////
// Command strings
////////////////////////////////////////////////////////////////////////////
const char * const CPredictionCUI::tszCommands[] =
{
 "?",
 "results",
 "players",
 "elo",
 "stddev",
 "simulations",
 "rounds",
 "lossscore",
 "drawscore",
 "winscore",
 "multiplier",
 "simulate",
 0
};

////////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////////
CPredictionCUI::CPredictionCUI(CEloRatingCUI &ercuiInit, int openmode) :
 CConsoleUI(&ercuiInit, openmode),
 ercui(ercuiInit),
 vbtvariance(ercui.crs.GetPlayers()),
 Simulations(100000),
 Rounds(1),
 ScoreMultiplier(0.5)
{
 tScore[0] = 0;
 tScore[1] = 1;
 tScore[2] = 2;

 ercui.bt.GetVariance(&vbtvariance[0]);
}

////////////////////////////////////////////////////////////////////////////
// Local prompt
////////////////////////////////////////////////////////////////////////////
void CPredictionCUI::PrintLocalPrompt(std::ostream &out)
{
 out << "Prediction";
}

////////////////////////////////////////////////////////////////////////////
// Process commands
////////////////////////////////////////////////////////////////////////////
int CPredictionCUI::ProcessCommand(const char *pszCommand,
                                   const char *pszParameters,
                                   std::istream &in,
                                   std::ostream &out)
{
 enum
 {
  IDC_Help,
  IDC_Results,
  IDC_Players,
  IDC_Elo,
  IDC_StdDev,
  IDC_Simulations,
  IDC_Rounds,
  IDC_LossScore,
  IDC_DrawScore,
  IDC_WinScore,
  IDC_Multiplier,
  IDC_Simulate
 };

 switch (ArrayLookup(pszCommand, tszCommands))
 {
  case IDC_Help: ///////////////////////////////////////////////////////////
   CConsoleUI::ProcessCommand(pszCommand, pszParameters, in, out);
   out << "Prediction commands\n";
   out << "~~~~~~~~~~~~~~~~~~~\n";
   out << "results ......... open interface to define players and results\n";
   out << "players ......... list players\n";
   out << "elo n [x] ....... get[set] elo of player number n\n";
   out << "stddev n [x] .... get[set] standard deviation of player number n\n";
   out << '\n';
   out << "simulations [n] . get[set] number of simulations\n";
   out << "rounds [n] ...... get[set] number of (double) rounds\n";
   out << "lossscore [n] ... get[set] score obtained when losing\n";
   out << "drawscore [n] ... get[set] score obtained when drawing\n";
   out << "winscore [n] .... get[set] score obtained when winning\n";
   out << "multiplier [x] .. get[set] score multiplier\n";
   out << '\n';
   out << "simulate [n]..... run simulations (n = rng seed)\n";
  break;

  case IDC_Results: ////////////////////////////////////////////////////////
  {
   //
   // Open a rscui to get a list of players and results
   //
   CResultSetCUI rscui(rs, vecName, this);
   rscui.MainLoop(in, out);

   //
   // Find player ratings and variances in ercui
   //
   velo.resize(vecName.size());
   vStdDev.resize(vecName.size());
   for (unsigned i = 0; i < vecName.size(); i++)
   {
    velo[i] = 0;
    vStdDev[i] = 1000;
    int fFound = 0;
    for (int j = ercui.vecName.size(); --j >= 0;)
     if (ercui.vecName[j] == vecName[i])
     {
      fFound = 1;
      velo[i] = ercui.bt.GetElo(j);
      vStdDev[i] = std::sqrt(vbtvariance[j]);
     }
   }
  }
  break;

  case IDC_Players: ////////////////////////////////////////////////////////
  {
   out << std::setw(3) << "Num" << ' ';
   out << std::setw(20) << "Name" << ' ';
   out << std::setw(8) << "Elo" << ' ';
   out << std::setw(8) << "Std.Dev." << '\n';
   for (unsigned i = 0; i < vecName.size(); i++)
   {
    out << std::setw(3) << i << ' ';
    out << std::setw(20) << vecName[i] << ' ';
    out << std::setw(8) << velo[i] << ' ';
    out << std::setw(8) << vStdDev[i] << '\n';
   }
   out << "(" << vecName.size() << " players)\n";
  }
  break;

  case IDC_Elo: ////////////////////////////////////////////////////////////
  {
   std::istringstream is(pszParameters);
   unsigned Player = 0;
   is >> Player;
   if (Player < velo.size())
   {
    is >> velo[Player];
    out << velo[Player] << '\n';
   }
  }
  break;

  case IDC_StdDev: /////////////////////////////////////////////////////////
  {
   std::istringstream is(pszParameters);
   unsigned Player = 0;
   is >> Player;
   if (Player < vStdDev.size())
   {
    is >> vStdDev[Player];
    out << vStdDev[Player] << '\n';
   }
  }
  break;

  case IDC_Simulations: ////////////////////////////////////////////////////
   GetSet<int>(Simulations, pszParameters, out);
  break;

  case IDC_Rounds: /////////////////////////////////////////////////////////
   GetSet<int>(Rounds, pszParameters, out);
  break;

  case IDC_LossScore: //////////////////////////////////////////////////////
   GetSet<int>(tScore[0], pszParameters, out);
  break;

  case IDC_DrawScore: //////////////////////////////////////////////////////
   GetSet<int>(tScore[1], pszParameters, out);
  break;

  case IDC_WinScore: ///////////////////////////////////////////////////////
   GetSet<int>(tScore[2], pszParameters, out);
  break;

  case IDC_Multiplier: /////////////////////////////////////////////////////
   GetSet<double>(ScoreMultiplier, pszParameters, out);
  break;

  case IDC_Simulate: ///////////////////////////////////////////////////////
  {
   int Seed = 0;
   std::istringstream(pszParameters) >> Seed;
   CRandom<unsigned> rnd(Seed);
   int Players = vecName.size();
   std::vector<int> vGames(Players * Players);
   std::vector<int> vScore(Players);

   //
   // Reset arrays
   //
   for (int i = Players * Players; --i >= 0;)
    vGames[i] = 0;
   for (int i = Players; --i >= 0;)
    vScore[i] = 0;

   //
   // Add games that have already been played
   //
   for (int i = rs.GetGames(); --i >= 0;)
   {
    int White = rs.GetWhite(i);
    int Black = rs.GetBlack(i);
    int Result = rs.GetResult(i);
    int Index = White + Players * Black;
    if (vGames[Index] < Rounds)
    {
     vGames[Index]++;
     vScore[White] += tScore[Result];
     vScore[Black] += tScore[2 - Result];
    }
    else
     out << "warning: more games than rounds\n";
   }

   //
   // Arrays to receive cumulative information
   //
   std::vector<int> vTotalScore(Players);
   std::vector<int> vTotalVariance(Players);
   std::vector<int> vTotalRank(Players);
   for (int i = Players; --i >= 0;)
   {
    vTotalScore[i] = 0;
    vTotalVariance[i] = 0;
    vTotalRank[i] = 0;
   }
   std::vector<int> vRankDistribution(Players * Players);
   for (int i = Players * Players; --i >= 0;)
    vRankDistribution[i] = 0;

   //
   // Permutation vector to rank players
   //
   std::vector<int> vPermutation(Players);
   for (int i = Players; --i >= 0;)
    vPermutation[i] = i;

   //
   // Run simulations
   //
   for (int i = Simulations; --i >= 0;)
   {
    std::vector<int> vLocalScore = vScore;
    std::vector<double> veloRandom = velo;

    for (int i = Players; --i >= 0;)
     veloRandom[i] += rnd.NextGaussian() * vStdDev[i];

    for (int White = Players; --White >= 0;)
     for (int Black = Players; --Black >= 0;)
      if (White != Black)
      {
       int Index = White + Players * Black;

       double eloDelta = veloRandom[White] - veloRandom[Black];
       double xWin = ercui.bt.WinProbability(eloDelta);
       double xLoss = xWin + ercui.bt.LossProbability(eloDelta);

       for (int r = Rounds - vGames[Index]; --r >= 0;)
       {
        double x = rnd.NextDouble();
        int Result = 1;
        if (x < xWin)
         Result = 2;
        else if (x < xLoss)
         Result = 0;
        vLocalScore[White] += tScore[Result];
        vLocalScore[Black] += tScore[2 - Result];
       }
      }

    std::sort(vPermutation.begin(),
              vPermutation.end(),
              CIndirectCompare<int>(&vLocalScore[0]));

    for (int Player = Players; --Player >= 0;)
    {
     vTotalScore[Player] += vLocalScore[Player];
     vTotalVariance[Player] += vLocalScore[Player] * vLocalScore[Player];
     vTotalRank[vPermutation[Player]] += Player;
     vRankDistribution[Player + Players * vPermutation[Player]]++;
    }
   }

   //
   // Find maximum name length
   //
   unsigned MaxNameLength = 11;
   for (int i = Players; --i >= 0;)
    if (vecName[i].length() > MaxNameLength)
     MaxNameLength = vecName[i].length();

   //
   // Sort players according to expected score
   //
   std::sort(vPermutation.begin(),
             vPermutation.end(),
             CIndirectCompare<int>(&vTotalScore[0]));

   //
   // Print summary of simulations
   //
   std::ios::fmtflags f = out.flags();

   out.setf(std::ios::fixed, std::ios::floatfield);
   out.precision(2);

   out.setf(std::ios::right, std::ios::adjustfield);
   out << std::setw(4) << "Rank" << ' ';
   out.setf(std::ios::left, std::ios::adjustfield);
   out << std::setw(MaxNameLength + 1) << "Player name";
   out.setf(std::ios::right, std::ios::adjustfield);
   out << std::setw(8) << "Points";
   out << std::setw(8) << "EPoints";
   out << std::setw(8) << "StdDev";
   out << std::setw(8) << "ERank";
   out << "  ";
   for (int k = 0; k < Players; k++)
    out << std::setw(3) << k + 1;
   out << '\n';

   for (int i = 0; i < Players; i++)
   {
    int j = vPermutation[i];

    float Score = float(vTotalScore[j] * ScoreMultiplier) / float(Simulations);
    float Score2 =
     float(vTotalVariance[j] * ScoreMultiplier * ScoreMultiplier) /
     float(Simulations);
    float Variance = Score2 - Score * Score;

    out.setf(std::ios::right, std::ios::adjustfield);
    out << std::setw(4) << i + 1 << ' ';
    out.setf(std::ios::left, std::ios::adjustfield);
    out << std::setw(MaxNameLength + 1) << vecName[j];
    out.setf(std::ios::right, std::ios::adjustfield);
    out << std::setw(8);
    out.precision(1);
    out << float(vScore[j] * ScoreMultiplier);
    out << std::setw(8);
    out.precision(2);
    out << Score;
    out << std::setw(8);
    out << std::sqrt(Variance);
    out << std::setw(8);
    out << float(vTotalRank[j]) / float(Simulations) + 1.0;
    out << "  ";
    for (int k = 0; k < Players; k++)
    {
     double x = (100.0 * vRankDistribution[k + Players * j]) /
                float(Simulations);
     out << std::setw(3) << int(x + 0.5);
    }

    out << '\n';
   }

   out.flags(f);
  }
  break;

  default: /////////////////////////////////////////////////////////////////
   return CConsoleUI::ProcessCommand(pszCommand, pszParameters, in, out);
 }

 return PC_Continue;
}
