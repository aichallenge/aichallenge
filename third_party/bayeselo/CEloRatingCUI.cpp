////////////////////////////////////////////////////////////////////////////
//
// Remi Coulom
//
// December, 2004
//
////////////////////////////////////////////////////////////////////////////
#include "CEloRatingCUI.h"
#include "CResultSet.h"
#include "CCDistribution.h"
#include "CCDistributionCUI.h"
#include "CDistributionCollection.h"
#include "CJointBayesian.h"
#include "clktimer.h"
#include "chtime.h"
#include "CTimeIO.h"
#include "CMatrix.h"
#include "CPredictionCUI.h"
#include "CIndirectCompare.h"
#include "ReadLineToString.h"

#include <iostream>
#include <iomanip>
#include <algorithm>
#include <sstream>
#include <fstream>
#include <set>

////////////////////////////////////////////////////////////////////////////
// Command strings
////////////////////////////////////////////////////////////////////////////
const char * const CEloRatingCUI::tszCommands[] =
{
 "?",
 "ratings",
 "details",
 "offset",
 "scale",
 "minelo",
 "maxelo",
 "advantage",
 "drawelo",
 "prior",
 "resolution",
 "confidence",
 "p",
 "likelihood",
 "prediction",
 "plotres",
 "plotdraw",
 "mm",
 "elostat",
 "elo",
 "jointdist",
 "exactdist",
 "variance",
 "covariance",
 "los",
 "advdist",
 "drawdist",
 "pairstats",
 0
};

////////////////////////////////////////////////////////////////////////////
// Convert to closes int
////////////////////////////////////////////////////////////////////////////
static int RoundDouble(double x)
{
 if (x > 0)
  return int(x+0.5);
 else
  return int(x-0.5);
}

////////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////////
CEloRatingCUI::CEloRatingCUI(const CResultSet &rsInit,
                             const std::vector<std::string> &vecNameInit,
                             CConsoleUI *pcui,
                             int openmode) :
 CConsoleUI(pcui, openmode),
 rs(rsInit),
 crs(rsInit),
 vecName(vecNameInit),
 vPermutation(crs.GetPlayers()),
 veloLower(crs.GetPlayers()),
 veloUpper(crs.GetPlayers()),
 Confidence(0.95),
 eloMin(-1500),
 eloMax(1500),
 Resolution(1001),
 eloOffset(0),
 EloScale(1.0),
 Prior(2.0),
 bt(crs),
 fLOSComputed(0)
{
 for (int i = crs.GetPlayers(); --i >= 0;)
 {
  bt.SetElo(i, 0);
  veloLower[i] = 0;
  veloUpper[i] = 0;
  vPermutation[i] = i; 
 }

 MaxNameLength = 0;
 for (int i = crs.GetPlayers(); --i >= 0;)
  if (vecName[i].length() > MaxNameLength)
   MaxNameLength = vecName[i].length();

 crs.AddPrior(Prior);
}

////////////////////////////////////////////////////////////////////////////
// Local prompt
////////////////////////////////////////////////////////////////////////////
void CEloRatingCUI::PrintLocalPrompt(std::ostream &out)
{
 out << "EloRating";
}

////////////////////////////////////////////////////////////////////////////
// Compute variance
////////////////////////////////////////////////////////////////////////////
void CEloRatingCUI::ComputeVariance()
{
 std::vector<double> vVariance(crs.GetPlayers());
 bt.GetVariance(&vVariance[0]);
 CCDistribution cdist(1000, -10, 10);
 cdist.SetNormal(0, 1);
 double x = cdist.GetUpperValue(Confidence);
 for (int i = crs.GetPlayers(); --i >= 0;)
  if (crs.GetOpponents(i) > 0)
   veloLower[i] = veloUpper[i] = x * std::sqrt(vVariance[i]);
}

////////////////////////////////////////////////////////////////////////////
// Process commands
////////////////////////////////////////////////////////////////////////////
int CEloRatingCUI::ProcessCommand(const char *pszCommand,
                                  const char *pszParameters,
                                  std::istream &in,
                                  std::ostream &out)
{
 enum
 {
  IDC_Help,
  IDC_Ratings,
  IDC_Details,
  IDC_Offset,
  IDC_Scale,
  IDC_MinElo,
  IDC_MaxElo,
  IDC_Advantage,
  IDC_DrawElo,
  IDC_Prior,
  IDC_Resolution,
  IDC_Confidence,
  IDC_P,
  IDC_Likelihood,
  IDC_Prediction,
  IDC_PlotRes,
  IDC_PlotDraw,
  IDC_MM,
  IDC_ELOstat,
  IDC_Elo,
  IDC_JointDist,
  IDC_ExactDist,
  IDC_Variance,
  IDC_Covariance,
  IDC_LOS,
  IDC_AdvDist,
  IDC_DrawDist,
  IDC_PairStats
 };

 switch (ArrayLookup(pszCommand, tszCommands))
 {
  case IDC_Help: ///////////////////////////////////////////////////////////
   CConsoleUI::ProcessCommand(pszCommand, pszParameters, in, out);
   out << "EloRating commands\n";
   out << "~~~~~~~~~~~~~~~~~~\n";
   out << "advantage [x] ... get[set] advantage of playing first\n";
   out << "drawelo [x] ..... get[set] draw Elo\n";
   out << "prior [x] ....... get[set] prior (= number of virtual draws)\n";
   out << "elo [p] [elo] ... get[set] Elo of player number p\n";
   out << "mm [a] [d] ...... compute maximum-likelihood Elos:\n";
   out << "                   a: flag to compute advantage (default = 0)\n";
   out << "                   d: flag to compute elodraw (default = 0)\n";
   out << "elostat ......... compute ratings with ELOstat algorithm\n";
   out << '\n';
   out << "ratings [min [f [F]]] list players and their ratings:\n";
   out << "                   min: minimum number of games\n";
   out << "                   f: file name that contains the list of names\n";
   out << "                   F: Full-rank flag (0 or 1, default 0)\n";
   out << "details ......... detailed list\n";
   out << "offset [elo [pl]] get[set] Elo offset, or player (pl) elo\n";
   out << "scale ........... get[set] Elo scale\n";
   out << '\n';
   out << "p <w> <b> <r> ... probability of result <r> with Elos <w> and <b>\n";
   out << "likelihood ...... log-likelihood of the current Elo list\n";
   out << "prediction ...... open prediction interface\n";
   out << '\n';
   out << "plotres [i] [M] . plot result likelihoods as a function of rating diff:\n";
   out << "                   i: number of intervals (default = 21)\n";
   out << "                   M: maximum rating difference (default = 500)\n";
   out << "plotdraw [n] [d]  plot draw frequency as a function of average rating\n";
   out << "                   n: number of games per group (default = 100)\n";
   out << "                   d: maximum rating difference (default = 100)\n";
   out << '\n';
   out << "confidence ...... get[set] level of confidence intervals\n";
   out << "variance ........ compute intervals with the diagonal of the Hessian\n";
   out << "covariance ...... compute intervals with the full Hessian\n";
   out << "los [f] [p] [w] . likelihood of superiority (f=first,p=players,w=width)\n";
   out << '\n';
   out << "minelo [x] ...... get[set] minimum Elo\n";
   out << "maxelo [x] ...... get[set] maximum Elo\n";
   out << "resolution [n] .. get[set] resolution\n";
   out << "jointdist [p] ... compute intervals from joint distribution\n";
   out << "exactdist [p] ... compute intervals assuming exact opponent Elos\n";
   out << "advdist ......... likelihood distribution of advantage\n";
   out << "drawdist ........ likelihood distribution of drawelo\n";
   out << '\n';
   out << "pairstats i j ... get stats between players i and j\n";
   out << '\n';
  break;

  case IDC_MinElo: /////////////////////////////////////////////////////////
   GetSet<double>(eloMin, pszParameters, out);
  break;

  case IDC_MaxElo: /////////////////////////////////////////////////////////
   GetSet<double>(eloMax, pszParameters, out);
  break;

  case IDC_Advantage: //////////////////////////////////////////////////////
  {
   double eloAdvantage = bt.GetAdvantage();
   GetSet<double>(eloAdvantage, pszParameters, out);
   bt.SetAdvantage(eloAdvantage);
  }
  break;

  case IDC_DrawElo: ////////////////////////////////////////////////////////
  {
   double eloDraw = bt.GetDrawElo();
   GetSet<double>(eloDraw, pszParameters, out);
   bt.SetDrawElo(eloDraw);
  }
  break;

  case IDC_Prior: //////////////////////////////////////////////////////////
  {
   float NewPrior = Prior;
   std::istringstream(pszParameters) >> NewPrior;
   crs.AddPrior(NewPrior - Prior);
   Prior = NewPrior;
   out << Prior << '\n';
   out << "With this prior, you will get the following Elo differences:\n";
   CResultSet rs;
   for (int i = 1; i < 6; i++)
   {
    rs.Append(0, 1, 2);
    CCondensedResults crs(rs);
    crs.AddPrior(Prior);
    CBradleyTerry bt(crs);
    bt.MinorizationMaximization(0, 0);
    double Delta = bt.GetElo(0) - bt.GetElo(1);
    out << i << "-0 : +" << Delta << '\n';
   }
  }
  break;

  case IDC_Resolution: /////////////////////////////////////////////////////
   GetSet<int>(Resolution, pszParameters, out);
  break;

  case IDC_Confidence: /////////////////////////////////////////////////////
   GetSet<double>(Confidence, pszParameters, out);
  break;

  case IDC_P: //////////////////////////////////////////////////////////////
  {
   double eloWhite = 0;
   double eloBlack = 0;
   int Result = 0;

   std::istringstream is(pszParameters);
   is >> eloWhite >> eloBlack >> Result;

   out << "White Elo: " << eloWhite << '\n';
   out << "Black Elo: " << eloBlack << '\n';
   out << "Result: " << Result << '\n';

   out << bt.ResultProbability(eloWhite - eloBlack, Result) << '\n';
  }
  break;

  case IDC_Likelihood: /////////////////////////////////////////////////////
   out << bt.LogLikelihood() << '\n';
  break;

  case IDC_Prediction: /////////////////////////////////////////////////////
  {
   CPredictionCUI predcui(*this);
   predcui.MainLoop(in, out);
  }
  break;

  case IDC_PlotRes: ////////////////////////////////////////////////////////
  {
   //
   // Parse parameters
   //
   int Intervals = 21;
   double eloMaxDiff = 500;
   {
    std::istringstream is(pszParameters);
    is >> Intervals;
    is >> eloMaxDiff;
   }

#if 0 // Compute ratings with a half the games, and mesure predictions on the other half
   unsigned Seed = 1;
   CResultSet rsBT;
   CResultSet rsRes;
   rs.Extract(rsBT, rsRes, Seed);

   CCondensedResults crsBT(rsBT);
   CBradleyTerry btRes(crsBT);
   for (int i = crsBT.GetPlayers(); --i >= 0;)
    btRes.SetElo(i, 0);
   crsBT.AddPrior(Prior);
//   btRes.MinorizationMaximization(0, 0);
   btRes.ELOstat();
#if 0 // compute log-likelihood for 100 priors
   {
    std::ofstream ofsCV("cv.dat");
    for (int i = 0; i < 200; i++)
    {
     double Prior = 0.0000001 + i * 0.1;
     CCondensedResults crsBT(rsBT);
     crsBT.AddPrior(Prior);
     CBradleyTerry btRes(crsBT);
     for (int i = crsBT.GetPlayers(); --i >= 0;)
      btRes.SetElo(i, 0);
     btRes.MinorizationMaximization(0, 0);
     double LL = btRes.LogLikelihood(rsRes);
     ofsCV << Prior << ' ' << LL << '\n';
     ofsCV.flush();
     std::cerr << "Prior = " << Prior;
     std::cerr << "; Loglikelihood = " << LL;
     std::cerr << "\n\n";
    }
   }
#endif
#else
   const CResultSet& rsRes = rs;
   const CBradleyTerry& btRes = bt;
#endif

//#define PLOTDIST

   //
   // Vector to store data for each interval
   //
   std::vector<int> vGames(Intervals);
   std::vector<double> vEloDiff(Intervals);
   std::vector<int> vWins(Intervals);
   std::vector<int> vDraws(Intervals);
   std::vector<int> vLosses(Intervals);
   std::vector<double> vExpectedWins(Intervals);
   std::vector<double> vExpectedDraws(Intervals);
   std::vector<double> vExpectedLosses(Intervals);
   std::vector<double> vExpectedElostat(Intervals);
#ifdef PLOTDIST
   std::vector<CDistribution> vLossDist(Intervals);
   std::vector<CDistribution> vDrawDist(Intervals);
   std::vector<CDistribution> vWinDist(Intervals);
#endif

   //
   // Initialization of data
   //
   for (int i = Intervals; --i >= 0;)
   {
    vGames[i] = 0;
    vEloDiff[i] = 0.0;
    vWins[i] = 0;
    vDraws[i] = 0;
    vLosses[i] = 0;
    vExpectedWins[i] = 0.0;
    vExpectedDraws[i] = 0.0;
    vExpectedLosses[i] = 0.0;
    vExpectedElostat[i] = 0.0;
   }

   //
   // Loop over games
   //
   for (int i = rsRes.GetGames(); --i >= 0;)
   {
    if (i % 100 == 0)
     std::cerr << i << " left    \r";
    double eloDiff = btRes.GetElo(rsRes.GetWhite(i)) -
                     btRes.GetElo(rsRes.GetBlack(i));
    double x = eloDiff + eloMaxDiff;
    if (x > 0)
    {
     int Index = int(Intervals * x / (2 * eloMaxDiff));
     if (Index < Intervals)
     {
      vGames[Index]++;
      vEloDiff[Index] += eloDiff;

      switch(rsRes.GetResult(i))
      {
       case 0: vLosses[Index]++; break;
       case 1: vDraws[Index]++; break;
       case 2: vWins[Index]++; break;
      }

      double PLoss = btRes.ResultProbability(eloDiff, 0);
      double PDraw = btRes.ResultProbability(eloDiff, 1);
      double PWin = btRes.ResultProbability(eloDiff, 2);
      double PElostat = btRes.ElostatProbability(eloDiff);

      vExpectedLosses[Index] += PLoss;
      vExpectedDraws[Index] += PDraw;
      vExpectedWins[Index] += PWin;
      vExpectedElostat[Index] += PElostat;

#ifdef PLOTDIST
      vLossDist[Index] = SumDistribution(vLossDist[Index],
                                         BinomialDistribution(PLoss));
      vDrawDist[Index] = SumDistribution(vDrawDist[Index],
                                         BinomialDistribution(PDraw));
      vWinDist[Index] = SumDistribution(vWinDist[Index],
                                        BinomialDistribution(PWin));
#endif
     }
    }
   }

   //
   // Print histogram data
   //
   for (int i = 0; i < Intervals; i++)
    if (vGames[i])
    {
     double g = vGames[i];
     out << vEloDiff[i] / g << ' ';
     out << vGames[i] << ' ';
     out << vLosses[i] / g << ' ';
     out << vDraws[i] / g << ' ';
     out << vWins[i] / g << ' ';
     out << (vWins[i] + 0.5 * vDraws[i]) / g << ' ';
     out << vExpectedLosses[i] / g << ' ';
     out << vExpectedDraws[i] / g << ' ';
     out << vExpectedWins[i] / g << ' ';
     out << (vExpectedWins[i] + 0.5 * vExpectedDraws[i]) / g << ' ';
     out << vExpectedElostat[i] / g << ' ';
#ifdef PLOTDIST
     out << vLossDist[i].GetLowerIndex(Confidence) / g << ' ';
     out << vDrawDist[i].GetLowerIndex(Confidence) / g << ' ';
     out << vWinDist[i] .GetLowerIndex(Confidence) / g << ' ';
     out << vLossDist[i].GetUpperIndex(Confidence) / g << ' ';
     out << vDrawDist[i].GetUpperIndex(Confidence) / g << ' ';
     out << vWinDist[i] .GetUpperIndex(Confidence) / g << ' ';
#endif
     out << '\n';
    }
  }
  break;

  case IDC_PlotDraw: ///////////////////////////////////////////////////////
  {
   int GroupSize = 100;
   double MaxDiff = 100;
   std::istringstream(pszParameters) >> GroupSize >> MaxDiff;
   
   //
   // Filter out games with a big rating difference
   //
   CResultSet rsFiltered;
   for (int i = rs.GetGames(); --i >= 0;)
   {
    double Diff = bt.GetElo(rs.GetWhite(i)) - bt.GetElo(rs.GetBlack(i));
    if (std::fabs(Diff) < MaxDiff) 
     rsFiltered.Append(rs.GetWhite(i), rs.GetBlack(i), rs.GetResult(i));
   }
   std::cerr << rsFiltered.GetGames() << " games left after filtering\n";

   //
   // Sort games according to average rating
   //
   std::vector<int> vOrder(rsFiltered.GetGames());
   std::vector<double> vAverage(rsFiltered.GetGames());
   {
    for (int i = rsFiltered.GetGames(); --i >= 0;)
    {
     vOrder[i] = i;
     vAverage[i] = bt.GetElo(rs.GetWhite(i)) + bt.GetElo(rs.GetBlack(i));
    }
    std::sort(vOrder.begin(), vOrder.end(), CIndirectCompare<double>(&vAverage[0]));
   }

   //
   // Number of draws for each group of GroupSize players, starting
   // from the top players
   //
   int Index = 0;
   for (int Group = rsFiltered.GetGames() / GroupSize; --Group >= 0;)
   {
    int Draws = 0;
    double TotalRating = 0;
    for (int i = GroupSize; --i >= 0;)
    {
     int g = vOrder[Index++];
     if (rsFiltered.GetResult(g) == 1)
      Draws++;
     TotalRating += vAverage[g];
    }
    CCDistribution cdist(Resolution, 0.0, 1.0);
    cdist.SetBinomialPosterior(GroupSize, Draws);
    out << TotalRating / (2 * GroupSize) << ' ';
    out << double(Draws) / double(GroupSize) << ' ';
    out << cdist.GetLowerValue(Confidence) << ' ';
    out << cdist.GetUpperValue(Confidence) << '\n';
   }
  }
  break;

  case IDC_Ratings: ////////////////////////////////////////////////////////
  {
   int MinGames = 1;
   std::string sFileName;
   int fFullRank = 0;

   std::istringstream(pszParameters) >> MinGames >> sFileName >> fFullRank;
   if (MinGames < 1)
    MinGames = 1;

   //
   // Read a list of player names
   //
   std::ifstream ifsNames(sFileName.c_str());
   std::set<std::string> setNames;
   while (1)
   {
    std::string s;
    ReadLineToString(s, ifsNames);
    if (ifsNames)
     setNames.insert(s);
    else
     break;
   }

   std::sort(vPermutation.begin(),
             vPermutation.end(),
             CIndirectCompare<double>(bt.GetElo()));

   int Width = MaxNameLength;
   if (Width < 4)
    Width = 4;

   CCondensedResults crsNoPrior(rs);

   std::ios::fmtflags f = out.flags();
   out.setf(std::ios::right, std::ios::adjustfield);
   out << std::setw(3) << "Rank" << ' ';
   out.setf(std::ios::left, std::ios::adjustfield);
   out << std::setw(Width) << "Name" << ' ';
   out.setf(std::ios::right, std::ios::adjustfield);
   out << std::setw(5) << "Elo" << ' ';
   out << std::setw(4) << "  +" << ' ';
   out << std::setw(4) << "  -" << ' ';
   out << std::setw(5) << "games" << ' ';
   out << std::setw(5) << "score" << ' ';
   out << std::setw(5) << "oppo." << ' ';
   out << std::setw(5) << "draws" << ' ';
   out << '\n';

   const double *pElo = bt.GetElo();
   for (int i = 0, Counter = 0; i < crs.GetPlayers(); i++)
   {
    int j = vPermutation[i];
    float Games = crsNoPrior.CountGames(j);
    if (Games >= MinGames &&
        (setNames.size() == 0 || setNames.find(vecName[j]) != setNames.end()))
    {
     Counter++;
     double Score = double(crsNoPrior.Score(j)) / 2;
     out.setf(std::ios::right, std::ios::adjustfield);
     if (fFullRank)
      out << std::setw(4) << i + 1 << ' ';
     else
      out << std::setw(4) << Counter << ' ';
     out.setf(std::ios::left, std::ios::adjustfield);
     out << std::setw(Width) << vecName[j] << ' ';
     out.setf(std::ios::right, std::ios::adjustfield);
     out << std::setw(5) << RoundDouble(EloScale * bt.GetElo(j) + eloOffset) << ' ';
     out << std::setw(4) << RoundDouble(EloScale * veloUpper[j]) << ' ';
     out << std::setw(4) << RoundDouble(EloScale * veloLower[j]) << ' ';
     out << std::setw(5) << Games << ' ';
     out << std::setw(4) << RoundDouble(100 * Score / Games) << "% ";
     out << std::setw(5) <<
      RoundDouble(EloScale * crs.AverageOpponent(j, pElo) + eloOffset) << ' ';
     out << std::setw(4) << RoundDouble(100 * crsNoPrior.CountDraws(j) /
                                        double(Games)) << "% ";
     out << '\n';
    }
   }

   out.flags(f);
  }
  break;

  case IDC_Details: ////////////////////////////////////////////////////////
  {
   std::sort(vPermutation.begin(),
             vPermutation.end(),
             CIndirectCompare<double>(bt.GetElo()));

   std::ios::fmtflags f = out.flags();
   int precision = out.precision();

   out.setf(std::ios::fixed, std::ios::floatfield);
   out.precision(1);

   int Width = MaxNameLength;
   if (Width < 4)
    Width = 4;

   CCondensedResults crsNoPrior(rs);

   for (int i = 0, Counter = 0; i < crs.GetPlayers(); i++)
   {
    int j = vPermutation[i];
    float Games = crsNoPrior.CountGames(j);
    Counter++;
    double Score = double(crsNoPrior.Score(j)) / 2;
    out.setf(std::ios::right, std::ios::adjustfield);
    out << std::setw(4) << Counter << ' ';
    out.setf(std::ios::left, std::ios::adjustfield);
    out << std::setw(Width) << vecName[j] << ' ';
    out.setf(std::ios::right, std::ios::adjustfield);
    out << std::setw(5) << RoundDouble(EloScale * bt.GetElo(j) + eloOffset) << ' ';
    out << std::setw(5) << Games << " (";
    out << std::setw(5) << Score << " : ";
    out << std::setw(5) << Games-Score << ")\n";

    //
    // Sort opponents
    //
    std::vector<int> vOpponentIndex(crsNoPrior.GetOpponents(j));
    std::vector<double> vOpponentElo(crsNoPrior.GetOpponents(j));
    for (int k = crsNoPrior.GetOpponents(j); --k >= 0;)
    {
     const CCondensedResult &cr = crsNoPrior.GetCondensedResult(j, k);
     vOpponentIndex[k] = k;
     vOpponentElo[k] = bt.GetElo(cr.Opponent);
    }
    std::sort(vOpponentIndex.begin(),
              vOpponentIndex.end(),
              CIndirectCompare<double>(&vOpponentElo[0]));

    //
    // Print opponent data
    //
    for (int k = 0; k < crsNoPrior.GetOpponents(j); k++)
    {
     int l = vOpponentIndex[k];
     const CCondensedResult &cr = crsNoPrior.GetCondensedResult(j, l);
     out.setf(std::ios::right, std::ios::adjustfield);
     out << std::setw(4) << ' ' << ' ';
     out.setf(std::ios::left, std::ios::adjustfield);
     out << std::setw(Width) << ' ' << ' ';
     out.setf(std::ios::right, std::ios::adjustfield);
     out << std::setw(5) << ' ' << ' ';
     out << std::setw(5) << cr.Games() << " (";
     out << std::setw(5) << cr.Score() << " : ";
     out << std::setw(5) << cr.Games()-cr.Score() << ") ";
     out.setf(std::ios::left, std::ios::adjustfield);
     out << std::setw(Width) << vecName[cr.Opponent] << ' ';
     out.setf(std::ios::right, std::ios::adjustfield);
     out << std::setw(5) << RoundDouble(EloScale * bt.GetElo(cr.Opponent) + eloOffset) << '\n';
    }
   }
   out.flags(f);
   out.precision(precision);
  }
  break;

  case IDC_Offset: /////////////////////////////////////////////////////////
  {
   std::istringstream iss(pszParameters);
   float elo = eloOffset;
   iss >> elo;

   std::string sName;
   iss >> sName; // ??? there must be a better way
   if (sName != "")
   {
    char c = 0;
    while (iss.get(c))
     sName += c;

    int fFound = 0;
    for (int i = 0; i < crs.GetPlayers(); i++)
     if (sName == vecName[i])
     {
      fFound = 1;
      eloOffset = elo - EloScale * bt.GetElo(i);
      break;
     }

    if (!fFound)
     out << "Error: unknow player " << sName << '\n';
   }
   else
    GetSet<double>(eloOffset, pszParameters, out);
  }
  break;

  case IDC_Scale: //////////////////////////////////////////////////////////
   GetSet<double>(EloScale, pszParameters, out);
  break;

  case IDC_MM: /////////////////////////////////////////////////////////////
  {
   int fThetaW = 0;
   int fThetaD = 0;
   std::istringstream(pszParameters) >> fThetaW >> fThetaD;
   CClockTimer timer;
   bt.MinorizationMaximization(fThetaW, fThetaD);
   out << timer.GetInterval() << '\n';
   ComputeVariance();
   {
    double x = std::pow(10.0, -bt.GetDrawElo() / 400);
    EloScale = x * 4.0 / ((1 + x) * (1 + x));
   }
   fLOSComputed = 0;
  }
  break;

  case IDC_ELOstat: ////////////////////////////////////////////////////////
  {
   crs.AddPrior(-Prior);
   CClockTimer timer;
   bt.ELOstat();
   bt.ELOstatIntervals(&veloLower[0], &veloUpper[0]);
   out << timer.GetInterval() << '\n';
   crs.AddPrior(Prior);
   EloScale = 1.0;
   fLOSComputed = 0;
  }
  break;

  case IDC_Elo: ////////////////////////////////////////////////////////////
  {
   std::istringstream is(pszParameters);
   unsigned Player = 0;
   is >> Player;
   if (Player < unsigned(crs.GetPlayers()))
   {
    double elo = bt.GetElo(Player);
    is >> elo;
    bt.SetElo(Player, elo);
    out << "Rating of player " << Player << " (" << vecName[Player];
    out << ") is " << elo << ".\n";
   }
   else
    out << "Player out of range (should be in 0-" <<
           crs.GetPlayers() - 1 << ")\n";
  }
  break;

  case IDC_JointDist: //////////////////////////////////////////////////////
  {
   int Player = -1;
   std::istringstream(pszParameters) >> Player;

   CClockTimer timer;
   CDistributionCollection dc(crs.GetPlayers(),
                              Resolution,
                              eloMin,
                              eloMax);
   CJointBayesian jb(rs, dc, bt);
   jb.RunComputation();
   out << timer.GetInterval() << '\n';
   for (int i = crs.GetPlayers(); --i >= 0;)
   {
    veloLower[i] = bt.GetElo(i) - dc.GetDistribution(i).
                                  GetLowerValue(Confidence);
    veloUpper[i] = dc.GetDistribution(i).
                   GetUpperValue(Confidence) - bt.GetElo(i);
    if (i == Player)
    {
     CCDistributionCUI cdcui(dc.GetDistribution(i), this);
     cdcui.MainLoop(in, out);
    }
   }
  }
  break;

  case IDC_ExactDist: //////////////////////////////////////////////////////
  {
   int Player = -1;
   std::istringstream(pszParameters) >> Player;

   CClockTimer timer;
   CCDistribution cdist(Resolution, eloMin, eloMax);

   for (int i = crs.GetPlayers(); --i >= 0;)
   {
    if (i % 10 == 0)
    {
     out << i << " left    \r";
     out.flush();
    }
    bt.GetPlayerDist(i, cdist);
    veloLower[i] = bt.GetElo(i) - cdist.GetLowerValue(Confidence);
    veloUpper[i] = cdist.GetUpperValue(Confidence) - bt.GetElo(i);
    if (i == Player)
    {
     CCDistributionCUI cdcui(cdist, this);
     cdcui.MainLoop(in, out);
    }
   }

   out << timer.GetInterval() << "              \n";
  }
  break;

  case IDC_Variance: ///////////////////////////////////////////////////////
   ComputeVariance();
  break;

  case IDC_Covariance: /////////////////////////////////////////////////////
  {
   bt.ComputeCovariance();
   bt.ComputeLikelihoodOfSuperiority();

   const CMatrix &mCovariance = bt.GetCovariance();
   CCDistribution cdist(1000, -10, 10);
   cdist.SetNormal(0, 1);
   double x = cdist.GetUpperValue(Confidence);
   for (int i = crs.GetPlayers(); --i >= 0;)
    veloLower[i] = veloUpper[i] = x * std::sqrt(mCovariance.GetElement(i, i));

   fLOSComputed = 1;
  }
  break;

  case IDC_LOS: ////////////////////////////////////////////////////////////
  {
   if (!fLOSComputed)
   {
    bt.ComputeCovariance();
    bt.ComputeLikelihoodOfSuperiority();
   }
   const CMatrix &mLOS = bt.GetLikelihoodOfSuperiority();

   //
   // Parse command parameters
   //
   int First = 0;
   int Players = mLOS.GetRows();
   int Width = 3;
   std::istringstream(pszParameters) >> First >> Players >> Width;
   if (Width < 2)
    Width = 2;
   if (First < 0)
    First = 0;
   if (First + Players > mLOS.GetRows())
    Players = mLOS.GetRows() - First;
   if (Players <= 0)
   {
    First = 0;
    Players = mLOS.GetRows();
   }
   const double Mult = std::pow(10.0, (Width > 8 ? 8 : Width) - 1);

   //
   // Sort players according to rating
   //
   std::sort(vPermutation.begin(),
             vPermutation.end(),
             CIndirectCompare<double>(bt.GetElo()));

   //
   // Column headers with Width-1 letters of each player
   //
   out << std::setw(MaxNameLength) << ' ' << ' ';
   for (int p = First; p < First + Players; p++)
   {
    int i = vPermutation[p];
    std::string s = vecName[i].substr(0, Width - 1);
    out << std::setw(Width) << s;
   }
   out << '\n';
   out.flush();

   //
   // Two nested loops to print the matrix
   //
   for (int p = First; p < First + Players; p++)
   {
    int i = vPermutation[p];
    out.setf(std::ios::left, std::ios::adjustfield);
    out << std::setw(MaxNameLength) << vecName[i] << ' ';
    out.setf(std::ios::right, std::ios::adjustfield);

    for (int q = First; q < First + Players; q++)
    {
     if (p != q)
     {
      int j = vPermutation[q];
      out << std::setw(Width) << int(mLOS.GetElement(i, j) * Mult);
     }
     else
      out << std::setw(Width) << ' ';
    }
    out << '\n';
   }
  }
  break;

  case IDC_AdvDist: ////////////////////////////////////////////////////////
  {
   CCDistribution cdist(Resolution, eloMin, eloMax);
   bt.GetAdvantageDist(cdist);
   out << cdist.GetLowerValue(Confidence);
   out << " <= " << cdist.GetMostLikelyValue();
   out << " <= " << cdist.GetUpperValue(Confidence) << '\n';
   CCDistributionCUI cdcui(cdist, this);
   cdcui.MainLoop(in, out);
  }
  break;

  case IDC_DrawDist: ///////////////////////////////////////////////////////
  {
   CCDistribution cdist(Resolution, eloMin < 0 ? 0 : eloMin, eloMax);
   bt.GetDrawEloDist(cdist);
   out << cdist.GetLowerValue(Confidence);
   out << " <= " << cdist.GetMostLikelyValue();
   out << " <= " << cdist.GetUpperValue(Confidence) << '\n';
   CCDistributionCUI cdcui(cdist, this);
   cdcui.MainLoop(in, out);
  }
  break;

  case IDC_PairStats: //////////////////////////////////////////////////////
  {
   //
   // Read command parameters and print player names
   //
   int i = 0;
   int j = 0;
   std::istringstream(pszParameters) >> i >> j;
   if (i < 0 || i >= crs.GetPlayers() ||
       j < 0 || j >= crs.GetPlayers() ||
       i == j)
    out << "error: bad player numbers\n";
   else
   {
    out << "-- pairstats between i = " << i << ", and j = " << j << '\n';
    out << "Name[i] = " << vecName[i] << '\n';
    out << "Name[j] = " << vecName[j] << '\n';
    out << "elo[i] - elo[j] = ";
    out << EloScale * (bt.GetElo(i) - bt.GetElo(j)) << '\n';
   }

   //
   // Build the set of results between these two players
   //
   CResultSet rsLocal(rs, i, j);
   out << "Games = " << rsLocal.GetGames() << '\n';
   if (rsLocal.GetGames())
   {
    CCondensedResults crsLocal(rsLocal);
    {
     CCondensedResult &cr = crsLocal.GetCondensedResult(0, 0);
     out << "w_ij = " << cr.w_ij << '\n';
     out << "d_ij = " << cr.d_ij << '\n';
     out << "l_ij = " << cr.l_ij << '\n';
     out << "w_ji = " << cr.w_ji << '\n';
     out << "d_ji = " << cr.d_ji << '\n';
     out << "l_ji = " << cr.l_ji << '\n';
     cr = crs.FindOpponent(i, j);
     cr.Opponent = 1;
    }
    {
     CCondensedResult &cr = crsLocal.GetCondensedResult(1, 0);
     cr = crs.FindOpponent(j, i);
     cr.Opponent = 0;
    }

    //
    // Compute and print elo ratings
    //
    CBradleyTerry btLocal(crsLocal);
    btLocal.SetAdvantage(bt.GetAdvantage());
    btLocal.SetDrawElo(bt.GetDrawElo());
    btLocal.MinorizationMaximization(0, 0);
    out << "elo[i] - elo[j] = ";
    out << EloScale * (btLocal.GetElo(0) - btLocal.GetElo(1)) << '\n';
   }
  }
  break;

  default: /////////////////////////////////////////////////////////////////
   return CConsoleUI::ProcessCommand(pszCommand, pszParameters, in, out);
 }

 return PC_Continue;
}
