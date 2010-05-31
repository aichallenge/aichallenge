/////////////////////////////////////////////////////////////////////////////
//
// Rémi Coulom
//
// December, 2004
//
/////////////////////////////////////////////////////////////////////////////
#include "CBradleyTerry.h"
#include "CCondensedResults.h"
#include "CResultSet.h"
#include "CCDistribution.h"
#include "CMatrix.h"
#include "CLUDecomposition.h"
#include "random.h"

#include <cmath>

#include <iostream>
#include "CMatrixIO.h"

/////////////////////////////////////////////////////////////////////////////
// One iteration of the MM algorithm on gammas
/////////////////////////////////////////////////////////////////////////////
void CBradleyTerry::UpdateGammas()
{
 //
 // Loop over players
 //
 for (int Player = crs.GetPlayers(); --Player >= 0;)
 {
  double A = 0;
  double B = 0;
  for (int j = crs.GetOpponents(Player); --j >= 0;)
  {
   const CCondensedResult &cr = crs.GetCondensedResult(Player, j);

   double OpponentGamma;
   if (cr.Opponent > Player)
    OpponentGamma = pNextGamma[cr.Opponent];
   else
    OpponentGamma = pGamma[cr.Opponent];

   A += cr.w_ij + cr.d_ij + cr.l_ji + cr.d_ji;
   B += (cr.d_ij + cr.w_ij) * ThetaW /
        (ThetaW * pGamma[Player] + ThetaD * OpponentGamma) +
        (cr.d_ij + cr.l_ij) * ThetaD * ThetaW /
        (ThetaD * ThetaW * pGamma[Player] + OpponentGamma) +
        (cr.d_ji + cr.w_ji) * ThetaD /
        (ThetaW * OpponentGamma + ThetaD * pGamma[Player]) +
        (cr.d_ji + cr.l_ji) / 
        (ThetaD * ThetaW * OpponentGamma + pGamma[Player]);
  }
  pNextGamma[Player] = A / B;
 }

 //
 // Swap buffers to prepare next iteration
 //
 {
  double *pTemp = pGamma;
  pGamma = pNextGamma;
  pNextGamma = pTemp;
 }
}

/////////////////////////////////////////////////////////////////////////////
// MM on ThetaW
/////////////////////////////////////////////////////////////////////////////
double CBradleyTerry::UpdateThetaW()
{
 double Numerator = 0;
 double Denominator = 0;

 for (int Player = crs.GetPlayers(); --Player >= 0;)
  for (int j = crs.GetOpponents(Player); --j >= 0;)
  {
   const CCondensedResult &cr = crs.GetCondensedResult(Player, j);
   double OpponentGamma = pGamma[cr.Opponent];

   Numerator += cr.w_ij + cr.d_ij;
   Denominator += (cr.d_ij + cr.w_ij) * pGamma[Player] /
                  (ThetaW * pGamma[Player] + ThetaD * OpponentGamma) +
                  (cr.d_ij + cr.l_ij) * ThetaD * pGamma[Player] /
                  (ThetaD * ThetaW * pGamma[Player] + OpponentGamma);
  }

 return Numerator / Denominator;
}

/////////////////////////////////////////////////////////////////////////////
// MM on ThetaD
/////////////////////////////////////////////////////////////////////////////
double CBradleyTerry::UpdateThetaD()
{
 double Numerator = 0;
 double Denominator = 0;

 for (int Player = crs.GetPlayers(); --Player >= 0;)
  for (int j = crs.GetOpponents(Player); --j >= 0;)
  {
   const CCondensedResult &cr = crs.GetCondensedResult(Player, j);
   double OpponentGamma = pGamma[cr.Opponent];

   Numerator += cr.d_ij;
   Denominator += (cr.d_ij + cr.w_ij) * OpponentGamma /
                  (ThetaW * pGamma[Player] + ThetaD * OpponentGamma) +
                  (cr.d_ij + cr.l_ij) * ThetaW * pGamma[Player] /
                  (ThetaD * ThetaW * pGamma[Player] + OpponentGamma);
  }

 double C = Numerator / Denominator;

 return C + std::sqrt(C * C + 1);
}

/////////////////////////////////////////////////////////////////////////////
// Compute the maximum relative difference between two iterations
/////////////////////////////////////////////////////////////////////////////
double CBradleyTerry::GetDifference(int n,
                                    const double *pd1,
                                    const double *pd2)
{
 double Result = 0;
 for (int i = n; --i >= 0;)
 {
  double Diff = std::fabs(pd1[i] - pd2[i]) / (pd1[i] + pd2[i]);
  if (Diff > Result)
   Result = Diff;
 }
 return Result;
}

/////////////////////////////////////////////////////////////////////////////
// Constructor
/////////////////////////////////////////////////////////////////////////////
CBradleyTerry::CBradleyTerry(const CCondensedResults &crsInit):
 crs(crsInit),
 velo(crs.GetPlayers()),
 eloAdvantage(0),//32.8),
 eloDraw(97.3),
 v1(crs.GetPlayers()),
 v2(crs.GetPlayers()),
 pGamma(&v1[0]),
 pNextGamma(&v2[0])
{
}

/////////////////////////////////////////////////////////////////////////////
// Convert Elos to gammas and Thetas
/////////////////////////////////////////////////////////////////////////////
void CBradleyTerry::ConvertEloToGamma() const
{
 ThetaW = std::pow(10.0, eloAdvantage/400.0);
 ThetaD = std::pow(10.0, eloDraw/400.0);
 for (int i = crs.GetPlayers(); --i >= 0;)
  pGamma[i] = std::pow(10.0, velo[i]/400.0);
}

/////////////////////////////////////////////////////////////////////////////
// MM Algorithm
/////////////////////////////////////////////////////////////////////////////
void CBradleyTerry::MinorizationMaximization(int fThetaW,
                                             int fThetaD,
                                             double Epsilon)
{
 //
 // Set initial values
 //
 ThetaW = fThetaW ? 1.0 : std::pow(10.0, eloAdvantage/400.0);
 ThetaD = fThetaD ? 1.0 : std::pow(10.0, eloDraw/400.0);
 for (int i = crs.GetPlayers(); --i >= 0;)
  pGamma[i] = 1.0;

 //
 // Main MM loop
 //
 for (int i = 0; i < 10000; i++)
 {
  UpdateGammas();
  double Diff = GetDifference(crs.GetPlayers(), pGamma, pNextGamma);

  if (fThetaW)
  {
   double NewThetaW = UpdateThetaW();
   double ThetaW_Diff = std::fabs(ThetaW - NewThetaW);
   if (ThetaW_Diff > Diff)
    Diff = ThetaW_Diff;
   ThetaW = NewThetaW;
  }

  if (fThetaD)
  {
   double NewThetaD = UpdateThetaD();
   double ThetaD_Diff = std::fabs(ThetaD - NewThetaD);
   if (ThetaD_Diff > Diff)
    Diff = ThetaD_Diff;
   ThetaD = NewThetaD;
  }

  if (Diff < Epsilon)
   break;

  //
  // Print iteration information
  //
#if 1
  if ((i + 1) % 100 == 0)
  {
   std::cout << "Iteration " << i + 1 << ": ";
   std::cout << Diff << ' ';
   std::cout << '\n';
   std::cout.flush();
  }
#endif
 }
 
 //
 // Convert back to Elos
 //
 {
  double Total = 0;
  for (int i = crs.GetPlayers(); --i >= 0;)
   Total += (velo[i] = std::log10(pGamma[i]) * 400);
  double Offset = - Total / crs.GetPlayers();
  for (int i = crs.GetPlayers(); --i >= 0;)
   velo[i] += Offset;
 }

 if (fThetaW)
  eloAdvantage = std::log10(ThetaW) * 400;

 if (fThetaD)
  eloDraw = std::log10(ThetaD) * 400;
}

/////////////////////////////////////////////////////////////////////////////
// Get the likelihood of the results of a player
/////////////////////////////////////////////////////////////////////////////
double CBradleyTerry::LogLikelihood(int Player) const
{
 double Result = 0;
 for (int i = crs.GetOpponents(Player); --i >= 0;)
 {
  const CCondensedResult &cr = crs.GetCondensedResult(Player, i);
  double Delta = velo[Player] - velo[cr.Opponent];
  if (cr.w_ij)
   Result += cr.w_ij * std::log(WinProbability(Delta));
  if (cr.d_ij)
   Result += cr.d_ij * std::log(DrawProbability(Delta));
  if (cr.l_ij)
   Result += cr.l_ij * std::log(LossProbability(Delta));
  if (cr.w_ji)
   Result += cr.w_ji * std::log(WinProbability(-Delta));
  if (cr.d_ji)
   Result += cr.d_ji * std::log(DrawProbability(-Delta));
  if (cr.l_ji)
   Result += cr.l_ji * std::log(LossProbability(-Delta));
 }
 return Result;
}

/////////////////////////////////////////////////////////////////////////////
// Get the likelihood of a result set
/////////////////////////////////////////////////////////////////////////////
double CBradleyTerry::LogLikelihood(const CResultSet &rs) const
{
 double Result = 0;

 for (int i = rs.GetGames(); --i >= 0;)
 {
  double Delta = velo[rs.GetWhite(i)] - velo[rs.GetBlack(i)];
  switch(rs.GetResult(i))
  {
   case 0: Result += std::log(LossProbability(Delta)); break;
   case 1: Result += std::log(DrawProbability(Delta)); break;
   case 2: Result += std::log(WinProbability(Delta)); break;
  }
 }

 return Result;
}

/////////////////////////////////////////////////////////////////////////////
// Get the likelihood of the full set of results
/////////////////////////////////////////////////////////////////////////////
double CBradleyTerry::LogLikelihood() const
{
 double Result = 0;
 for (int Player = crs.GetPlayers(); --Player >= 0;)
  for (int i = crs.GetOpponents(Player); --i >= 0;)
  {
   const CCondensedResult &cr = crs.GetCondensedResult(Player, i);
   double Delta = velo[Player] - velo[cr.Opponent];
   if (cr.w_ij)
    Result += cr.w_ij * std::log(WinProbability(Delta));
   if (cr.d_ij)
    Result += cr.d_ij * std::log(DrawProbability(Delta));
   if (cr.l_ij)
    Result += cr.l_ij * std::log(LossProbability(Delta));
  }
 return Result;
}

/////////////////////////////////////////////////////////////////////////////
// Get the likelihood distribution of one player
// The ratings of opponents are supposed to be exact
/////////////////////////////////////////////////////////////////////////////
void CBradleyTerry::GetPlayerDist(int Player, CCDistribution &cdist) const
{
 std::vector<double> veloBackup(velo);

 for (int i = cdist.GetSize(); --i >= 0;)
 {
  velo[Player] = cdist.ValueFromIndex(i);
  if (crs.GetPlayers() > 1)
  {
   double Delta = (velo[Player] - veloBackup[Player]) / (crs.GetPlayers() - 1);
   for (int j = crs.GetPlayers(); --j >= 0;)
    if (j != Player)
     velo[j] = veloBackup[j] - Delta;
  }
  cdist.SetProbability(i, LogLikelihood(Player));
 }
 cdist.LogNormalize(); 

 velo = veloBackup;
}

/////////////////////////////////////////////////////////////////////////////
// Likelihood distribution of the eloAdvantage parameter
/////////////////////////////////////////////////////////////////////////////
void CBradleyTerry::GetAdvantageDist(CCDistribution &cdist) const
{
 double eloPrevious = eloAdvantage;
 for (int i = cdist.GetSize(); --i >= 0;)
 {
  eloAdvantage = cdist.ValueFromIndex(i);
  cdist.SetProbability(i, LogLikelihood());
 }
 cdist.LogNormalize();
 eloAdvantage = eloPrevious;
}

/////////////////////////////////////////////////////////////////////////////
// Likelihood distribution of the eloDraw parameter
/////////////////////////////////////////////////////////////////////////////
void CBradleyTerry::GetDrawEloDist(CCDistribution &cdist) const
{
 double eloPrevious = eloDraw;
 for (int i = cdist.GetSize(); --i >= 0;)
 {
  eloDraw = cdist.ValueFromIndex(i);
  cdist.SetProbability(i, LogLikelihood());
 }
 cdist.LogNormalize();
 eloDraw = eloPrevious;
}

/////////////////////////////////////////////////////////////////////////////
// Compute variance with the diagonal of the Hessian
/////////////////////////////////////////////////////////////////////////////
void CBradleyTerry::GetVariance(double *pdVariance) const
{
 ConvertEloToGamma();

 const double x = std::log(10.0) / 400;
 const double xx = x * x;

 for (int Player = crs.GetPlayers(); --Player >= 0;)
 {
  double Diag = 0;
  double PlayerGamma = pGamma[Player];

  for (int j = crs.GetOpponents(Player); --j >= 0;)
  {
   const CCondensedResult &cr = crs.GetCondensedResult(Player, j);
   double OpponentGamma = pGamma[cr.Opponent];
 
   double h = 0;

   {
    double d = ThetaW * PlayerGamma + ThetaD * OpponentGamma;
    h += (cr.w_ij + cr.d_ij) / (d * d);
   }
   {
    double d = ThetaD * ThetaW * PlayerGamma + OpponentGamma;
    h += (cr.l_ij + cr.d_ij) / (d * d);
   }
   {
    double d = ThetaW * OpponentGamma + ThetaD * PlayerGamma;
    h += (cr.w_ji + cr.d_ji) / (d * d);
   }
   {
    double d = ThetaD * ThetaW * OpponentGamma + PlayerGamma;
    h += (cr.l_ji + cr.d_ji) / (d * d);
   }

   h *= PlayerGamma * OpponentGamma * ThetaD * ThetaW;
   Diag -= h;
  }

  pdVariance[Player] = -1.0 / (Diag * xx);
 }
}

/////////////////////////////////////////////////////////////////////////////
// Compute the covariance matrix
// This function assumes that ratings are maximum-likelihood ratings
/////////////////////////////////////////////////////////////////////////////
void CBradleyTerry::ComputeCovariance()
{
 //
 // Compute the truncated opposite of the Hessian
 //
 CMatrix mTruncatedHessian(crs.GetPlayers() - 1, crs.GetPlayers() - 1);
 {
  ConvertEloToGamma();

  const double x = std::log(10.0) / 400;
  const double xx = -x * x;

  mTruncatedHessian.Zero();

  for (int Player = crs.GetPlayers() - 1; --Player >= 0;)
  {
   double Diag = 0;
   double PlayerGamma = pGamma[Player];

   for (int j = crs.GetOpponents(Player); --j >= 0;)
   {
    const CCondensedResult &cr = crs.GetCondensedResult(Player, j);
    double OpponentGamma = pGamma[cr.Opponent];
  
    double h = 0;

    {
     double d = ThetaW * PlayerGamma + ThetaD * OpponentGamma;
     h += (cr.w_ij + cr.d_ij) / (d * d);
    }
    {
     double d = ThetaD * ThetaW * PlayerGamma + OpponentGamma;
     h += (cr.l_ij + cr.d_ij) / (d * d);
    }
    {
     double d = ThetaW * OpponentGamma + ThetaD * PlayerGamma;
     h += (cr.w_ji + cr.d_ji) / (d * d);
    }
    {
     double d = ThetaD * ThetaW * OpponentGamma + PlayerGamma;
     h += (cr.l_ji + cr.d_ji) / (d * d);
    }

    h *= PlayerGamma * OpponentGamma * ThetaD * ThetaW;
    Diag -= h;
    if (cr.Opponent != crs.GetPlayers() - 1)
     mTruncatedHessian.SetElement(Player, cr.Opponent, h * xx);
   }

   mTruncatedHessian.SetElement(Player, Player, Diag * xx);
  }
 }

 //
 // LU-Decompose it
 //
 CLUDecomposition lud(crs.GetPlayers() - 1);
 std::vector<int> vIndex(crs.GetPlayers() - 1);
 lud.Decompose(mTruncatedHessian, &vIndex[0]);

 //
 // Fill A
 //
 CMatrix mA(crs.GetPlayers(), crs.GetPlayers() - 1);
 {
  double x = -1.0 / crs.GetPlayers();
  for (int i = crs.GetPlayers() * (crs.GetPlayers() - 1); --i >= 0;)
   mA[i] = x;
  for (int i = crs.GetPlayers() - 1; --i >= 0;)
   mA.SetElement(i, i, 1.0 + x);
 }

 //
 // Compute AC
 //
 CMatrix mAC(crs.GetPlayers(), crs.GetPlayers() - 1);
 for (int i = crs.GetPlayers(); --i >= 0;)
 {
  int Index = i * (crs.GetPlayers() - 1);
  lud.Solve(mTruncatedHessian, &vIndex[0], mA + Index, mAC + Index);
 }

 //
 // Compute the covariance
 //
 mCovariance.SetProductByTranspose(mAC, mA);
}

/////////////////////////////////////////////////////////////////////////////
// Compute the likelihood of superiority
// This function assumes that the covariance matrix and ratings are computed
/////////////////////////////////////////////////////////////////////////////
void CBradleyTerry::ComputeLikelihoodOfSuperiority()
{
 mLOS.SetSize(crs.GetPlayers(), crs.GetPlayers());

 for (int i = mLOS.GetRows(); --i > 0;)
  for (int j = i; --j >= 0;)
  {
   double Sigma2 = mCovariance.GetElement(i, i) +
                   mCovariance.GetElement(j, j) -
                   mCovariance.GetElement(i, j) -
                   mCovariance.GetElement(j, i);
   double Mu = velo[j] - velo[i];
   double x = Mu / std::sqrt(2 * Sigma2);
   mLOS.SetElement(i, j, erfc(x) / 2);
   mLOS.SetElement(j, i, erfc(-x) / 2);
  }

 for (int i = mLOS.GetRows(); --i >= 0;)
  mLOS.SetElement(i, i, 0.0);
}

/////////////////////////////////////////////////////////////////////////////
// ELOstat bounding of probabilities
/////////////////////////////////////////////////////////////////////////////
static double ELOstatBound(double p)
{
 static const double MaxP = 1 / (1 + std::pow(10.0, -600 / double(400)));
 static const double MinP = 1 / (1 + std::pow(10.0, 600 / double(400)));

 if (p > MaxP)
  p = MaxP;
 if (p < MinP)
  p = MinP;

 return p;
}

/////////////////////////////////////////////////////////////////////////////
// ELOstat algorithm
/////////////////////////////////////////////////////////////////////////////
void CBradleyTerry::ELOstat(double Epsilon)
{
 double *pElo = &v1[0];
 double *pNextElo = &v2[0];
 for (int i = crs.GetPlayers(); --i >= 0;)
  pElo[i] = 0;

 for (int i = 0; i < 10000; i++)
 {
  //
  // Start by copying ratings
  //
  for (int j = crs.GetPlayers(); --j >= 0;)
   pNextElo[j] = pElo[j];

  //
  // Loop over players to compute their new ratings
  //
  double TotalElo = 0;
  double GrandTotalGames = 0;
  for (int j = crs.GetPlayers(); --j >= 0;)
  {
   double TotalGames = 0;
   double TotalScore = 0;
   double TotalOpponentElo = 0;

   //
   // Loop over opponents
   //
   for (int k = crs.GetOpponents(j); --k >= 0;)
   {
    const CCondensedResult &cr = crs.GetCondensedResult(j, k);
    double Games = cr.Games();
    TotalGames += Games;
    TotalScore += cr.Score();
    TotalOpponentElo += Games * pElo[cr.Opponent];
   }

   double p = ELOstatBound(TotalScore / TotalGames);
   double Delta = 400 * std::log10(p / (1 - p));
   pNextElo[j] = TotalOpponentElo / TotalGames + Delta;
   TotalElo += TotalGames * pNextElo[j];
   GrandTotalGames += TotalGames;
  }

  //
  // Normalize and swap
  //
  for (int j = crs.GetPlayers(); --j >= 0;)
   pNextElo[j] -= TotalElo / GrandTotalGames;
  {
   double *pd = pElo;
   pElo = pNextElo;
   pNextElo = pd;
  }

  //
  // Compute difference
  //
  double MaxDiff = 0;
  for (int j = crs.GetPlayers(); --j >= 0;)
  {
   double Diff = std::fabs(pNextElo[j] - pElo[j]);
   if (Diff > MaxDiff)
    MaxDiff = Diff;
  }
  if (MaxDiff < Epsilon)
  {
   std::cout << i + 1 << " iterations\n";
   break;
  }
 }

 //
 // Store computed Elo ratings into vElo
 //
 for (int i = crs.GetPlayers(); --i >= 0;)
  velo[i] = pElo[i];
}

/////////////////////////////////////////////////////////////////////////////
// ELOstat way of computing confidence intervals
/////////////////////////////////////////////////////////////////////////////
void CBradleyTerry::ELOstatIntervals(double *peloLower,
                                     double *peloUpper) const
{
 //
 // Loop over players to compute confidence intervals
 //
 for (int j = crs.GetPlayers(); --j >= 0;)
 {
  //
  // Loop over opponents
  //
  double TotalGames = 0;
  double TotalScore = 0;
  for (int k = crs.GetOpponents(j); --k >= 0;)
  {
   const CCondensedResult &cr = crs.GetCondensedResult(j, k);
   TotalGames += cr.Games();
   TotalScore += cr.Score();
  }
  double MeanScore = TotalScore / TotalGames;

  //
  // Loop again for variance
  //
  double TotalVariance = 0;
  for (int k = crs.GetOpponents(j); --k >= 0;)
  {
   const CCondensedResult &cr = crs.GetCondensedResult(j, k);
   TotalVariance += (cr.w_ij + cr.l_ji)*(1 - MeanScore)*(1 - MeanScore) +
                    (cr.d_ij + cr.d_ji)*(0.5 - MeanScore)*(0.5 - MeanScore) +
                    (cr.w_ji + cr.l_ij)*(0 - MeanScore)*(0 - MeanScore);
  }
  if (TotalVariance < 0)
   TotalVariance = 0;
  double Variance = TotalVariance / (TotalGames * TotalGames);
  double StandardDeviation = std::sqrt(Variance);
  double pLower = ELOstatBound(MeanScore - 1.95996 * StandardDeviation);
  double pUpper = ELOstatBound(MeanScore + 1.95996 * StandardDeviation);
  double p = ELOstatBound(MeanScore);
  double DeltaLower = 400 * std::log10(pLower / (1 - pLower));
  double DeltaUpper = 400 * std::log10(pUpper / (1 - pUpper));
  double Delta = 400 * std::log10(p / (1 - p));
  peloLower[j] = Delta - DeltaLower;
  peloUpper[j] = DeltaUpper - Delta;
 }
}
