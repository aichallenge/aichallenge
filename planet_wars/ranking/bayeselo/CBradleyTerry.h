/////////////////////////////////////////////////////////////////////////////
//
// Rémi Coulom
//
// December, 2004
//
/////////////////////////////////////////////////////////////////////////////
#ifndef CBradleyTerry_Declared
#define CBradleyTerry_Declared

class CResultSet;
class CCondensedResults;
class CCDistribution;
class CDistributionCollection;

#include "CMatrix.h"

#include <vector>
#include <cmath>

class CBradleyTerry // bt
{
 private: ///////////////////////////////////////////////////////////////////
  const CCondensedResults &crs;
  
  mutable std::vector<double> velo;
  mutable double eloAdvantage;
  mutable double eloDraw;

  mutable std::vector<double> v1;
  mutable std::vector<double> v2;
  mutable double *pGamma;
  mutable double *pNextGamma;
  mutable double ThetaW;
  mutable double ThetaD;

  CMatrix mCovariance;
  CMatrix mLOS;
  CMatrix mWinProbability;
  CMatrix mLossProbability;
  CMatrix mDrawProbability;

  void ConvertEloToGamma() const;
  void UpdateGammas();
  double UpdateThetaW();
  double UpdateThetaD();
  double GetDifference(int n, const double *pd1, const double *pd2);

 public: ////////////////////////////////////////////////////////////////////
  CBradleyTerry(const CCondensedResults &crsInit);

  //
  // Gets
  //
  const double *GetElo() const {return &velo[0];}
  double GetElo(int i) const {return velo[i];}
  double GetAdvantage() const {return eloAdvantage;}
  double GetDrawElo() const {return eloDraw;}

  //
  // Sets
  //
  double *GetElo() {return &velo[0];}
  void SetElo(int i, double x) {velo[i] = x;}
  void SetAdvantage(double x) {eloAdvantage = x;}
  void SetDrawElo(double x) {eloDraw = x;}

  //
  // Methods to compute elo ratings
  //
  void MinorizationMaximization(int fThetaW,
                                int fThetaD,
                                double Epsilon = 1e-5);
  void ELOstat(double Epsilon = 1e-5);
  void ELOstatIntervals(double *peloLower, double *peloUpper) const;
 
  //
  // Basic Probabilities
  //
  static double Probability(double eloDelta)
  {
   return 1 / (1 + std::pow(10.0, eloDelta / 400.0));
  }

  double ElostatProbability(double eloDelta) const
  {
   return Probability(-eloDelta - eloAdvantage);
  }

  double WinProbability(double eloDelta) const
  {
   return Probability(-eloDelta - eloAdvantage + eloDraw);
  }

  double LossProbability(double eloDelta) const
  {
   return Probability(eloDelta + eloAdvantage + eloDraw);
  }

  double DrawProbability(double eloDelta) const
  {
   return 1 - WinProbability(eloDelta) - LossProbability(eloDelta);
  }

  double ResultProbability(double eloDelta, int Result) const
  {
   if (Result == 0)
    return LossProbability(eloDelta);
   else if (Result == 1)
    return DrawProbability(eloDelta);
   else
    return WinProbability(eloDelta);
  }

  //
  // Likelihood of results
  //
  double LogLikelihood(int Player) const;
  double LogLikelihood() const;
  double LogLikelihood(const CResultSet &rs) const;

  //
  // Likelihood distributions
  //
  void GetPlayerDist(int Player, CCDistribution &cdist) const;
  void GetAdvantageDist(CCDistribution &cdist) const;
  void GetDrawEloDist(CCDistribution &cdist) const;

  void GetVariance(double *pdVariance) const;
  void ComputeCovariance();
  void ComputeLikelihoodOfSuperiority();

  const CMatrix &GetCovariance() const {return mCovariance;}
  const CMatrix &GetLikelihoodOfSuperiority() const {return mLOS;}
};

#endif
