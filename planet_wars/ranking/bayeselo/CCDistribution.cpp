/////////////////////////////////////////////////////////////////////////////
//
// Rémi Coulom
//
// December, 2004
//
/////////////////////////////////////////////////////////////////////////////
#include "CCDistribution.h"

#include <cmath>

/////////////////////////////////////////////////////////////////////////////
// Get Bound helper function
/////////////////////////////////////////////////////////////////////////////
double CCDistribution::GetBoundValue(double Confidence,
                                     int Begin,
                                     int End,
                                     int Direction) const
{
 const double Threshold = (1 - Confidence) / 2;
 double P = 0;
 double PreviousDistribution = 0;

 for (int i = Begin; i != End; i += Direction)
 {
  double NewDistribution = vDistribution[i];
  double NewP = P + (PreviousDistribution + NewDistribution) / 2;
  if (NewP >= Threshold)
  {
   double Value = ValueFromIndex(i - Direction);
   double NewValue = ValueFromIndex(i);
#if 0
   double Delta = PreviousDistribution * PreviousDistribution - 
        4 * (P - Threshold) * (NewDistribution - PreviousDistribution) / 2;
   double Alpha = (-PreviousDistribution + std::sqrt(Delta)) / 
                  (NewDistribution - PreviousDistribution);
   return Value + (NewValue - Value) * Alpha;
#else
   // The second-order approximation above causes problems when NewD=PreviousD
   return Value + (NewValue - Value) * (Threshold - P) / (NewP - P);
#endif
  }
  P = NewP;
  PreviousDistribution = NewDistribution;
 }
 
 return ValueFromIndex(End - Direction);
}

/////////////////////////////////////////////////////////////////////////////
// Get Lower Bound
/////////////////////////////////////////////////////////////////////////////
double CCDistribution::GetLowerValue(double Confidence) const
{
 return GetBoundValue(Confidence, 0, vDistribution.size(), +1);
}

/////////////////////////////////////////////////////////////////////////////
// Get Upper Bound
/////////////////////////////////////////////////////////////////////////////
double CCDistribution::GetUpperValue(double Confidence) const
{
 return GetBoundValue(Confidence, vDistribution.size() - 1, -1, -1);
}

/////////////////////////////////////////////////////////////////////////////
// Get Most Likely Value
/////////////////////////////////////////////////////////////////////////////
double CCDistribution::GetMostLikelyValue() const
{
 double MaxP = vDistribution[0];
 int iMax = 0;
 for (int i = vDistribution.size(); --i > 0;)
  if (vDistribution[i] > MaxP)
  {
   iMax = i;
   MaxP = vDistribution[i];
  }
 return ValueFromIndex(iMax);
}

/////////////////////////////////////////////////////////////////////////////
// Gaussian
/////////////////////////////////////////////////////////////////////////////
void CCDistribution::SetNormal(double Mu, double Sigma)
{
 const double b = 1.0 / (2.0 * Sigma * Sigma);

 for (int i = vDistribution.size(); --i > 0;)
 {
  double x = ValueFromIndex(i);
  double delta = x - Mu;
  vDistribution[i] = std::exp(-b * delta * delta);
 }
 
 Normalize();
}

/////////////////////////////////////////////////////////////////////////////
// Binomial posterior ??? log-likelihood for better stability
/////////////////////////////////////////////////////////////////////////////
void CCDistribution::SetBinomialPosterior(int N, int n)
{
 for (int i = vDistribution.size(); --i > 0;)
 {
  double x = ValueFromIndex(i);
  vDistribution[i] = std::pow(x, n) * std::pow (1 - x, N - n);
 }
 Normalize();
}
