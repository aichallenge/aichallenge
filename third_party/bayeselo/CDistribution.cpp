/////////////////////////////////////////////////////////////////////////////
//
// Rémi Coulom
//
// December, 2004
//
/////////////////////////////////////////////////////////////////////////////
#include "CDistribution.h"

#include <cmath>

/////////////////////////////////////////////////////////////////////////////
// reset to zero
/////////////////////////////////////////////////////////////////////////////
void CDistribution::Reset()
{
 for (int i = vDistribution.size(); --i >= 0;)
  vDistribution[i] = 0;
}

/////////////////////////////////////////////////////////////////////////////
// set to uniform distribution
/////////////////////////////////////////////////////////////////////////////
void CDistribution::SetUniform()
{
 for (int i = vDistribution.size(); --i >= 0;)
  vDistribution[i] = 1;
}

/////////////////////////////////////////////////////////////////////////////
// normalize
/////////////////////////////////////////////////////////////////////////////
void CDistribution::Normalize()
{
 double Total = 0;
 for (int i = vDistribution.size(); --i >= 0;)
  Total += vDistribution[i];
 if (Total > 0)
  for (int i = vDistribution.size(); --i >= 0;)
   vDistribution[i] /= Total;
}

/////////////////////////////////////////////////////////////////////////////
// Normalize, assuming the distribution contains log-likelihoods
/////////////////////////////////////////////////////////////////////////////
void CDistribution::LogNormalize()
{
 double Max = vDistribution[0];
 for (int i = vDistribution.size(); --i > 0;)
  if (vDistribution[i] > Max)
   Max = vDistribution[i];

 for (int i = vDistribution.size(); --i >= 0;)
  vDistribution[i] = std::exp(vDistribution[i] - Max);

 Normalize();
}

/////////////////////////////////////////////////////////////////////////////
// Discrete GetBound
/////////////////////////////////////////////////////////////////////////////
int CDistribution::GetBoundIndex(double Confidence,
                                 int Begin,
                                 int End,
                                 int Direction) const
{
 const double Threshold = (1 - Confidence) / 2;
 double P = 0;

 for (int i = Begin; i != End; i += Direction)
 {
  P += vDistribution[i];
  if (P > Threshold)
   return i;
 }

 return End;
}

/////////////////////////////////////////////////////////////////////////////
// Discrete lower bound
/////////////////////////////////////////////////////////////////////////////
int CDistribution::GetLowerIndex(double Confidence) const
{
 return GetBoundIndex(Confidence, 0, vDistribution.size(), +1);
}

/////////////////////////////////////////////////////////////////////////////
// Discrete upper bound
/////////////////////////////////////////////////////////////////////////////
int CDistribution::GetUpperIndex(double Confidence) const
{
 return GetBoundIndex(Confidence, vDistribution.size() - 1, -1, -1);
}

/////////////////////////////////////////////////////////////////////////////
// Binomial distribution
/////////////////////////////////////////////////////////////////////////////
CDistribution BinomialDistribution(double p)
{
 CDistribution distResult(2);
 distResult.vDistribution[0] = 1 - p;
 distResult.vDistribution[1] = p;
 return distResult;
}

/////////////////////////////////////////////////////////////////////////////
// Distribution of the sum
/////////////////////////////////////////////////////////////////////////////
CDistribution SumDistribution(const CDistribution &dist1,
                              const CDistribution &dist2)
{
 CDistribution distResult(dist1.GetSize() + dist2.GetSize() - 1);

 distResult.Reset();

 for (int i1 = dist1.GetSize(); --i1 >= 0;)
  for (int i2 = dist2.GetSize(); --i2 >= 0;)
   distResult.vDistribution[i1 + i2] += dist1.vDistribution[i1] *
                                        dist2.vDistribution[i2];

 return distResult;
}
