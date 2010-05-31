/////////////////////////////////////////////////////////////////////////////
//
// Rémi Coulom
//
// December, 2004
//
/////////////////////////////////////////////////////////////////////////////
#include "CJointBayesian.h"
#include "CResultSet.h"
#include "CCDistribution.h"
#include "CDistributionCollection.h"
#include "CBradleyTerry.h"

#include <iostream>

/////////////////////////////////////////////////////////////////////////////
// Constructor
/////////////////////////////////////////////////////////////////////////////
CJointBayesian::CJointBayesian(const CResultSet &rsInit,
                               CDistributionCollection &dcInit,
                               const CBradleyTerry &btInit) :
 rs(rsInit),
 dc(dcInit),
 bt(btInit),
 indexMax(dc.GetDiscretizationSize() - 1)
{
 pindex = new int[rs.GetPlayers()];

 //
 // Pre-compute probability cache
 //
 pProbabilityCache = new double[3 * (indexMax * 2 + 1)];
 for (int Result = 3; --Result >= 0;)
  for (int i = indexMax * 2 + 1; --i >= 0;)
   pProbabilityCache[i + Result * (indexMax * 2 + 1)] =
    bt.ResultProbability(dc.ValueFromIndex(i) - dc.ValueFromIndex(indexMax),
                         Result);
}

/////////////////////////////////////////////////////////////////////////////
// Destructor
/////////////////////////////////////////////////////////////////////////////
CJointBayesian::~CJointBayesian()
{
 delete[] pProbabilityCache;
 delete[] pindex;
}

/////////////////////////////////////////////////////////////////////////////
// Get probability from indices
/////////////////////////////////////////////////////////////////////////////
double CJointBayesian::GetProbability() const
{
 double P = 1;

 for (int i = rs.GetGames(); --i >= 0;)
 {
#if 0
  P *= bt.ResultProbability(dc.ValueFromIndex(pindex[rs.GetWhite(i)]),
                            dc.ValueFromIndex(pindex[rs.GetBlack(i)]),
                            rs.GetResult(i));
#else
  P *= pProbabilityCache[indexMax +
                         pindex[rs.GetWhite(i)] -
                         pindex[rs.GetBlack(i)] +
                         rs.GetResult(i) * (indexMax * 2 + 1)];
#endif
 }

 return P;
}

/////////////////////////////////////////////////////////////////////////////
// Recursive helper function
/////////////////////////////////////////////////////////////////////////////
void CJointBayesian::RecursiveJointBayesian(int player, int indexTotal)
{
 if (player < 0)
 {
  double p = GetProbability();
  for (int i = rs.GetPlayers(); --i >= 0;)
   dc.GetDistribution(i).Add(pindex[i], p);
  return;
 }

 int max = indexMax;
 if (max > indexTotal)
  max = indexTotal;

 int min = 0;
 if (min < indexTotal - player * indexMax)
  min = indexTotal - player * indexMax;

 for (pindex[player] = max + 1; --pindex[player] >= min;)
  RecursiveJointBayesian(player - 1, indexTotal - pindex[player]);
}

/////////////////////////////////////////////////////////////////////////////
// Estimate rating distributions
/////////////////////////////////////////////////////////////////////////////
void CJointBayesian::RunComputation()
{
 for (int i = rs.GetPlayers(); --i >= 0;)
  dc.GetDistribution(i).Reset();

 RecursiveJointBayesian(rs.GetPlayers() - 1,
                        dc.GetDiscretizationSize() * rs.GetPlayers() / 2);

 for (int i = rs.GetPlayers(); --i >= 0;)
  dc.GetDistribution(i).Normalize();
}
