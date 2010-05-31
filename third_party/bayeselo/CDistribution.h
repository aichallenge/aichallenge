/////////////////////////////////////////////////////////////////////////////
//
// Rémi Coulom
//
// December, 2004
//
/////////////////////////////////////////////////////////////////////////////
#ifndef CDistribution_Declared
#define CDistribution_Declared

#include <vector>

class CDistribution // dist
{
 protected: ////////////////////////////////////////////////////////////////
  std::vector<double> vDistribution;

  int GetBoundIndex(double Confidence,
                    int Begin,
                    int End,
                    int Direction) const;

 public: ///////////////////////////////////////////////////////////////////
  CDistribution() : vDistribution(1) {vDistribution[0] = 1.0;}
  CDistribution(int Size) : vDistribution(Size) {}

  //
  // Basic functions
  //
  int GetSize() const {return vDistribution.size();}
  double GetProbability(int i) const {return vDistribution[i];}
  void SetProbability(int i, double p) {vDistribution[i] = p;}
  void Add(int i, double p) {vDistribution[i] += p;}
  
  void Reset();
  void SetUniform();
  void Normalize();
  void LogNormalize();

  //
  // Discrete bounds
  //
  int GetLowerIndex(double Confidence) const;
  int GetUpperIndex(double Confidence) const;

  //
  // Functions to deal with result distributions
  //
  friend CDistribution BinomialDistribution(double p);
  friend CDistribution SumDistribution(const CDistribution &dist1,
                                       const CDistribution &dist2);
};

#endif
