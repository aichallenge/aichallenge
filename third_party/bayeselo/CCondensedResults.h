/////////////////////////////////////////////////////////////////////////////
//
// Rémi Coulom
//
// December, 2004
//
/////////////////////////////////////////////////////////////////////////////
#ifndef CCondensedResults_Declared
#define CCondensedResults_Declared

#include <iosfwd>

class CResultSet;

class CCondensedResult // cr
{
 public:
  int Opponent;
  int TrueGames;
  float w_ij;
  float d_ij;
  float l_ij;
  float w_ji;
  float d_ji;
  float l_ji;

  void Reset();
  float Games() const {return w_ij + d_ij + l_ij + w_ji + d_ji + l_ji;}
  float Score() const {return w_ij + l_ji + (d_ij + d_ji) * 0.5;}
};

class CCondensedResults // crs
{
 private: ///////////////////////////////////////////////////////////////////
  int Players;
  int *pOpponents;
  CCondensedResult **ppcr;

 public: ////////////////////////////////////////////////////////////////////
  CCondensedResults(const CResultSet &rs);

  void AddPrior(float PriorDraw);

  int GetPlayers() const {return Players;}
  int GetOpponents(int Player) const {return pOpponents[Player];}

  const CCondensedResult &GetCondensedResult(int Player, int i) const
   {return ppcr[Player][i];}
  CCondensedResult &GetCondensedResult(int Player, int i)
   {return ppcr[Player][i];}
  CCondensedResult &FindOpponent(int Player, int Opponent);

  float CountGames(int Player) const;
  int CountTrueGames(int Player) const;
  float CountDraws(int Player) const;
  float Score(int Player) const; // win = 2, draw = 1
  double AverageOpponent(int Player, const double *pelo) const;

  void Dump(std::ostream &out) const;

  ~CCondensedResults();
};

#endif
