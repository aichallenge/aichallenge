/////////////////////////////////////////////////////////////////////////////
//
// Rémi Coulom
//
// December, 2004
//
/////////////////////////////////////////////////////////////////////////////
#include "CCondensedResults.h"
#include "CResultSet.h"
#include "debug.h"

#include <set>
#include <iostream>

/////////////////////////////////////////////////////////////////////////////
// Constructor
/////////////////////////////////////////////////////////////////////////////
CCondensedResults::CCondensedResults(const CResultSet &rs) :
 Players(rs.GetPlayers())
{
 pOpponents = new int[Players];
 ppcr = new CCondensedResult*[Players];

 //
 // Count opponents of each player
 //
 {
  std::set<int> *pset = new std::set<int>[Players];

  for (int i = rs.GetGames(); --i >= 0;)
  {
   int White = rs.GetWhite(i);
   int Black = rs.GetBlack(i);
   pset[White].insert(Black);
   pset[Black].insert(White);
  }

  for (int i = Players; --i >= 0;)
  {
   pOpponents[i] = pset[i].size();
   ppcr[i] = new CCondensedResult[pOpponents[i]];
   for (int j = pOpponents[i]; --j >= 0;)
    ppcr[i][j].Reset();
  }

  delete[] pset;
 }

 //
 // Fill-in all CCondensedResult's
 //
 for (int i = rs.GetGames(); --i >= 0;)
 {
  int White = rs.GetWhite(i);
  int Black = rs.GetBlack(i);

  CCondensedResult &crWhite = FindOpponent(White, Black);
  CCondensedResult &crBlack = FindOpponent(Black, White);
  crWhite.TrueGames++;
  crBlack.TrueGames++;

  switch(rs.GetResult(i))
  {
   case 0: //////////////////////////////////////////////////////////////////
    crWhite.l_ij++;
    crBlack.l_ji++;
   break;

   case 1: //////////////////////////////////////////////////////////////////
    crWhite.d_ij++;
    crBlack.d_ji++;
   break;

   case 2: //////////////////////////////////////////////////////////////////
    crWhite.w_ij++;
    crBlack.w_ji++;
   break;
  }
 }
}

/////////////////////////////////////////////////////////////////////////////
// Add prior
/////////////////////////////////////////////////////////////////////////////
void CCondensedResults::AddPrior(float PriorDraw)
{
 for (int i = Players; --i >= 0;)
 {
  float Prior = PriorDraw * 0.25 / CountTrueGames(i);
  for (int j = pOpponents[i]; --j >= 0;)
  {
   CCondensedResult &crPlayer = ppcr[i][j];
   CCondensedResult &crOpponent = FindOpponent(crPlayer.Opponent, i);
   float ThisPrior = Prior * crPlayer.TrueGames;
   crPlayer.d_ij += ThisPrior;
   crPlayer.d_ji += ThisPrior;
   crOpponent.d_ij += ThisPrior;
   crOpponent.d_ji += ThisPrior;
  }
 }
}

/////////////////////////////////////////////////////////////////////////////
// Reset
/////////////////////////////////////////////////////////////////////////////
void CCondensedResult::Reset()
{
 Opponent = -1;
 TrueGames = 0;
 w_ij = 0;
 d_ij = 0;
 l_ij = 0;
 w_ji = 0;
 d_ji = 0;
 l_ji = 0;
}

/////////////////////////////////////////////////////////////////////////////
// Find Opponent
/////////////////////////////////////////////////////////////////////////////
CCondensedResult &CCondensedResults::FindOpponent(int Player, int Opponent)
{
 for (int i = pOpponents[Player]; --i >= 0;)
 {
  CCondensedResult &cr = ppcr[Player][i];
  if (cr.Opponent == Opponent)
   return cr;
  if (cr.Opponent < 0)
  {
   cr.Opponent = Opponent;
   return cr;
  }
 }

 FATAL(1);
 return ppcr[Player][0];
}

/////////////////////////////////////////////////////////////////////////////
// Count the number of true games played by one player
/////////////////////////////////////////////////////////////////////////////
int CCondensedResults::CountTrueGames(int Player) const
{
 int Result = 0;
 for (int i = pOpponents[Player]; --i >= 0;)
 {
  CCondensedResult &cr = ppcr[Player][i];
  Result += cr.TrueGames;
 }
 return Result;
}

/////////////////////////////////////////////////////////////////////////////
// Evaluate the average opponent of one player
/////////////////////////////////////////////////////////////////////////////
double CCondensedResults::AverageOpponent(int Player, const double *pelo) const
{
 double Total = 0;
 int GameCount = 0;
 for (int i = pOpponents[Player]; --i >= 0;)
 {
  CCondensedResult &cr = ppcr[Player][i];
  Total += cr.TrueGames * pelo[cr.Opponent];
  GameCount += cr.TrueGames;
 }

 if (GameCount)
  return Total / GameCount;
 else
  return 0;
}

/////////////////////////////////////////////////////////////////////////////
// Count the number of games played by one player
/////////////////////////////////////////////////////////////////////////////
float CCondensedResults::CountGames(int Player) const
{
 float Result = 0;
 for (int i = pOpponents[Player]; --i >= 0;)
 {
  CCondensedResult &cr = ppcr[Player][i];
  Result += cr.w_ij + cr.d_ij + cr.l_ij + cr.w_ji + cr.d_ji + cr.l_ji;
 }
 return Result;
}

/////////////////////////////////////////////////////////////////////////////
// Count the number of draws played by one player
/////////////////////////////////////////////////////////////////////////////
float CCondensedResults::CountDraws(int Player) const
{
 float Result = 0;
 for (int i = pOpponents[Player]; --i >= 0;)
 {
  CCondensedResult &cr = ppcr[Player][i];
  Result += cr.d_ij + cr.d_ji;
 }
 return Result;
}

/////////////////////////////////////////////////////////////////////////////
// Get the score of one player
/////////////////////////////////////////////////////////////////////////////
float CCondensedResults::Score(int Player) const
{
 float Result = 0;
 for (int i = pOpponents[Player]; --i >= 0;)
 {
  CCondensedResult &cr = ppcr[Player][i];
  Result += 2 * cr.w_ij + cr.d_ij + cr.d_ji + 2 * cr.l_ji;
 }
 return Result;
}

/////////////////////////////////////////////////////////////////////////////
// Destructor
/////////////////////////////////////////////////////////////////////////////
CCondensedResults::~CCondensedResults()
{
 for (int i = Players; --i >= 0;)
  delete[] ppcr[i];
 delete[] ppcr;
 delete[] pOpponents;
}

/////////////////////////////////////////////////////////////////////////////
// Dump
/////////////////////////////////////////////////////////////////////////////
void CCondensedResults::Dump(std::ostream &out) const
{
 out << "Players = " << Players << '\n';
 for (int i = 0; i < Players; i++)
 {
  out << "Player " << i << '\n';
  out << "pOpponents[i] = " << pOpponents[i] << '\n';
  out << "CountTrueGames(i) = " << CountTrueGames(i) << '\n';
  for (int j = 0; j < pOpponents[i]; j++)
  {
   const CCondensedResult &cr = ppcr[i][j];
   out << "Opponent " << j << " = " << cr.Opponent << '\n';
   out << "TrueGames = " << cr.TrueGames << '\n';
   out << "w_ij = " << cr.w_ij << ' ';
   out << "d_ij = " << cr.d_ij << ' ';
   out << "l_ij = " << cr.l_ij << ' ';
   out << "w_ji = " << cr.w_ji << ' ';
   out << "d_ji = " << cr.d_ji << ' ';
   out << "l_ji = " << cr.l_ji;
   out << '\n';
  }
  out << '\n';
 } 
 out.flush();
}
