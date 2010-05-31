/////////////////////////////////////////////////////////////////////////////
//
// Rémi Coulom
//
// December, 2004
//
/////////////////////////////////////////////////////////////////////////////
#include "CResultSet.h"
#include "debug.h"
#include "random.h"

/////////////////////////////////////////////////////////////////////////////
// Subset constructor for one player
/////////////////////////////////////////////////////////////////////////////
CResultSet::CResultSet(const CResultSet &rs, int Player)
{
 Reset();
 for (int i = 0; i < rs.GetGames(); i++)
  if (rs.GetWhite(i) == Player || rs.GetBlack(i) == Player)
   Append(rs.GetWhite(i), rs.GetBlack(i), rs.GetResult(i));
}

/////////////////////////////////////////////////////////////////////////////
// Subset constructor for a pair of players (+pack)
/////////////////////////////////////////////////////////////////////////////
CResultSet::CResultSet(const CResultSet &rs, int Player1, int Player2)
{
 Reset();
 for (int i = 0; i < rs.GetGames(); i++)
 {
  if (rs.GetWhite(i) == Player1 && rs.GetBlack(i) == Player2)
   Append(0, 1, rs.GetResult(i));
  if (rs.GetWhite(i) == Player2 && rs.GetBlack(i) == Player1)
   Append(1, 0, rs.GetResult(i));
 }
}

/////////////////////////////////////////////////////////////////////////////
// Count games of a player
/////////////////////////////////////////////////////////////////////////////
int CResultSet::CountGames(unsigned Player) const
{
 int Result = 0;
 for (int i = GetGames(); --i >= 0;)
  if (vWhite[i] == Player || vBlack[i] == Player)
   Result++;
 return Result;
}

/////////////////////////////////////////////////////////////////////////////
// Reset to empty set
/////////////////////////////////////////////////////////////////////////////
void CResultSet::Reset()
{
 vWhite.clear();
 vBlack.clear();
 vResult.clear();
 Players = 0;
}

/////////////////////////////////////////////////////////////////////////////
// Add a result
/////////////////////////////////////////////////////////////////////////////
void CResultSet::Append(unsigned w, unsigned b, unsigned r)
{
 FATAL(r > 2);

 vWhite.push_back(w);
 vBlack.push_back(b);
 vResult.push_back(r);

 if (w >= Players)
  Players = w + 1;
 if (b >= Players)
  Players = b + 1;
}

/////////////////////////////////////////////////////////////////////////////
// Remove a game
/////////////////////////////////////////////////////////////////////////////
void CResultSet::RemoveGame(int Index)
{
 int Games = GetGames();
 vWhite[Index] = vWhite[Games - 1];
 vBlack[Index] = vBlack[Games - 1];
 vResult[Index] = vResult[Games - 1];
 vWhite.resize(Games - 1);
 vBlack.resize(Games - 1);
 vResult.resize(Games - 1);
}

/////////////////////////////////////////////////////////////////////////////
// Remove games by a player
/////////////////////////////////////////////////////////////////////////////
void CResultSet::RemovePlayer(unsigned Player)
{
 for (int i = GetGames(); --i >= 0;)
  if (vWhite[i] == Player || vBlack[i] == Player)
   RemoveGame(i);
}

/////////////////////////////////////////////////////////////////////////////
// Remove games by player who have played less than Threshold games
/////////////////////////////////////////////////////////////////////////////
void CResultSet::RemoveRarePlayers(int Threshold)
{
 std::vector<int> vPlayersToRemove;
 for (int i = GetPlayers(); --i >= 0;)
  if (CountGames(i) < Threshold)
   vPlayersToRemove.push_back(i);
 for (int i = vPlayersToRemove.size(); --i >= 0;)
  RemovePlayer(vPlayersToRemove[i]);
}

/////////////////////////////////////////////////////////////////////////////
// Pack players
/////////////////////////////////////////////////////////////////////////////
void CResultSet::PackPlayers(std::vector<std::string> &vName)
{
 //
 // Find Players that have games
 //
 std::vector<int> vTranslation(Players);
 std::vector<int> vReverseTranslation(Players);
 int NewPlayers = 0;
 for (unsigned i = 0; i < Players; i++)
 {
  vTranslation[i] = NewPlayers;
  if (CountGames(i))
  {
   vReverseTranslation[NewPlayers] = i;
   NewPlayers++;
  }
 }

 //
 // Change player numbers
 //
 for (int i = GetGames(); --i >= 0;)
 {
  vWhite[i] = vTranslation[vWhite[i]];
  vBlack[i] = vTranslation[vBlack[i]];
 }
 Players = NewPlayers;

 //
 // Reorder player names
 //
 std::vector<std::string> vNewName(NewPlayers);
 for (int i = NewPlayers; --i >= 0;)
  vNewName[i] = vName[vReverseTranslation[i]];
 vName = vNewName;
}

/////////////////////////////////////////////////////////////////////////////
// Extract a subset (used for cross-validation)
/////////////////////////////////////////////////////////////////////////////
void CResultSet::Extract(CResultSet &rs1, CResultSet &rs2, unsigned Seed) const
{
 CRandom<unsigned> rnd(Seed);
 for (int i = 0; i < GetGames(); i++)
 {
  if (rnd.NewValue() & 1)
   rs1.Append(GetWhite(i), GetBlack(i), GetResult(i));
  else
   rs2.Append(GetWhite(i), GetBlack(i), GetResult(i));
 }
}

/////////////////////////////////////////////////////////////////////////////
// Connect (return the new number of Player)
/////////////////////////////////////////////////////////////////////////////
int CResultSet::Connect(unsigned Player,
                        int ForbiddenResult,
                        std::vector<std::string> &vecName)
{
 if (Player >= GetPlayers())
  return Player;

 std::vector<int> vConnected(GetPlayers());
 std::fill(vConnected.begin(), vConnected.end(), 0);

 //
 // Loop to find connected players
 //
 vConnected[Player] = 1;
 while(1)
 {
  int Additions = 0;

  for (int i = GetGames(); --i >= 0;)
   if (vConnected[GetWhite(i)] == 1 &&
       vConnected[GetBlack(i)] == 0 &&
       GetResult(i) != ForbiddenResult)
   {
    vConnected[GetBlack(i)] = 1;
    Additions++;
   }
   else if (vConnected[GetWhite(i)] == 0 &&
            vConnected[GetBlack(i)] == 1 &&
            GetResult(i) != 2 - ForbiddenResult)
   {
    vConnected[GetWhite(i)] = 1;
    Additions++;
   }

  if (Additions == 0)
   break;
 }

 //
 // Compute translation of player numbers
 //
 std::vector<int> vTranslation(GetPlayers());
 std::vector<int> vReverseTranslation(GetPlayers());

 int Players = 0;
 for (unsigned i = 0; i < GetPlayers(); i++)
 {
  vTranslation[i] = Players;
  vReverseTranslation[Players] = i;
  Players += vConnected[i];
 }

 //
 // Build new vector of player names
 //
 std::vector<std::string> vecNewName(Players);
 for (int i = Players; --i >= 0;)
  vecNewName[i] = vecName[vReverseTranslation[i]];
 vecName = vecNewName;

 //
 // Build new set of results
 //
 CResultSet rsNew;
 for (int i = 0; i < GetGames(); i++)
  if (vConnected[GetWhite(i)] && vConnected[GetBlack(i)])
   rsNew.Append(vTranslation[GetWhite(i)],
                vTranslation[GetBlack(i)],
                GetResult(i));
 *this = rsNew;

 return vTranslation[Player];
}
