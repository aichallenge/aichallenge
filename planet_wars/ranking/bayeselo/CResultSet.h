/////////////////////////////////////////////////////////////////////////////
//
// Rémi Coulom
//
// December, 2004
//
/////////////////////////////////////////////////////////////////////////////
#ifndef CResultSet_Declared
#define CResultSet_Declared

#include <vector>
#include <string>

class CResultSet // rs
{
 private: //////////////////////////////////////////////////////////////////
  std::vector<unsigned> vWhite;
  std::vector<unsigned> vBlack;
  std::vector<unsigned> vResult;

  unsigned Players;

 public: ///////////////////////////////////////////////////////////////////
  CResultSet(): Players(0) {}
  CResultSet(const CResultSet &rs, int Player);
  CResultSet(const CResultSet &rs, int Player1, int Player2);

  unsigned GetPlayers() const {return Players;}
  int GetGames() const {return vResult.size();}
  int CountGames(unsigned Player) const;

  int GetWhite(int i) const {return vWhite[i];}
  int GetBlack(int i) const {return vBlack[i];}
  int GetResult(int i) const {return vResult[i];}

  void Reset();
  void Append(unsigned w, unsigned b, unsigned r);
  void RemoveGame(int Index);
  void RemovePlayer(unsigned Player);
  void RemoveRarePlayers(int Threshold);

  void PackPlayers(std::vector<std::string> &vName);

  void Extract(CResultSet &rs1, CResultSet &rs2, unsigned Seed) const;
  int Connect(unsigned Player,
              int ForbiddenResult,
              std::vector<std::string> &vecName);
};

#endif
