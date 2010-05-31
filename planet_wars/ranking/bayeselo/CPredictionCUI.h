////////////////////////////////////////////////////////////////////////////
//
// CPredictionCUI.h
//
// Remi Coulom
//
// December, 2005
//
////////////////////////////////////////////////////////////////////////////
#ifndef CPredictionCUI_Declared
#define CPredictionCUI_Declared

#include "consolui.h" // CConsoleUI
#include "CResultSet.h"

#include <vector>

class CEloRatingCUI;

class CPredictionCUI : public CConsoleUI // predcui
{
 private: //////////////////////////////////////////////////////////////////
  static const char * const tszCommands[];

  CEloRatingCUI &ercui;
  std::vector<double> vbtvariance;

  std::vector<double> velo;
  std::vector<double> vStdDev;
  CResultSet rs;
  std::vector<std::string> vecName;

  int Simulations;
  int Rounds;
  int tScore[3];
  double ScoreMultiplier;

 protected: ////////////////////////////////////////////////////////////////
  virtual int ProcessCommand(const char *pszCommand,
                             const char *pszParameters,
                             std::istream &in,
                             std::ostream &out);

  virtual void PrintLocalPrompt(std::ostream &out);

 public: ///////////////////////////////////////////////////////////////////
  CPredictionCUI(CEloRatingCUI &ercuiInit, int openmode = OpenModal);
};

#endif
