////////////////////////////////////////////////////////////////////////////
//
// CEloRatingCUI.h
//
// Remi Coulom
//
// December, 2004
//
////////////////////////////////////////////////////////////////////////////
#ifndef CEloRatingCUI_Declared
#define CEloRatingCUI_Declared

#include "consolui.h" // CConsoleUI
#include "CCondensedResults.h"
#include "CBradleyTerry.h"

#include <vector>
#include <string>

class CResultSet;
class CDistributionCollection;
class CPredictionCUI;

class CEloRatingCUI : public CConsoleUI // ercui
{
 friend class CPredictionCUI;

 private: //////////////////////////////////////////////////////////////////
  static const char * const tszCommands[];
  const CResultSet &rs;
  CCondensedResults crs;
  std::vector<std::string> vecName;
  unsigned MaxNameLength;
  std::vector<int> vPermutation;
  std::vector<double> veloLower;
  std::vector<double> veloUpper;
  CDistributionCollection *pdc;

  double Confidence;
  double eloMin;
  double eloMax;
  int Resolution;
  double eloOffset;
  double EloScale;
  float Prior;

  CBradleyTerry bt;
  int fLOSComputed;

  void ComputeVariance();

 protected: ////////////////////////////////////////////////////////////////
  virtual int ProcessCommand(const char *pszCommand,
                             const char *pszParameters,
                             std::istream &in,
                             std::ostream &out);

  virtual void PrintLocalPrompt(std::ostream &out);

 public: ///////////////////////////////////////////////////////////////////
  CEloRatingCUI(const CResultSet &rsInit,
                const std::vector<std::string> &vecNameInit,
                CConsoleUI *pcui = 0,
                int openmode = OpenModal);
};

#endif
