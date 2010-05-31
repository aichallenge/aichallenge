////////////////////////////////////////////////////////////////////////////
//
// CCDistributionCUI.h
//
// Remi Coulom
//
// January, 2005
//
////////////////////////////////////////////////////////////////////////////
#ifndef CCDistributionCUI_Declared
#define CCDistributionCUI_Declared

#include "consolui.h" // CConsoleUI

class CCDistribution;

class CCDistributionCUI : public CConsoleUI // cdcui
{
 private: //////////////////////////////////////////////////////////////////
  static const char * const tszCommands[];

  const CCDistribution &cdist;

 protected: ////////////////////////////////////////////////////////////////
  virtual int ProcessCommand(const char *pszCommand,
                             const char *pszParameters,
                             std::istream &in,
                             std::ostream &out);

  virtual void PrintLocalPrompt(std::ostream &out);

 public: ///////////////////////////////////////////////////////////////////
  CCDistributionCUI(const CCDistribution &cdistInit,
                    CConsoleUI *pcui = 0,
                    int openmode = OpenModal);
};

#endif
