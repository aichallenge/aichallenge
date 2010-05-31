////////////////////////////////////////////////////////////////////////////
//
// Remi Coulom
//
// January, 2005
//
////////////////////////////////////////////////////////////////////////////
#include "CCDistributionCUI.h"
#include "CCDistribution.h"

#include <sstream>

////////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////////
CCDistributionCUI::CCDistributionCUI(const CCDistribution &cdistInit,
                                     CConsoleUI *pcui,
                                     int openmode) :
 CConsoleUI(pcui, openmode),
 cdist(cdistInit)
{
}

////////////////////////////////////////////////////////////////////////////
// Local prompt
////////////////////////////////////////////////////////////////////////////
void CCDistributionCUI::PrintLocalPrompt(std::ostream &out)
{
 out << "Distribution";
}

////////////////////////////////////////////////////////////////////////////
// Command strings
////////////////////////////////////////////////////////////////////////////
const char * const CCDistributionCUI::tszCommands[] =
{
 "?",
 "lower",
 "upper",
 "mostlikely",
 "write",
 0
};

////////////////////////////////////////////////////////////////////////////
// Process commands
////////////////////////////////////////////////////////////////////////////
int CCDistributionCUI::ProcessCommand(const char *pszCommand,
                                      const char *pszParameters,
                                      std::istream &in,
                                      std::ostream &out)
{
 enum
 {
  IDC_Help,
  IDC_Lower,
  IDC_Upper,
  IDC_MostLikely,
  IDC_Write
 };

 switch (ArrayLookup(pszCommand, tszCommands))
 {
  case IDC_Help: ///////////////////////////////////////////////////////////
   CConsoleUI::ProcessCommand(pszCommand, pszParameters, in, out);
   out << "Distribution commands\n";
   out << "~~~~~~~~~~~~~~~~~~~~~\n";
   out << "lower [c] ....... lower bound (c=confidence, default=0.95)\n";
   out << "upper [c] ....... upper bound\n";
   out << "mostlikely ...... most likely value\n";
   out << "write ........... write complete distribution\n";
  break;

  case IDC_Lower: //////////////////////////////////////////////////////////
  {
   double Confidence = 0.95;
   std::istringstream(pszParameters) >> Confidence;
   out << cdist.GetLowerValue(Confidence) << '\n';
  }
  break;

  case IDC_Upper: //////////////////////////////////////////////////////////
  {
   double Confidence = 0.95;
   std::istringstream(pszParameters) >> Confidence;
   out << cdist.GetUpperValue(Confidence) << '\n';
  }
  break;

  case IDC_MostLikely: /////////////////////////////////////////////////////
   out << cdist.GetMostLikelyValue() << '\n';
  break;

  case IDC_Write: //////////////////////////////////////////////////////////
   for (int i = 0; i < cdist.GetSize(); i++)
    out << cdist.ValueFromIndex(i) << ' ' << cdist.GetProbability(i) << '\n';
  break;

  default: /////////////////////////////////////////////////////////////////
   return CConsoleUI::ProcessCommand(pszCommand, pszParameters, in, out);
 }

 return PC_Continue;
}
