////////////////////////////////////////////////////////////////////////////
//
// consolui.cpp
//
// CConsoleUI class
//
// Remi Coulom
//
// june 1996
//
////////////////////////////////////////////////////////////////////////////
#include "consolui.h"
#include "listi.h"    // CListIterator<T>
#include "readstr.h"  // ReadString

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <sstream>

//
// Size of buffer for command input
//
const int CConsoleUI::BufferSize = 1024;

//
// Array of command strings
//
const char * const CConsoleUI::tszCommands[] =
{
 "?",
 "x",
 "p",
 "child",
 "children",
 "echo",
 "!",
 "prompt",
 "myself",
 0
};

////////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////////
CConsoleUI::CConsoleUI(CConsoleUI *pcui, int openmodeInit):
 openmode(openmodeInit)
{
 SetParent(pcui);
}

////////////////////////////////////////////////////////////////////////////
// Set parent
////////////////////////////////////////////////////////////////////////////
void CConsoleUI::SetParent(CConsoleUI *pcui)
{
 pcuiParent = pcui;
 if (pcui)
  SetFPrompt(pcui->GetFPrompt());
 else
  fPrompt = 1;
}

////////////////////////////////////////////////////////////////////////////
// Function to look up a string in a nul-terminated array of strings
// return value: -1 if not found, index if found
////////////////////////////////////////////////////////////////////////////
int CConsoleUI::ArrayLookup(const char * const psz,
                            const char * const * const ppsz)
{
 int Result = -1;

 for (int i = 0; ppsz[i]; i++)
  if (!strcmp(psz, ppsz[i]))
  {
   Result = i;
   break;
  }

 return Result;
}

////////////////////////////////////////////////////////////////////////////
// Function to set a flag
////////////////////////////////////////////////////////////////////////////
const char *CConsoleUI::SetFlag(int &f, const char *psz)
{
 if (!strcmp(psz, "on"))
  f = 1;
 else if (!strcmp(psz, "off"))
  f = 0;

 return f ? "on\n" : "off\n";
}

////////////////////////////////////////////////////////////////////////////
// Opening of a child
////////////////////////////////////////////////////////////////////////////
void CConsoleUI::OpenChild(int Parameter,
                           std::istream &in,
                           std::ostream &out)
{
 CListIterator<CConsoleUI *> listi(lpcuiChildren);
 int Child = 1;

 while (!listi.IsAtTheEnd() && Child != Parameter)
 {
  listi.Increment();
  Child++;
 }

 if (listi.IsAtTheEnd())
  out << "Error: no child having number " << Parameter << ".\n";
 else
  if (listi.Value()->MainLoop(in, out) == PC_Close)
  {
   delete listi.Value();
   listi.Remove();
  }
}

////////////////////////////////////////////////////////////////////////////
// Function to print an error message
////////////////////////////////////////////////////////////////////////////
void CConsoleUI::PrintErrorMessage(std::ostream &out, const char *pszCommand)
{
 out << "Unknown command: " << pszCommand << '\n';
 out << "type '?' for help\n";
}   

////////////////////////////////////////////////////////////////////////////
// Function to process commands
////////////////////////////////////////////////////////////////////////////
int CConsoleUI::ProcessCommand(const char *pszCommand,
                               const char *pszParameters,
                               std::istream &in,
                               std::ostream &out)
{
 enum
 {
  IDC_Help,
  IDC_Close,
  IDC_Parent,
  IDC_Child,
  IDC_Children,
  IDC_Echo,
  IDC_System,
  IDC_Prompt,
  IDC_Myself
 };

 int RetVal = PC_Continue;
 
 switch (ArrayLookup(pszCommand, tszCommands))
 {
  case IDC_Help: ///////////////////////////////////////////////////////////
   out << '\n';
   out << "Syntax for command lines\n";
   out << "~~~~~~~~~~~~~~~~~~~~~~~~\n";
   out << "_command_ _parameters_ [< _input_] [>[>] _output_] [;_comment_]\n";
   out << '\n';
   out << "Universal commands\n";
   out << "~~~~~~~~~~~~~~~~~~\n";
   out << "x ............... close this console interface\n";
   out << "p ............... go to the parent interface\n";
   out << "children ........ list children interfaces\n";
   out << "child <n> ....... select a child interface\n";
   out << '\n';
   out << "echo <params> ... repeat <params> to the output stream\n";
   out << "! [<command>] ... make a system call\n";
   out << "prompt [on|off] . show|hide the command prompt\n";
   out << "myself <n> ...... call recursively this interface <n> times\n";
   out << '\n';
  break;

  case IDC_Close: //////////////////////////////////////////////////////////
   if (openmode != OpenPermanent)
    RetVal = PC_Close;
   else
    out << "Error: impossible to close a permanent interface\n";
  break;

  case IDC_Parent: /////////////////////////////////////////////////////////
   if (openmode != OpenModal)
    RetVal = PC_Parent;
   else
    out << "Error: parent not accessible to modal interfaces\n";
  break;

  case IDC_Child: //////////////////////////////////////////////////////////
  {
   int Parameter = 0;
   std::istringstream(pszParameters) >> Parameter;
   OpenChild(Parameter, in, out);
  }
  break;

  case IDC_Children: ///////////////////////////////////////////////////////
  {
   CListIterator<CConsoleUI *> listi(lpcuiChildren);
   int n = 0;

   while (!listi.IsAtTheEnd())
   {
    out << ' ' << ++n << ": ";
    listi.Value()->PrintLocalPrompt(out);
    out << '\n';
    listi.Increment();
   }
  }
  break;

  case IDC_Echo: ///////////////////////////////////////////////////////////
   out << pszParameters << '\n';
  break;

  case IDC_System: /////////////////////////////////////////////////////////
  #ifndef NOSYSTEM
   system(pszParameters);
  #else
   out << "Not available.\n";
  #endif
  break;

  case IDC_Prompt: /////////////////////////////////////////////////////////
   out << SetFlag(fPrompt, pszParameters);
  break;

  case IDC_Myself: /////////////////////////////////////////////////////////
  {
   int Parameter = 1;
   std::istringstream(pszParameters) >> Parameter;
   int OldPrompt = GetFPrompt();
   SetFPrompt(0);
   for (int i = Parameter; --i >= 0;)
   {
    MainLoop(in, out);
    in.seekg(0);
   }
   SetFPrompt(OldPrompt);
  }
  break;

  default: /////////////////////////////////////////////////////////////////
   PrintErrorMessage(out, pszCommand);
  break;
 }
 
 return RetVal;
}

////////////////////////////////////////////////////////////////////////////
// Function to set the parent CUI for this CUI
////////////////////////////////////////////////////////////////////////////
void CConsoleUI::AddChild(CConsoleUI *pcui,
                          std::istream &in,
                          std::ostream &out)
{
 if (pcui)
 {
  lpcuiChildren.Add();
  lpcuiChildren.Head() = pcui;
  OpenChild(1, in, out);
 }
 else
  out << "Error: could not create child\n";
}

////////////////////////////////////////////////////////////////////////////
// Function: parses a command string
//           The syntax is:
//
// 'command' 'parameters' [<'input'] [>[>]'output'] ;'comment'
//
//            '\0's are inserted in the string between fields
////////////////////////////////////////////////////////////////////////////
void CConsoleUI::Parse(char *pszCommand,
                       const char *&pszCommand2,
                       const char *&pszParameters,
                       const char *&pszInput,
                       const char *&pszOutput) const
{
 //
 // Tabulation and white spaces at the beginning are ignored
 //
 while(*pszCommand == ' ' || *pszCommand == '\t')
  pszCommand++;
 pszCommand2 = pszCommand;
  
 //
 // Comments are removed
 //
 {
  char *psz = strchr(pszCommand, ';');
  if (psz)
   *psz = 0;
 }

 //
 // Name of file for output
 //
 pszOutput = 0;
 {
  char *psz = strchr(pszCommand, '>');
  if (psz)
  {
   pszOutput = psz + 1;
   *psz = 0;
  }
 }

 //
 // Name of file for input
 //
 pszInput = 0;
 {
  char *psz = strchr(pszCommand, '<');
  if (psz)
  {
   pszInput = psz + 1;
   *psz = 0;
  }
 }

 //
 // Separation between command string and parameters
 //
 char *psz = pszCommand;

 while(*psz && *psz != ' ')
  psz++;

 while(*psz == ' ')
 {
  *psz = 0;
  psz++;
 }

 pszParameters = psz;
}

////////////////////////////////////////////////////////////////////////////
// Function to print a prompt
////////////////////////////////////////////////////////////////////////////
void CConsoleUI::PrintPrompt(std::ostream &out)
{
 if (pcuiParent)
 {
  pcuiParent->PrintPrompt(out);
  switch(openmode)
  {
   case OpenModal: out << '-'; break;
   case OpenModeless: out << '/'; break;
   case OpenPermanent: out << '|'; break;
  }
 }
 PrintLocalPrompt(out);
}

////////////////////////////////////////////////////////////////////////////
// Function to process a command
////////////////////////////////////////////////////////////////////////////
int CConsoleUI::EnterParseAndProcess(std::istream &in,
                                     std::ostream &out,
                                     std::ostream *pofsLog)
{
 int Result = PC_Continue;
 
 //
 // The user may be prompted
 //
 if (fPrompt)
 {
  PrintPrompt(out);
  out << '>';
 }

 //
 // The output stream is flushed before the command is entered
 //
 out.flush();

 //
 // The command string is entered
 //
 char szCommand[BufferSize];
 if (ReadString(in, szCommand, sizeof(szCommand)) < 0)
  Result = PC_Close;

 {
  //
  // The string is parsed
  //
  const char *pszCommand;
  const char *pszParameters;
  const char *pszInput;
  const char *pszOutput;

  Parse(szCommand, pszCommand, pszParameters, pszInput, pszOutput);

  //
  // The command is processed only if it is not empty
  //
  if (*pszCommand)
  {
   int fStreamsOK = 1;

   //
   // New input stream
   //
   std::istream *pinNew = &in;
   std::ifstream *pifs = 0;
   if (pszInput)
   {
    pifs = new std::ifstream(pszInput, std::ios::in);
    if (pifs->good())
     pinNew = pifs;
    else
    {
     out << "Error: could not open input stream: " << pszInput << '\n';
     out.flush();
     fStreamsOK = 0;
    }
   }

   //
   // New output stream
   //
   std::ostream *poutNew = &out;
   std::ofstream *pofs = 0;
   if (pszOutput)
   {
    pofs = new std::ofstream;
    if (*pszOutput == '>')
     pofs->open(pszOutput + 1, std::ios::out | std::ios::app);
    else
     pofs->open(pszOutput, std::ios::out);

    if (pofs->good())
     poutNew = pofs;
    else
    {
     out << "Error: could not open output stream: " << pszOutput << '\n';
     out.flush();
     fStreamsOK = 0;
    }
   }

   //
   // The command is processed with these new streams
   //
   if (fStreamsOK)
   {
    if (pofsLog)
    {
     *pofsLog << "Command: " << pszCommand << ' ' << pszParameters << '\n';
     pofsLog->flush();
    } 
    Result = ProcessCommand(pszCommand, pszParameters, *pinNew, *poutNew);
    poutNew->flush();
   } 

   //
   // If new file streams have been created, we must delete them
   //
   if (pifs)
    delete pifs;
   if (pofs)
    delete pofs;
  }
 }

 return Result;
}

////////////////////////////////////////////////////////////////////////////
// Function: Main loop of the user interface
////////////////////////////////////////////////////////////////////////////
int CConsoleUI::MainLoop(std::istream &in, std::ostream &out)
{
 while(1)
 {
  int Result;

  Result = EnterParseAndProcess(in, out);
  if (Result != PC_Continue)
   return Result;
 }

 return PC_Close;
}

////////////////////////////////////////////////////////////////////////////
// Get[Set] utility function
////////////////////////////////////////////////////////////////////////////
template<class T>
void CConsoleUI::GetSet(T &x, const char *pszParameters, std::ostream &out)
{
 if (*pszParameters)
 {
  std::istringstream is(pszParameters);
  is >> x;
 }
 out << x << '\n';
}

template void CConsoleUI::GetSet<int>(int&, const char*, std::ostream&);
template void CConsoleUI::GetSet<float>(float&, const char*, std::ostream&);
template void CConsoleUI::GetSet<double>(double&, const char*, std::ostream&);

////////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////////
CConsoleUI::~CConsoleUI()
{
 CListIterator<CConsoleUI *> listi(lpcuiChildren);
 
 while (!listi.IsAtTheEnd())
 {
  delete listi.Value();
  listi.Remove();
 }
}
