/////////////////////////////////////////////////////////////////////////////
//
// Rémi Coulom
//
// December, 2004
//
/////////////////////////////////////////////////////////////////////////////
#include "version.cpp"
#include "pgnlex.cpp"

#include "str.cpp"
#include "const.cpp"
#include "date.cpp"
#include "pgnstr.cpp"
#include "move.cpp"

#include "consolui.cpp"
#include "clktimer.cpp"
#include "CTimeIO.cpp"
#include "chtime.cpp"
#include "readstr.cpp"
#include "ReadLineToString.cpp"
#include "CVector.cpp"
#include "CMatrix.cpp"
#include "CMatrixIO.cpp"
#include "CLUDecomposition.cpp"

#include "CBradleyTerry.cpp"
#include "CCDistribution.cpp"
#include "CCDistributionCUI.cpp"
#include "CCondensedResults.cpp"
#include "CDistribution.cpp"
#include "CDistributionCollection.cpp"
#include "CEloRatingCUI.cpp"
#include "CJointBayesian.cpp"
#include "CResultSet.cpp"
#include "CResultSetCUI.cpp"
#include "EloDataFromFile.cpp"
#include "CPredictionCUI.cpp"

#include <iostream>

/////////////////////////////////////////////////////////////////////////////
// main function
/////////////////////////////////////////////////////////////////////////////
int main()
{
 std::cout << CVersion::GetCopyright();
 CResultSet rs;
 std::vector<std::string> vecName;
 CResultSetCUI rscui(rs, vecName);
 rscui.MainLoop(std::cin, std::cout);

 return 0;
}
