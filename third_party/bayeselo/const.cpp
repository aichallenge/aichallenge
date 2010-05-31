////////////////////////////////////////////////////////////////////////////
//
// const.cpp
//
// CConst class
//
// Remi Coulom
//
// July 1996
//
////////////////////////////////////////////////////////////////////////////
#include "const.h"
#include "square.h"     // CSquare class

//
// Table indicating distances to the edge of the board
//
const int CConst::tcEdge[64] =
{
 Left|Down ,Left2|Down ,Down ,Down ,Down ,Down ,Right2|Down ,Right|Down,
 Left|Down2,Left2|Down2,Down2,Down2,Down2,Down2,Right2|Down2,Right|Down2,
 Left      ,Left2      ,0    ,0    ,0    ,0    ,Right2      ,Right,
 Left      ,Left2      ,0    ,0    ,0    ,0    ,Right2      ,Right,
 Left      ,Left2      ,0    ,0    ,0    ,0    ,Right2      ,Right,
 Left      ,Left2      ,0    ,0    ,0    ,0    ,Right2      ,Right,
 Left|Up2  ,Left2|Up2  ,Up2  ,Up2  ,Up2  ,Up2  ,Right2|Up2  ,Right|Up2,
 Left|Up   ,Left2|Up   ,Up   ,Up   ,Up   ,Up   ,Right2|Up   ,Right|Up
};

//
// Table of Knight moves
//
const struct CConst::st CConst::TKnightMove[8] =
{
 {Up           | Left  | Left2  , CSquare::a5 - CSquare::c4},
 {Up   | Up2   | Left           , CSquare::b6 - CSquare::c4},
 {Up   | Up2   | Right          , CSquare::d6 - CSquare::c4},
 {Up           | Right | Right2 , CSquare::e5 - CSquare::c4},
 {Down         | Right | Right2 , CSquare::e3 - CSquare::c4},
 {Down | Down2 | Right          , CSquare::d2 - CSquare::c4},
 {Down | Down2 | Left           , CSquare::b2 - CSquare::c4},
 {Down         | Left  | Left2  , CSquare::a3 - CSquare::c4}
};

//
// Table of King moves
//
const struct CConst::st CConst::TKingMove[8] =
{
 {Down | Right, CSquare::c1 - CSquare::b2},
 {Down        , CSquare::b1 - CSquare::b2},
 {Down | Left , CSquare::a1 - CSquare::b2},
 {       Left , CSquare::a2 - CSquare::b2},
 {Up   | Left , CSquare::a3 - CSquare::b2},
 {Up          , CSquare::b3 - CSquare::b2},
 {Up   | Right, CSquare::c3 - CSquare::b2},
 {       Right, CSquare::c2 - CSquare::b2}
};

//
// Array of directions
//
const int CConst::tDirection[13] =
{
 0,
 CSquare::Left,
                 CSquare::Up,
 CSquare::Left + CSquare::Up,
                               CSquare::Right,
 CSquare::Left +               CSquare::Right,
                 CSquare::Up + CSquare::Right,
 CSquare::Left + CSquare::Up + CSquare::Right,
                                                CSquare::Down,
 CSquare::Left +                                CSquare::Down,
                 CSquare::Up +                  CSquare::Down,
 CSquare::Left + CSquare::Up +                  CSquare::Down,
                               CSquare::Right + CSquare::Down
};

