<?php

require_once 'Ants.php';

class LeftyBot
{
    public $antsLefty = array();
    public $antsStraight = array();
    
    public function doTurn( $ants )
    {
        $destinations = array();
        $newStraight = array();
        $newLefty = array();


        foreach( $ants->myAnts as $ant ) {
            list($aRow, $aCol) = $ant;

            // send new ants in a straight line
            $aKey = $aRow.'_'.$aCol;
            if ( !isset($this->antsStraight[$aKey]) && !isset($this->antsLefty[$aKey]) ) {
                if ($aRow % 2 == 0) {
                    if ($aCol % 2 == 0) {
                        $direction = 'n';
                    } else {
                        $direction = 's';
                    }
                } else {
                    if ($aCol % 2 == 0) {
                        $direction = 'e';
                    } else {
                        $direction = 'w';
                    }
                }
                $this->antsStraight[$aKey] = $direction;
            }
            // send ants going in a straight line in the same direction
            if (isset($this->antsStraight[$aKey])) {
                $direction = $this->antsStraight[$aKey];
                list($nRow, $nCol) = $ants->destination($aRow, $aCol, $direction);
                $nKey = $nRow.'_'.$nCol;
                if ($ants->passable($nRow, $nCol)) {
                    if ($ants->unoccupied($nRow, $nCol) && !isset($destinations[$nKey]) ) {
                        $ants->issueOrder($aRow, $aCol, $direction);
                        $newStraight[$nKey] = $direction;
                        $destinations[$nKey] = 1;
                    } else {
                        // pause ant, turn and try again next turn
                        $newStraight[$aKey] = $ants->LEFT[$direction];
                        $destinations[$aKey] = 1;
                    }
                } else {
                    // hit a wall, start following it
                    $this->antsLefty[$aKey] = $ants->RIGHT[$direction];
                }
            }

            // send ants following a wall, keeping it on their left
            if (isset($this->antsLefty[$aKey])) {
                $direction = $this->antsLefty[$aKey];
                $directions = array( $ants->LEFT[$direction], $direction, $ants->RIGHT[$direction], $ants->BEHIND[$direction] );
                // try 4 directions in order, attempting to turn left at corners
                foreach ($directions as $newDirection) {
                    list($nRow, $nCol) = $ants->destination($aRow, $aCol, $newDirection);
                    $nKey = $nRow.'_'.$nCol;
                    if ($ants->passable($nRow, $nCol)) {
                        if ($ants->unoccupied($nRow, $nCol) && !isset($destinations[$nKey])) {
                            $ants->issueOrder($aRow, $aCol, $newDirection);
                            $newLefty[$nKey] = $newDirection;
                            $destinations[$nKey] = 1;
                            break;
                        } else {
                            # have ant wait until it is clear
                            $newStraight[$aKey] = $ants->RIGHT[$direction];
                            $destinations[$aKey] = 1;
                            break;
                        }
                    }
                }
            }
        }

        # reset lists
        $this->antsStraight = $newStraight;
        $this->antsLefty = $newLefty;
    }
}


Ants::run( new LeftyBot() );