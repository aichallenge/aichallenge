<?php
require_once 'Ants.php';

class RandomBot
{
    public function doTurn( $ants )
    {
        $destinations = array();
        foreach( $ants->myAnts as $ant ) {
            list($aRow, $aCol) = $ant;
            // try all directions randomly until one is passable and not occupied
            $directions = array_keys($ants->AIM);
            shuffle($directions);
            foreach ( $directions as $direction ) {
                list($nRow, $nCol) = $ants->destination($aRow, $aCol, $direction);
                $nKey = $nRow.'_'.$nCol;
                if ( !isset($destinations[$nKey]) && $ants->passable($nRow, $nCol) ) {
                    $ants->issueOrder($aRow, $aCol, $direction);
                    $destinations[$nKey] = 1;
                    continue 2;
                }
            }
            //ant can't move
            $destinations[$aRow.'_'.$aCol] = 1;
        }
    }
}

Ants::run( new RandomBot() );