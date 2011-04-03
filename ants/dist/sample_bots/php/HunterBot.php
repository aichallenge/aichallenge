<?php

require_once 'Ants.php';

class HunterBot
{
    public function doTurn( $ants )
    {
        $destinations = array();
        foreach( $ants->myAnts as $ant ) {
            list($aRow, $aCol) = $ant;
            
            $targets = array_merge( $ants->food, $ants->enemyAnts);
            // find closest food or enemy ant
            $closest_target = null;
            $closest_distance = 999999;
            foreach ($targets as $target ) {
                list($tRow, $tCol) = $target;
                $dist = $ants->distance($aRow, $aCol, $tRow, $tCol);
                if ($dist < $closest_distance) {
                    $closest_distance = $dist;
                    $closest_target = array($tRow, $tCol);
                }
            }
            if ($closest_target === null) {
                # no target found, mark ant as not moving so we don't run into it
                $destinations []= $ant;
                continue;
            }
            $directions = $ants->direction($aRow, $aCol, $closest_target[0], $closest_target[1]);
            shuffle($directions);
            foreach ($directions as $direction) {
                list( $nRow, $nCol) = $ants->destination($aRow, $aCol, $direction);
                $nKey = $nRow.'_'.$nCol;
                if ($ants->unoccupied($nRow, $nCol) and !isset($destinations[$nKey]) ) {
                    $destinations[$nKey] = 1;
                    $ants->issueOrder($aRow, $aCol, $direction);
                    continue 2;
                }
            }
            //ant can't move
            $destinations[$aRow.'_'.$aCol] = 1;
        }
    }
}

/**
 * Don't run bot when unit-testing
 */
if( !defined('PHPUnit_MAIN_METHOD') ) {
    Ants::run( new HunterBot() );
}