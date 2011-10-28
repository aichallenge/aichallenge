# Ants, aichallenge.org
# This file implements simple gameplay logic.
# by paulwal

source ants.tcl      ;# This file handles the input, output, and game state.  The procedure inputLoop must be called at the end of this file to start the input loop.
source helper.tcl    ;# Some helper procedures.


# This procedure is called after "ready" is received from the game server.
# A "go" message is automatically sent after this procedure returns.
proc initialize {} {
    return
}

# This is called after "end" is received from the server to indicate that the game has ended.
proc endGame {} {
    return
}

# This is called after the state of visible tiles for this turn has been received.
# A "go" message is automatically sent after this procedure returns.
proc doTurn {} {

    # Loop through each ant.
    foreach index [getMyAnts] {
    
        # The 'index' is a two-element list of row and col values for a tile.
        set row [lindex $index 0]
        set col [lindex $index 1]

        
        # Check adjacent tiles in this order: north, east, south, west.
        foreach {direction offsetRow offsetCol} {N -1 0   E 0 1   S 1 0   W 0 -1} {
    
    
            # This is the index of the adjacent tile.
            incr offsetRow $row
            incr offsetCol $col
            set index "$offsetRow $offsetCol"
    
    
            # The index could be out of bounds, so allow for wrap-around map edges.
            set index [wrapIndex $index]
    
    
            # If this adjacent tile is not water, attempt to move there and continue with the next ant.
            if { [isPassable $index] } {
            
                # Instruct the game server to move ant from $row,$col in this direction.
                output "o $row $col $direction"
                
                # Uncomment this line to see some debug info:
                #puts stderr "turn $::Parameter(turn), moving ant to $index"
                
                # Next!
                break
            }
        }
    }


    return
}


# The input loop must be started.
inputLoop
