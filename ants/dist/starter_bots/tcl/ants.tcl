# Ants, aichallenge.org
# This file handles the input, output, and game state.
# by paulwal


# This is a dictionary of the game board's state.  
# It stores visible and remembered tiles on the game board.
# Each tile is indexed by the row and column as a two-element list: "$row $col". 
# The value of each tile in the dict is a nested dictionary of attributes(type, owner, blocked, l̶a̶s̶t̶s̶e̶e̶n̶) where 'blocked' may equal 1 if the type is a hill with an ant on it.
# If there is no owner, for instance for a food tile, then -1 is used for the owner.
set Tile [dict create]

# This is an array of the game's parameters: like loadtime, turntime, viewradius2, turn number, etc.
array set Parameter [list]


# Output a line to the parent process, ie., the game server.
proc output {line} {
    puts stdout $line
    flush stdout
    return
}

# Continuously accept input from the server.
proc inputLoop {} {

    # Loop until the process is killed.
    while 1 {

        # Read one line from standard input.
        gets stdin line
        
        # Decided what to do with this input.
        switch -glob -- $line {
                    
            "ready" { # Initialize. 
                      # This procedure should be defined elsewhere.                      
                      initialize
                      output "go"
                    }
                    
            "go"    { # Do turn. 
                      # This procedure should be defined elsewhere.
                      doTurn
                      
                      # End the turn.
                      output "go"
                      
                      # Unset any tiles that are not water.
                      unsetNonWaterTiles          
                    }
                    
            "end"   { # The game is over. 
                      # This procedure should be defined elsewhere.
                      endGame
                    }
                    
            default { # Adjust game state. 
                      processInput $line
                    }
        }
    }
    
    return
}


# Process input from the server.
proc processInput {line} {
    global Parameter
    
    # The type of message.
    set type  [lindex $line 0]
    
    # A list of values included in the message, or a single value if only one is present.
    set value [lrange $line 1 end]

    # For messages related to the state of tiles on the game board, row and col are the first two values, 
    # and for some messages 'owner' is a third value.
    set row   [lindex $value 0]
    set col   [lindex $value 1]
    set owner [lindex $value 2]
    
    switch -- $type {
        "w"            { setTile $row $col w -1             ;# Water tile.                  }
        "f"            { setTile $row $col f -1             ;# Food location.               }
        "h"            { setTile $row $col h $owner         ;# Hill location.               }
        "a"            { setTile $row $col a $owner         ;# An ant is here.              }
        "d"            { setTile $row $col d $owner         ;# An ant died here last turn.  }
        default        { set Parameter($type) $value        ;# This is a game parameter.    }
    }

    return
}


proc unsetTile {index} {
    global Tile
    dict unset Tile $index
    return
}


# Define the tile at a row and column.
# An existing row,col tile will be overwritten, unless an ant and a hill are in the same tile position, in which case it will be saved as a hill that is blocked.
proc setTile {row col type owner} {
    global Tile

    # Create a dict for this tile's attributes.
    set attributes [dict create  \
        type        $type        \
        owner       $owner       \
        blocked     0            \
        lastseen    0            \
    ]
    
    set index "$row $col"
    
    # Check if this is a blocked hill.
    # If this tile is being set to a hill or an ant... Then check if there is already a hill or ant on this tile f̶r̶o̶m̶ ̶t̶h̶i̶s̶ ̶t̶u̶r̶n̶.
    if {  $type == "h"  ||  $type == "a"  } {
        
        # Check if this tile exists in the Tile dictionary.
        if { [dict exists $Tile $index] } {
        
            # Check if it is an ant or hill type.
            if {  [dict get $Tile $index type] == "a"  ||  [dict get $Tile $index type] == "h"  } {
            
                # This is a blocked hill! (ant+hill on the same tile)
                dict set attributes type     "h"
                dict set attributes blocked  1
            }
        }
    }
    
    # Nest the attributes dict in the Tile dict.
    dict set Tile $index $attributes
        
    return
}


# Get one or all attributes of a tile.
# getTile "row col" ?attribute?
# Returns the requested attribute or all attributes as a key-value list.
# Attributes: type, owner, blocked, l̶a̶s̶t̶s̶e̶e̶n
# type = w, f, h, a, or d (water, food, hill, ant, or dead ant)
# owner = ID of player if type is h, a, or d.  -1 otherwise.  0 = self
# blocked = 1, if the type is a hill with an ant on it. 0 otherwise.
# l̶a̶s̶t̶s̶e̶e̶n̶ ̶=̶ ̶T̶h̶e̶ ̶n̶u̶m̶b̶e̶r̶ ̶o̶f̶ ̶t̶u̶r̶n̶s̶ ̶a̶g̶o̶ ̶t̶h̶i̶s̶ ̶t̶i̶l̶e̶ ̶w̶a̶s̶ ̶s̶e̶e̶n̶,̶ ̶0̶ ̶=̶ ̶t̶h̶i̶s̶ ̶t̶u̶r̶n̶.̶ ̶T̶h̶i̶s̶ ̶i̶s̶ ̶u̶s̶e̶d̶ ̶t̶o̶ ̶t̶r̶a̶c̶k̶ ̶w̶h̶e̶n̶ ̶a̶ ̶t̶i̶l̶e̶ ̶w̶a̶s̶ ̶l̶a̶s̶t̶ ̶s̶e̶e̶n̶ ̶t̶o̶ ̶a̶s̶s̶e̶s̶s̶ ̶h̶o̶w̶ ̶a̶c̶c̶u̶r̶a̶t̶e̶ ̶t̶h̶a̶t̶ ̶i̶n̶f̶o̶r̶m̶a̶t̶i̶o̶n̶ ̶i̶s̶ ̶l̶i̶k̶e̶l̶y̶ ̶t̶o̶ ̶b̶e̶.̶ ̶A̶ ̶f̶o̶o̶d̶ ̶t̶i̶l̶e̶ ̶s̶e̶e̶n̶ ̶t̶h̶i̶s̶ ̶t̶u̶r̶n̶ ̶i̶s̶ ̶d̶e̶f̶i̶n̶i̶t̶e̶l̶y̶ ̶t̶h̶e̶r̶e̶,̶ ̶b̶u̶t̶ ̶o̶n̶e̶ ̶s̶e̶e̶n̶ ̶f̶i̶v̶e̶ ̶t̶u̶r̶n̶s̶ ̶a̶g̶o̶ ̶m̶a̶y̶ ̶b̶e̶ ̶g̶o̶n̶e̶ ̶b̶y̶ ̶n̶o̶w̶.̶
proc getTile {index args} {
    global Tile
    
    # If an attribute is not provided, all attributes will be returned as a key-value list.
    if { [dict exists $Tile $index {*}$args] } {
        return [dict get $Tile $index {*}$args]
    }
    
    return
}


# Returns a list of indices for all tiles that match the given key-value pairs provided.
# An operator is optional to use something other than the default equals operator (==).
# Example:
#    getTiles {type h} {owner != 0}
proc getTiles {args} {
    global Tile

    set indices [list]
    
    dict for {index attributes} $Tile {
        set flag 0

        foreach comparison $args {
            set key       [lindex $comparison 0]
            set value     [lindex $comparison end]
            set operator "=="
            
            if { [llength $comparison] == 3 } {
                set operator [lindex $comparison 1]
            }
            
            set bool [expr \"[dict get $attributes $key]\" $operator \"$value\"]
        
            if $bool {
                set flag 1
                
            } else {
                set flag 0
                break
            }
        }
        
        if { $flag == 1 } {
            lappend indices $index
        }
    }
    
    return $indices
}


# Unset any tile that isn't water.
proc unsetNonWaterTiles {} {

    foreach index [getTiles {type != w}] {
        unsetTile $index
    }
    
    return
}
