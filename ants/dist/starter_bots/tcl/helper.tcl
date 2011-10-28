# Here are some simple example helper procedures.

proc getMyHills {} {
    return [getTiles {type h} {owner 0}]
}

proc getEnemyHills {} {
    return [getTiles {type h} {owner != 0}]
}

proc getMyAnts {} {
    # Get all ants that belong to me.
    set ants [getTiles {type a} {owner 0}]
    
    # Get my blocked hills, which are an ant and a hill on the same tile but stored as hills with the blocked flag on.
    lappend ants {*}[getTiles {blocked 1} {owner 0}]
    
    return $ants
}

proc getEnemyAnts {} {
    set ants [getTiles {type a} {owner != 0}]
    lappend ants {*}[getTiles {blocked 1} {owner != 0}]
    
    return $ants
}

proc getFoods {} {
    return [getTiles {type f}]
}

# Returns 1 if the specified tile is not water.
proc isPassable {index} {

    if { [getTile $index type] == "w" } {
        return 0
    }
    
    return 1
}


# Returns 1 or 0, indicating whether the provided tile is currently visible.
# This isn't accurate for water tiles but it is very efficient.
proc isVisible {index} {
    global Tile

    if { [dict exists $Tile $index]  } {
        return 1
    }
    
    return 0
}


# Returns a list of tile indices that are visible to an ant located at the given tile index.
proc getVisibleIndices {index} {
    global Parameter
    
    set row      [lindex $index 0]                    ;# The row of the tile index.
    set col      [lindex $index 1]                    ;# The column of the tile index.
    set indices  [list]                               ;# A list of visible indices which this procedure will return.
    set radius2  $Parameter(viewradius2)              ;# The view radius squared, as provided by the server.
    set radius   [expr { int(sqrt($radius2)) }]       ;# The view radius, rounded to an integer towards zero.
    
    # Start with a negative radius and loop through to a positive radius, for both row and column.
    for {set offsetRow -$radius} {$offsetRow <= $radius} {incr offsetRow} {
        for {set offsetCol -$radius} {$offsetCol <= $radius} {incr offsetCol} {
        
            # Pythagorean's forumla. This is the distance the offset row,col is from 0,0. Ie., the hypotenuse squared.
            set distance [expr { pow($offsetRow,2) + pow($offsetCol,2) }]
            
            # I don't remember why this works. My brain hurts.
            if { $distance <= $radius2 } {
                
                # The original row,col plus this offset of each is visible.
                set visibleRow [expr { $row+$offsetRow }]
                set visibleCol [expr { $col+$offsetCol }]
                
                # Allow the index to wrap around when out of bounds.
                set index [wrapIndex "$visibleRow $visibleCol"]
                
                lappend indices $index
            }
        }
    }
    
    return $indices
}


# If row or col are out of range for the map, adjust them so they wrap around.
proc wrapIndex {index} {
    global Parameter
    
    set row [lindex $index 0]
    set col [lindex $index 1]

    if { $row < 0 } {
        incr row $Parameter(rows)
    }
    if { $col < 0 } {
        incr col $Parameter(cols)
    }
    if { $row >= $Parameter(rows) } {
        incr row -$Parameter(rows)
    }
    if { $col >= $Parameter(cols) } {
        incr col -$Parameter(cols)
    }
    
    return "$row $col"
}


