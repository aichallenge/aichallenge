package main

import (
	"log"
)

//Item represents all the various items that may be on the map
type Item int8

const (
	UNKNOWN Item = iota - 5
	WATER
	FOOD
	LAND
	DEAD
	MY_ANT //= 0
	PLAYER1
	PLAYER2
	PLAYER3
	PLAYER4
	PLAYER5
	PLAYER6
	PLAYER7
	PLAYER8
	PLAYER9
	PLAYER10
	PLAYER11
	PLAYER12
	PLAYER13
	PLAYER14
	PLAYER15
	PLAYER16
	PLAYER17
	PLAYER18
	PLAYER19
	PLAYER20
	PLAYER21
	PLAYER22
	PLAYER23
	PLAYER24
	PLAYER25
)

//Symbol returns the symbol for the ascii diagram
func (o Item) Symbol() byte {
	switch o {
	case UNKNOWN:
		return '.'
	case WATER:
		return '%'
	case FOOD:
		return '*'
	case LAND:
		return ' '
	case DEAD:
		return '!'
	}

	if o < MY_ANT || o > PLAYER25 {
		log.Panicf("invalid item: %v", o)
	}

	return byte(o) + 'a'
}

//FromSymbol reverses Symbol
func FromSymbol(ch byte) Item {
	switch ch {
	case '.':
		return UNKNOWN
	case '%':
		return WATER
	case '*':
		return FOOD
	case ' ':
		return LAND
	case '!':
		return DEAD
	}
	if ch < 'a' || ch > 'z' {
		log.Panicf("invalid item symbol: %v", ch)
	}
	return Item(ch) + 'a'
}


//Location combines (Row, Col) coordinate pairs for use as keys in maps (and in a 1d array)
type Location int

type Map struct {
	Rows int
	Cols int

	itemGrid []Item

	Ants         map[Location]Item
	Dead         map[Location]Item
	Water        map[Location]bool
	Food         map[Location]bool
	Destinations map[Location]bool
}

//NewMap returns a newly constructed blank map.
func NewMap(Rows, Cols int) *Map {
	m := &Map{
		Rows:     Rows,
		Cols:     Cols,
		Water:    make(map[Location]bool),
		itemGrid: make([]Item, Rows*Cols),
	}
	m.Reset()
	return m
}

//String returns an ascii diagram of the map.
func (m *Map) String() string {
	str := ""
	for row := 0; row < m.Rows; row++ {
		for col := 0; col < m.Cols; col++ {
			s := m.itemGrid[row*m.Cols+col].Symbol()
			str += string([]byte{s}) + " "
		}
		str += "\n"
	}
	return str
}

//Reset clears the map (except for water) for the next turn
func (m *Map) Reset() {
	for i := range m.itemGrid {
		m.itemGrid[i] = UNKNOWN
	}
	for i, val := range m.Water {
		if val {
			m.itemGrid[i] = WATER
		}
	}
	m.Ants = make(map[Location]Item)
	m.Dead = make(map[Location]Item)
	m.Food = make(map[Location]bool)
	m.Destinations = make(map[Location]bool)
}

//Item returns the item at a given location
func (m *Map) Item(loc Location) Item {
	return m.itemGrid[loc]
}

func (m *Map) AddWater(loc Location) {
	m.Water[loc] = true
	m.itemGrid[loc] = WATER
}

func (m *Map) AddAnt(loc Location, ant Item) {
	m.Ants[loc] = ant
	m.itemGrid[loc] = ant
}

//AddLand adds a circle of land centered on the given location
func (m *Map) AddLand(center Location, viewrad2 int) {
	m.DoInRad(center, viewrad2, func(row, col int) {
		loc := m.FromRowCol(row, col)
		if m.itemGrid[loc] == UNKNOWN {
			m.itemGrid[loc] = LAND
		}
	})
}

func (m *Map) DoInRad(center Location, rad2 int, Action func(row, col int)) {
	row1, col1 := m.FromLocation(center)
	for row := row1 - m.Rows/2; row < row1+m.Rows/2; row++ {
		for col := col1 - m.Cols/2; col < col1+m.Cols/2; col++ {
			row_delta := row - row1
			col_delta := col - col1
			if row_delta*row_delta+col_delta*col_delta < rad2 {
				Action(row, col)
			}
		}
	}
}

func (m *Map) AddDeadAnt(loc Location, ant Item) {
	m.Dead[loc] = ant
	m.itemGrid[loc] = DEAD
}

func (m *Map) AddFood(loc Location) {
	m.Food[loc] = true
	m.itemGrid[loc] = FOOD
}

func (m *Map) AddDestination(loc Location) {
	if m.Destinations[loc] {
		log.Panicf("Already have something at that destination!")
	}
	m.Destinations[loc] = true
}

func (m *Map) RemoveDestination(loc Location) {
	m.Destinations[loc] = false, false
}

//SafeDestination will tell you if the given location is a 
//safe place to dispatch an ant. It considers water and both
//ants that have already sent an order and those that have not.
func (m *Map) SafeDestination(loc Location) bool {
	if _, exists := m.Water[loc]; exists {
		return false
	}
	if occupied := m.Destinations[loc]; occupied {
		return false
	}
	return true
}

//FromRowCol returns a Location given an (Row, Col) pair
func (m *Map) FromRowCol(Row, Col int) Location {
	for Row < 0 {
		Row += m.Rows
	}
	for Row >= m.Rows {
		Row -= m.Rows
	}
	for Col < 0 {
		Col += m.Cols
	}
	for Col >= m.Cols {
		Col -= m.Cols
	}

	return Location(Row*m.Cols + Col)
}

//FromLocation returns an (Row, Col) pair given a Location
func (m *Map) FromLocation(loc Location) (Row, Col int) {
	Row = int(loc) / m.Cols
	Col = int(loc) % m.Cols
	return
}


//Direction represents the direction concept for issuing orders.
type Direction int

const (
	North Direction = iota
	East
	South
	West

	NoMovement
)

func (d Direction) String() string {
	switch d {
	case North:
		return "n"
	case South:
		return "s"
	case West:
		return "w"
	case East:
		return "e"
	case NoMovement:
		return "-"
	}
	log.Panicf("%v is not a valid direction", d)
	return ""
}

//Move returns a new location which is one step in the specified direction from the specified location.
func (m *Map) Move(loc Location, d Direction) Location {
	Row, Col := m.FromLocation(loc)
	switch d {
	case North:
		Row -= 1
	case South:
		Row += 1
	case West:
		Col -= 1
	case East:
		Col += 1
	case NoMovement: //do nothing
	default:
		log.Panicf("%v is not a valid direction", d)
	}
	return m.FromRowCol(Row, Col) //this will handle wrapping out-of-bounds numbers
}
