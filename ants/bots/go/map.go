package main

import (
	"image"
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
		case UNKNOWN:	return '.'
		case WATER:		return '%'
		case FOOD:		return '*'
		case LAND:		return ' '
		case DEAD:		return '!'
	}
	
	if o < MY_ANT || o > PLAYER25 {
		Panicf("invalid item: %v", o)
	}
	
	return byte(o) + 'a'
}

//FromSymbol reverses Symbol
func FromSymbol(ch byte) Item {
	switch ch {
		case '.':	return UNKNOWN
		case '%':	return WATER
		case '*':	return FOOD
		case ' ':	return LAND
		case '!':	return DEAD
	}
	if ch < 'a' || ch > 'z' {
		Panicf("invalid item symbol: %v", ch)
	}
	return Item(ch) + 'a'
}

//Location combines (X, Y) coordinate pairs for use as keys in maps (and in a 1d array)
type Location int

type Map struct {
	Rows int
	Cols int
	
	data []Item
	
	Ants map[Location]Item
	Dead map[Location]Item
	Water map[Location]Item
	Food map[Location]Item
	Destinations map[Location]bool
}

//String implements the stringizer (stringer?) interface. It returns an ascii diagram of the map.
func (m *Map) String() string {
	str := ""
	for j := 0; j < m.Rows; j++ {
		for i := 0; i < m.Cols; i++ {
			s := m.data[j * m.Cols + i].Symbol()
			str += string([]byte{s}) + " "
		}
		str += "\n"
	}
	return str
}

//NewMap returns a newly constructed blank map.
func NewMap(Cols, Rows int) *Map {
	m := &Map{
		Rows: Rows, 
		Cols: Cols,
		Water: make(map[Location]Item),
		data: make([]Item, Rows * Cols),
	}
	m.Reset()
	return m
}

//Reset clears the map (except for water) for the next turn
func (m *Map) Reset() {
	for i := range m.data {
		m.data[i] = UNKNOWN
	}
	for i, val := range m.Water {
		m.data[i] = val
	}
	m.Ants = make(map[Location]Item)
	m.Dead = make(map[Location]Item)
	m.Food = make(map[Location]Item)
	m.Destinations = make(map[Location]bool)
}

func (m *Map) AddWater(loc Location) {
	m.Water[loc] = WATER
	m.data[loc] = WATER
}

func (m *Map) AddAnt(loc Location, ant Item) {
	m.Ants[loc] = ant
	m.data[loc] = ant
}

//AddLand adds a circle of land centered on the given location
func (m *Map) AddLand(center Location, viewrad2 int) {
	x1, y1 := m.FromLocation(center)
	for x := 0; x < m.Cols; x++ {
		for y := 0; y < m.Rows; y++ {
			loc := m.FromXY(x, y)
			if m.data[loc] != UNKNOWN {
				continue
			}
			xΔ := x - x1
			yΔ := y - y1
			if xΔ * xΔ + yΔ * yΔ < viewrad2 {
				m.data[loc] = LAND
			}
		}
	}
}

func (m *Map) AddDeadAnt(loc Location, ant Item) {
	m.Dead[loc] = ant
	m.data[loc] = DEAD
}

func (m *Map) AddFood(loc Location) {
	m.Food[loc] = FOOD
	m.data[loc] = FOOD
}

func (m *Map) AddDestination(loc Location) {
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

//FromXY returns a Location given an (X, Y) pair
func (m *Map) FromXY(X, Y int) Location {
	for X < 0 {X += m.Cols}
	for X >= m.Cols {X -= m.Cols}
	for Y < 0 {Y += m.Rows}
	for Y >= m.Rows {Y -= m.Rows}
	
	return Location(Y * m.Cols + X)
}

//FromLocation returns an (X, Y) pair given a Location
func (m *Map) FromLocation(loc Location) (X, Y int) {
	X = int(loc) % m.Cols
	Y = int(loc) / m.Cols
	return
}

//implement Image for fancy image debugging
func (m *Map) ColorModel() image.ColorModel {
	return image.NRGBAColorModel
}
func (m *Map) Bounds() image.Rectangle {
	return image.Rect(0, 0, m.Cols * 4, m.Rows * 4)
}
func (m *Map) At(x, y int) image.Color {
	loc := m.FromXY(x / 4, y / 4)
	switch m.data[loc] {
		case UNKNOWN:	return image.NRGBAColor{0xb0, 0xb0, 0xb0, 0xff}
		case WATER: 	return image.NRGBAColor{0x10, 0x10, 0x50, 0xff}
		case FOOD:		return image.NRGBAColor{0xe0, 0xe0, 0xc0, 0xff}
		case LAND:		return image.NRGBAColor{0x8b, 0x45, 0x13, 0xff}
		case DEAD:		return image.NRGBAColor{0x69, 0x33, 0x05, 0xff}
		case MY_ANT:	return image.NRGBAColor{0xf0, 0x00, 0x00, 0xff}
		case PLAYER1:	return image.NRGBAColor{0xf0, 0xf0, 0x00, 0xff}
		case PLAYER2:	return image.NRGBAColor{0x00, 0xf0, 0x00, 0xff}
		case PLAYER3:	return image.NRGBAColor{0x00, 0x00, 0xf0, 0xff}
		case PLAYER4:	return image.NRGBAColor{0xf0, 0x00, 0xf0, 0xff}
		case PLAYER5:	return image.NRGBAColor{0xf0, 0xf0, 0xf0, 0xff}
		case PLAYER6:	return image.NRGBAColor{0x80, 0x80, 0x00, 0xff}
		case PLAYER7:	return image.NRGBAColor{0x00, 0x80, 0x80, 0xff}
		case PLAYER8:	return image.NRGBAColor{0x80, 0x00, 0x80, 0xff}
		case PLAYER9:	return image.NRGBAColor{0x80, 0x00, 0xf0, 0xff}
	}
	return image.NRGBAColor{0x00, 0x00, 0x00, 0xff}
}


//Direction represents the direction concept for issuing orders.
type Direction int
const (
	North Direction = iota
	East
	South
	West
)

func (d Direction) String() string {
	switch d {
		case North:	return "n"
		case South:	return "s"
		case West:	return "w"
		case East:	return "e"
	}
	Panicf("%v is not a valid direction", d)
	return ""
}

//Move returns a new location which is one step in the specified direction from the specified location.
func (m *Map) Move(loc Location, d Direction) Location {
	X, Y := m.FromLocation(loc)
	switch d {
		case North:		Y -= 1
		case South:		Y += 1
		case West:		X -= 1
		case East:		X += 1
		default: Panicf("%v is not a valid direction", d)
	}
	return m.FromXY(X, Y) //this will handle wrapping out-of-bounds numbers
}


