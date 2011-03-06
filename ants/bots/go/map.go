package main

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

func (o Item) Symbol() byte {
	switch o {
		case UNKNOWN:	return '.'
		case WATER:		return '%'
		case FOOD:		return '*'
		case LAND:		return ' '
		case DEAD:		return '!'
	}
	
	if o < MY_ANT || o > PLAYER25 {
		panic("invalid item")
	}
	
	return byte(o) + 'a'
}

func FromSymbol(ch byte) Item {
	switch ch {
		case '.':	return UNKNOWN
		case '%':	return WATER
		case '*':	return FOOD
		case ' ':	return LAND
		case '!':	return DEAD
	}
	if ch < 'a' || ch > 'z' {
		panic("invalid item symbol")
	}
	return Item(ch) + 'a'
}

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

func NewMap(Cols, Rows int) *Map {
	m := &Map{
		Rows: Rows, 
		Cols: Cols,
		Ants: make(map[Location]Item),
		Dead: make(map[Location]Item),
		Food: make(map[Location]Item),
		Water: make(map[Location]Item),
		Destinations: make(map[Location]bool),
		data: make([]Item, Rows * Cols),
	}
	return m
}

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

func (m *Map) FromXY(X, Y int) Location {
	for X < 0 {X += m.Cols}
	for X >= m.Cols {X -= m.Cols}
	for Y < 0 {Y += m.Rows}
	for Y >= m.Rows {Y -= m.Rows}
	
	return Location(Y * m.Cols + X)
}

func (m *Map) FromLocation(loc Location) (X, Y int) {
	X = int(loc) % m.Cols
	Y = int(loc) / m.Cols
	return
}

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
	panic("not a valid direction")
	return ""
}

func (m *Map) Move(loc Location, d Direction) Location {
	X, Y := m.FromLocation(loc)
	switch d {
		case North:		Y -= 1
		case South:		Y += 1
		case West:		X -= 1
		case East:		X += 1
		default: panic("not a valid direction")
	}
	return m.FromXY(X, Y) //this will handle wrapping out-of-bounds numbers
}


