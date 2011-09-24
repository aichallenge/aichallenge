package main

import (
	"testing"
)

func TestMap(t *testing.T) {
	m := NewMap(4, 3)
	m.Reset()
	if m.String() != `. . . 
. . . 
. . . 
. . . 
` {
		t.Errorf("map is wrong size, got `%s`", m)
	}

	loc := m.FromRowCol(3, 2)
	row, col := m.FromLocation(loc)
	if row != 3 || col != 2 {
		t.Errorf("conversion broken, got (%v, %v), wanted (3, 2)", row, col)
	}

	loc2 := m.FromRowCol(3, -1)
	if loc2 != loc {
		t.Errorf("from xy broken, got (%v), wanted (%v)", loc2, loc)
	}

	n := m.FromRowCol(2, 2)
	s := m.FromRowCol(4, 2)
	e := m.FromRowCol(3, 3)
	w := m.FromRowCol(3, 1)

	if n != m.Move(loc, North) {
		t.Errorf("Move north is broken")
	}
	if s != m.Move(loc, South) {
		t.Errorf("Move south is broken")
	}
	if e != m.Move(loc, East) {
		t.Errorf("Move east is broken")
	}
	if w != m.Move(loc, West) {
		t.Errorf("Move west is broken")
	}

	m.AddAnt(n, MY_ANT)
	m.AddAnt(s, MY_ANT)

	if m.String() != `. . a 
. . . 
. . a 
. . . 
` {
		t.Errorf("map put ants in wrong place, got `%s`", m)
	}
}
