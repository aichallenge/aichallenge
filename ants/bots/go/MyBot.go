package main

import (
	"os"
	"rand"
	//"fmt"
)

type MyBot struct {
}

func (mb *MyBot) DoTurn(s *State) os.Error {
	
	//for testing, if you want to see the map:
	//fmt.Println(s.Map)
	
	dirs := []Direction{North, East, South, West}
	
	for loc, ant := range s.Map.Ants {
		if ant != MY_ANT {
			continue
		}
		
		//try each direction in a random order
		p := rand.Perm(4)
		for _, i := range p {
			d := dirs[i]
			
			loc2 := s.Map.Move(loc, d)
			if s.Map.SafeDestination(loc2) {
				s.IssueOrderLoc(loc, d)
				break
			}
		}
		
	}
	
	//returning an error will halt the whole program!
	return nil
}

