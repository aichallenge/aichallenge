package main

import (
	"os"
	"rand"
	"fmt"
)

type MyBot struct {
}

func (mb *MyBot) DoTurn(s *State) os.Error {
	
	fmt.Println(s.Map)
	
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
			_, water := s.Map.Water[loc2]
			if !s.Map.Destinations[loc2] && !water {
				s.IssueOrderLoc(loc, d)
				break
			}
		}
		
	}
	
	//returning an error will halt the whole program!
	return nil
}



func main() {
	var s State
	
	err := s.Start()
	if err != nil {
		panic(err)
	}
	
	mb := &MyBot{}
	
	err = s.Loop(mb)
	if err != nil {
		panic(err)
	}
}
