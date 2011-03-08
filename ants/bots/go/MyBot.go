package main

import (
	"os"
	"rand"
	//"fmt"
)

type MyBot struct {
}

//NewBot creates a new instance of your bot
func NewBot() Bot {
	mb := &MyBot{
		//do any necessary initialization here
	}
	
	return mb
}

//DoTurn is where you should do your bot's actual work.
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
				//there's also an s.IssueOrderRowCol if you don't have a Location handy
				break
			}
		}
		
	}
	
	//returning an error will halt the whole program!
	return nil
}

