package main

import (
	"os"
	"rand"
	"image"
	//"fmt"
)

type MyBot struct {
}

//NewBot creates a new instance of your bot
func NewBot(s *State) Bot {
	mb := &MyBot{
		//do any necessary initialization here
	}
	
	return mb
}

//DoTurn is where you should do your bot's actual work.
func (mb *MyBot) DoTurn(s *State) os.Error {
	
	//for testing, if you want to see the map:
	//fmt.Println(s.Map)
	
	//if you want to visualize some data, I've provided a helper function.
	//to use this, execute your bot with "-imgprefix xxxx" command line parameter,
	//and then the following will get made into an image called xxxx.fieldofview.turn#.png
	//You can output as many images as you want.
	s.WriteDebugImage("fieldofview", func (row, col int) image.NRGBAColor {
		//to construct an image.NRGBAColor, do this:
		//return image.NRGBAColor{red, green, blue, alpha}
		//where all four values are uint8's (0-255), and alpha represents transparency
		
		//for now, we'll just return the default color for each item.
		return s.Map.Item(s.Map.FromRowCol(row, col)).Color()
	})
	
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

