package main

import (
	"os"
	"bufio"
	"strconv"
	"strings"
	"fmt"
)

type Bot interface {
	DoTurn(s *State) os.Error
}

var stdin = bufio.NewReader(os.Stdin)

type State struct {
	LoadTime		int	//in milliseconds
	TurnTime		int	//in milliseconds
	Rows			int	//number of rows in the map
	Cols			int	//number of columns in the map
	Turns			int	//maximum number of turns in the game
	ViewRadius2		int	//view radius squared
	AttackRadius2	int	//battle radius squared
	SpawnRadius2	int	//spawn radius squared
	
	Turn			int //current turn number
	
	Map				*Map
}

func (s *State) Start() os.Error {
	
	for {
		line, err := stdin.ReadString('\n')
		if err != nil {
			return err
		}
		line = line[:len(line) - 1] //remove the delimiter
		
		if line == "" {
			continue
		}
		
		if line == "ready" {
			break
		}
		
		words := strings.Split(line, " ", 2)
		if len(words) != 2 {
			panic(`"` + line + `"`)
			return os.NewError("invalid command format: " + line)
		}
		
		param, _ := strconv.Atoi(words[1])
		
		switch words[0] {
			case "loadtime":		s.LoadTime = param
			case "turntime":		s.TurnTime = param
			case "rows":			s.Rows = param
			case "cols":			s.Cols  = param
			case "turns":			s.Turns = param
			case "viewradius2":		s.ViewRadius2 = param
			case "attackradius2":	s.AttackRadius2 = param
			case "spawnradius2":	s.SpawnRadius2 = param
			
			case "turn":			s.Turn = param
			
			default:
				panic(1)
				return os.NewError("unknown command: " + line)
		}
	}
	
	s.Map = NewMap(s.Rows, s.Cols)
	
	
	return nil
}


func (s *State) Loop(b Bot) os.Error {
	
	//indicate we're ready
	os.Stdout.Write([]byte("go\n"))
	
	
	for {
		line, err := stdin.ReadString('\n')
		if err != nil {
			panic(2)
			return err
		}
		line = line[:len(line) - 1] //remove the delimiter
		
		if line == "" {
			continue
		}
		
		if line == "go" {
			b.DoTurn(s)
			
			//end turn
			s.endTurn()
			
			s.Map.Reset()
			continue
		}
		
		if line == "end" {
			break
		}
		
		words := strings.Split(line, " ", 5)
		if len(words) < 2 {
			panic(3)
			return os.NewError("invalid command format: " + line)
		}
		
		switch words[0] {
			case "turn":
				turn, _ := strconv.Atoi(words[1])
				if turn != s.Turn + 1 {
					panic(4)
					return os.NewError("Somehow turn number got out of sync")
				}
				s.Turn = turn
			case "f":
				if len(words) < 3 {
					panic("not enough parameters for food!")
				}
				X, _ := strconv.Atoi(words[1])
				Y, _ := strconv.Atoi(words[2])
				loc := s.Map.FromXY(X, Y)
				s.Map.AddFood(loc)
			case "w":
				if len(words) < 3 {
					panic("not enough parameters for water!")
				}
				X, _ := strconv.Atoi(words[1])
				Y, _ := strconv.Atoi(words[2])
				loc := s.Map.FromXY(X, Y)
				s.Map.AddWater(loc)
			case "a":
				if len(words) < 4 {
					panic("not enough parameters for ant!")
				}
				X, _ := strconv.Atoi(words[1])
				Y, _ := strconv.Atoi(words[2])
				Ant, _ := strconv.Atoi(words[3])
				loc := s.Map.FromXY(X, Y)
				s.Map.AddAnt(loc, Item(Ant))
				
				if Item(Ant) == MY_ANT {
					s.Map.AddLand(loc, s.ViewRadius2)
				}
			case "d":
				if len(words) < 4 {
					panic("not enough parameters for dead ant!")
				}
				X, _ := strconv.Atoi(words[1])
				Y, _ := strconv.Atoi(words[2])
				Ant, _ := strconv.Atoi(words[3])
				loc := s.Map.FromXY(X, Y)
				s.Map.AddDeadAnt(loc, Item(Ant))
			
		}
	}
	
	return nil
}

func (s *State) IssueOrderXY(X, Y int, d Direction) {
	loc := s.Map.FromXY(X, Y)
	dest := s.Map.Move(loc, d)
	s.Map.AddDestination(dest)
	fmt.Fprintf(os.Stdout, "o %d %d %s\n", X, Y, d)
}

func (s *State) IssueOrderLoc(loc Location, d Direction) {
	X, Y := s.Map.FromLocation(loc)
	dest := s.Map.Move(loc, d)
	s.Map.AddDestination(dest)
	fmt.Fprintf(os.Stdout, "o %d %d %s\n", X, Y, d)
}

func (s *State) endTurn() {
	os.Stdout.Write([]byte("go\n"))
}


