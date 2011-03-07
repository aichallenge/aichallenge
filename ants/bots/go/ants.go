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

//b's DoWork function gets called each turn after the map has been setup
//BetweenTurnWork gets called after a turn but before the map is reset. It is 
//meant to do debugging work.
func (s *State) Loop(b Bot, BetweenTurnWork func()) os.Error {
	
	//indicate we're ready
	os.Stdout.Write([]byte("go\n"))
	
	
	for {
		line, err := stdin.ReadString('\n')
		if err != nil {
			if err == os.EOF {
				return err
			}
			Panicf("ReadString returns an error: %s", err)
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
			
			BetweenTurnWork()
				
			s.Map.Reset()
			continue
		}
		
		if line == "end" {
			break
		}
		
		words := strings.Split(line, " ", 5)
		if len(words) < 2 {
			Panicf("Invalid command format: \"%s\"", line)
		}
		
		switch words[0] {
			case "turn":
				turn, _ := strconv.Atoi(words[1])
				if turn != s.Turn + 1 {
					Panicf("Turn number out of sync, expected %v got %v", s.Turn + 1, turn)
				}
				s.Turn = turn
			case "f":
				if len(words) < 3 {
					Panicf("Invalid command format (not enough parameters for food): \"%s\"", line)
				}
				X, _ := strconv.Atoi(words[1])
				Y, _ := strconv.Atoi(words[2])
				loc := s.Map.FromXY(X, Y)
				s.Map.AddFood(loc)
			case "w":
				if len(words) < 3 {
					Panicf("Invalid command format (not enough parameters for water): \"%s\"", line)
				}
				X, _ := strconv.Atoi(words[1])
				Y, _ := strconv.Atoi(words[2])
				loc := s.Map.FromXY(X, Y)
				s.Map.AddWater(loc)
			case "a":
				if len(words) < 4 {
					Panicf("Invalid command format (not enough parameters for ant): \"%s\"", line)
				}
				X, _ := strconv.Atoi(words[1])
				Y, _ := strconv.Atoi(words[2])
				Ant, _ := strconv.Atoi(words[3])
				loc := s.Map.FromXY(X, Y)
				s.Map.AddAnt(loc, Item(Ant))
				s.Map.AddDestination(loc)
				
				if Item(Ant) == MY_ANT {
					s.Map.AddLand(loc, s.ViewRadius2)
				}
			case "d":
				if len(words) < 4 {
					Panicf("Invalid command format (not enough parameters for dead ant): \"%s\"", line)
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
	s.Map.RemoveDestination(loc)
	s.Map.AddDestination(dest)
	fmt.Fprintf(os.Stdout, "o %d %d %s\n", X, Y, d)
}

func (s *State) IssueOrderLoc(loc Location, d Direction) {
	X, Y := s.Map.FromLocation(loc)
	dest := s.Map.Move(loc, d)
	s.Map.RemoveDestination(loc)
	s.Map.AddDestination(dest)
	fmt.Fprintf(os.Stdout, "o %d %d %s\n", X, Y, d)
}

func (s *State) endTurn() {
	os.Stdout.Write([]byte("go\n"))
}


