package main

import (
	"flag"
	"image/png"
	"os"
	"fmt"
)

//use -imgprefix="bot0" to make a series of images (bot0.0.png ... bot0.N.png) which 
//illustrate the bot's knowledge of the map at each turn. If you want the images in a 
//subdirectory, make sure you create the directory first. (e.g., -imgprefix="images/bot0")
var imageOutPrefix *string = flag.String("imgprefix", "", "prefix for helpful debugging images")


//call Panicf to halt the program with a stack trace. Use it like fmt.Sprintf
func Panicf(format string, args ...interface {}) {
	panic(fmt.Sprintf(format, args...))
} 


//main initializes the state and starts the processing loop
func main() {
	var s State
	
	flag.Parse()
	
	err := s.Start()
	if err != nil {
		Panicf("Start() failed (%s)", err)
	}
	
	mb := NewBot()
	
	err = s.Loop(mb, func () {
		if *imageOutPrefix != "" {
			fname := fmt.Sprintf("%s.%d.png", *imageOutPrefix, s.Turn)
			//fmt.Printf("making image: %s\n", fname)
			f, err := os.Open(fname, os.O_WRONLY | os.O_CREATE, 0666)
			if err != nil {
				Panicf("Couldn't open %s (%s)", fname, err)
			}
			defer f.Close()
			err = png.Encode(f, s.Map)
			if err != nil {
				Panicf("Couldn't encode png (%s)", err)
			}
		}
		
		//if you want to do other between-turn debugging things, this is where to do them
	})
	
	if err != nil && err != os.EOF {
		Panicf("Loop() failed (%s)", err)
	}
}