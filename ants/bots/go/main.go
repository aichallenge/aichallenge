package main

import (
	"flag"
	"image"
	"image/png"
	"os"
	"fmt"
)

//use -imgprefix="bot0" to make a series of images (bot0.0.png ... bot0.N.png) which 
//illustrate the bot's knowledge of the map at each turn. If you want the images in a 
//subdirectory, make sure you create the directory first. (e.g., -imgprefix="images/bot0")
var imageOutPrefix *string = flag.String("imgprefix", "", "prefix for helpful debugging images")

//runs a test to make sure the map object does what we expect
var runTests *bool = flag.Bool("test-suite", false, "set this to run the tests")


//call Panicf to halt the program with a stack trace. Use it like fmt.Sprintf
func Panicf(format string, args ...interface {}) {
	panic(fmt.Sprintf(format, args...))
}


type ImageHelper struct {
	m *Map
	pixel func(row, col int) image.NRGBAColor
}
func (ih ImageHelper) ColorModel() image.ColorModel {
	return image.NRGBAColorModel
}
func (ih ImageHelper) Bounds() image.Rectangle {
	return image.Rect(0, 0, ih.m.Cols * 4, ih.m.Rows * 4)
}
func (ih ImageHelper) At(x, y int) image.Color {
	return ih.pixel(y / 4, x / 4)
}
func (s *State) WriteDebugImage(Desc string, At func(row, col int) image.NRGBAColor) {
	if imageOutPrefix == nil {
		return
	}
	
	fname := fmt.Sprintf("%s.%s.%3.3d.png", *imageOutPrefix, Desc, s.Turn)
	//fmt.Printf("making image: %s\n", fname)
	f, err := os.Open(fname, os.O_WRONLY | os.O_CREATE, 0666)
	if err != nil {
		Panicf("Couldn't open %s (%s)", fname, err)
	}
	defer f.Close()
	err = png.Encode(f, ImageHelper{s.Map, At})
	if err != nil {
		Panicf("Couldn't encode png (%s)", err)
	}
}


//main initializes the state and starts the processing loop
func main() {
	var s State
	
	flag.Parse()
	
	if *runTests {
		//We can't use go's built-in test framework for the main package, 
		//and we can't split it into separate packages because the contest doesn't use makefiles.
		//(I did provide a makefile for your convienience, though)
		TestMap()
		return
	}
	
	err := s.Start()
	if err != nil {
		Panicf("Start() failed (%s)", err)
	}
	
	mb := NewBot(&s)
	
	err = s.Loop(mb, func () {
		
		//I added a mechanism to make customizing image output a lot easier, see 
		//the use of WriteDebugImage in MyBot.go
		//I'll leave this here for reference, since it works also
		/*if *imageOutPrefix != "" {
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
		}*/
		
		//if you want to do other between-turn debugging things, you can do them here
	})
	
	if err != nil && err != os.EOF {
		Panicf("Loop() failed (%s)", err)
	}
}
