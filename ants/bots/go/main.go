package main

import (
	"flag"
	"image/png"
	"os"
	"fmt"
)

var imageOutPrefix *string = flag.String("imgprefix", "", "prefix for helpful debugging images")

func Panicf(format string, args ...interface {}) {
	panic(fmt.Sprintf(format, args...))
} 


func main() {
	var s State
	
	flag.Parse()
	
	err := s.Start()
	if err != nil {
		Panicf("Start() failed (%s)", err)
	}
	
	mb := &MyBot{}
	
	err = s.Loop(mb, func () {
		if *imageOutPrefix != "" {
			fname := fmt.Sprintf("%s.%d.png", *imageOutPrefix, s.Turn)
			//fmt.Printf("making image: %s\n", fname)
			f, err := os.Open(fname, os.O_WRONLY | os.O_CREATE, 0666)
			if err != nil {
				Panicf("Couldn't open %s (%s)", fname, err)
				return
			}
			defer f.Close()
			err = png.Encode(f, s.Map)
			if err != nil {
				Panicf("Couldn't encode png (%s)", err)
			}
		}
	})
	
	if err != nil && err != os.EOF {
		Panicf("Loop() failed (%s)", err)
	}
}