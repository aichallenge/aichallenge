all: CppStarterPackage.zip JavaStarterPackage.zip

clean:
	rm -rf CppStarterPackage JavaStarterPackage *.zip

CppStarterPackage.zip:
	mkdir CppStarterPackage
	cp cpp/MyBot.cc CppStarterPackage
	cp cpp/PlanetWars.cc CppStarterPackage
	cp cpp/PlanetWars.h CppStarterPackage
	cp cpp/Makefile CppStarterPackage
	mkdir CppStarterPackage/Tools
	cp ../viz/PlayGame.jar CppStarterPackage/Tools
	cp ../viz/ShowGame.jar CppStarterPackage/Tools
	zip -r CppStarterPackage.zip CppStarterPackage

JavaStarterPackage.zip:
	mkdir JavaStarterPackage
	cp cpp/MyBot.java JavaStarterPackage
	cp cpp/PlanetWars.java JavaStarterPackage
	cp cpp/Fleet.java JavaStarterPackage
	cp cpp/Planet.java JavaStarterPackage
	mkdir JavaStarterPackage/Tools
	cp ../viz/PlayGame.jar JavaStarterPackage/Tools
	cp ../viz/ShowGame.jar JavaStarterPackage/Tools
	zip -r JavaStarterPackage.zip JavaStarterPackage
