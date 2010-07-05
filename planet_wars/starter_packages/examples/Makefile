all: BullyBot.jar DualBot.jar ProspectorBot.jar RageBot.jar RandomBot.jar

BullyBot.class:
	javac BullyBot.java

BullyBot.jar: BullyBot.class
	jar cfe BullyBot.jar BullyBot BullyBot.class Fleet.class Planet.class PlanetWars.class

clean:
	rm -rf *.class *.jar

DualBot.class:
	javac DualBot.java

DualBot.jar: DualBot.class
	jar cfe DualBot.jar DualBot DualBot.class Fleet.class Planet.class PlanetWars.class

ExpandBot.class:
	javac ExpandBot.java

ExpandBot.jar: ExpandBot.class
	jar cfe ExpandBot.jar ExpandBot ExpandBot.class Fleet.class Planet.class PlanetWars.class

ProspectorBot.class:
	javac ProspectorBot.java

ProspectorBot.jar: ProspectorBot.class
	jar cfe ProspectorBot.jar ProspectorBot ProspectorBot.class Fleet.class Planet.class PlanetWars.class

RageBot.class:
	javac RageBot.java

RageBot.jar: RageBot.class
	jar cfe RageBot.jar RageBot RageBot.class Fleet.class Planet.class PlanetWars.class

RandomBot.class:
	javac RandomBot.java

RandomBot.jar: RandomBot.class
	jar cfe RandomBot.jar RandomBot RandomBot.class Fleet.class Planet.class PlanetWars.class
