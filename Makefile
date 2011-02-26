all:
	gcc -O3 -funroll-loops -c -o MyBot.o MyBot.c
	gcc -O3 -funroll-loops -c -o YourCode.o YourCode.c
	gcc -O3 -funroll-loops -c -o ants.o ants.c
	gcc YourCode.o MyBot.o ants.o -lm -o MyBot

clean:
	rm -f ants.o YourCode.o MyBot.o ants.o MyBot
