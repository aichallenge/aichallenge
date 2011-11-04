ALL:
	gcc-4.4 -c -g -O2 -I ants.ads -gnatA mybot.adb
	gcc-4.4 -c -g -O2 -I ants.ads -gnatA ants.adb
	gcc-4.4 -c -g -O2 -I ants.ads -gnatA strategy.adb
	gnatbind mybot.ali
	gnatlink mybot.ali -o MyBot
clean:
	rm ants.ali ants.o MyBot mybot.ali mybot.o strategy.ali strategy.o
