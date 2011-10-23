ALL:
	gcc -c -g -O2 -I ants.ads -gnatA mybot.adb          
	gcc -c -g -O2 -I ants.ads -gnatA ants.adb
	gcc -c -g -O2 -I ants.ads -gnatA strategy.adb
	gnatbind mybot.ali
	gnatlink mybot.ali -o mybot
clean:
	rm ants.ali ants.o mybot mybot.ali mybot.o strategy.ali strategy.o
