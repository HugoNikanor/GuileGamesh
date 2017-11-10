.PHONY : clean all

CFLAGS = $$(guile-config compile) $$(pkg-config --cflags sdl2) -pthread -fPIC -Wall
LFLAGS = $$(guile-config link) $$(pkg-config --libs sdl2) -lm -pthread
DFLAGS = -ggdb

%.o : %.c
	gcc -c ${CFLAGS} ${DFLAGS} -o $@ $<

main : main.o
	#gcc ${LFLAGS} ${CFLAGS} -o $@ $^
	gcc ${LFLAGS} -o $@ $^

all : main

clean:
	-rm *.o
	rm main
