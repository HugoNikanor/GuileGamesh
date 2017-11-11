.PHONY : clean all

CFLAGS = $$(guile-config compile) \
         $$(pkg-config --cflags sdl2) \
	     $$(pkg-config --cflags SDL2_ttf) \
	     $$(pkg-config --cflags SDL2_image) \
	     -pthread -fPIC -Wall
LFLAGS = $$(guile-config link) \
         $$(pkg-config --libs sdl2) \
	     $$(pkg-config --libs SDL2_ttf) \
	     $$(pkg-config --libs SDL2_image) \
	     -lm -pthread
DFLAGS = -ggdb

C_FILES = $(wildcard *.c)
O_FILES = $(notdir $(C_FILES:.c=.o))

%.o : %.c
	gcc -c ${CFLAGS} ${DFLAGS} -o $@ $<

main : $(O_FILES)
	gcc ${LFLAGS} -o $@ $^

all : main

clean:
	-rm *.o
	rm main
