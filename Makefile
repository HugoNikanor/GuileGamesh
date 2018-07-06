.PHONY : clean all tags

CFLAGS = $(shell guile-config compile) \
         $(shell pkg-config --cflags sdl2) \
         $(shell pkg-config --cflags SDL2_ttf) \
         $(shell pkg-config --cflags SDL2_image) \
         -pthread -fPIC -Wall
LFLAGS = $(shell guile-config link) \
         $(shell pkg-config --libs sdl2) \
         $(shell pkg-config --libs SDL2_ttf) \
         $(shell pkg-config --libs SDL2_image) \
         -lm -pthread
DFLAGS = -ggdb

C_FILES = $(wildcard src/*.c)
O_FILES = $(addprefix obj/,$(notdir $(C_FILES:.c=.o)))

all : main.so main

obj/%.o : src/%.c
	test -d obj || mkdir obj
	gcc -c ${CFLAGS} ${DFLAGS} -o $@ $<

main.so : $(O_FILES)
	gcc ${LFLAGS} -shared -o $@ $^

main : $(O_FILES)
	gcc ${LFLAGS} -o $@ $^

# This is marked PHONY, but is actually a real file
# the flag is since make has no idea if it has to be 
# remade.
tags :
	ctags -R scheme src
	ctags -e -R scheme src

clean:
	-rm obj/*.o
	-rmdir obj
	-rm main.so
	-rm main
