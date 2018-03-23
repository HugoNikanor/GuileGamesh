.PHONY : clean all tags

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

C_FILES = $(wildcard src/*.c)
O_FILES = $(addprefix obj/,$(notdir $(C_FILES:.c=.o)))

all : main.so main

obj :
	mkdir obj

obj/%.o : src/%.c obj
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
