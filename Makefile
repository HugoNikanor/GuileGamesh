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

C_FILES = $(wildcard src/*.c)
O_FILES = $(addprefix obj/,$(notdir $(C_FILES:.c=.o)))

all : main.so main

obj/%.o : src/%.c
	-mkdir obj
	gcc -c ${CFLAGS} ${DFLAGS} -o $@ $<

main.so : $(O_FILES)
	gcc ${LFLAGS} -shared -o $@ $^

main : $(O_FILES)
	gcc ${LFLAGS} -o $@ $^

clean:
	-rm obj/*.o
	-rm main.so
	-rm main
