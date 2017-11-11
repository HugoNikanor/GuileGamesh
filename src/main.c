#include <libguile.h>
#include <SDL.h>
#include <SDL_ttf.h>
#include <SDL_image.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>

#include "types.h"
#include "event.h"
#include "sdl_scm.h"

bool ready = false;
SCM draw_func = SCM_UNDEFINED;
SCM tick_func = SCM_UNDEFINED;
SCM event_func = SCM_UNDEFINED;

SCM make_empty_scene = SCM_UNDEFINED;
SCM get_event_list   = SCM_UNDEFINED;
SCM get_tick_list    = SCM_UNDEFINED;
SCM get_draw_list    = SCM_UNDEFINED;
SCM current_scene    = SCM_UNDEFINED;

SDL_Renderer* renderer;

/*
 * Run this after creating the obj-draw function in scheme code.
 *
 * This binds it and informs the system that it's now safe to
 * try and draw objects.
 */
static SCM set_ready() {
	draw_func  = scm_variable_ref(scm_c_lookup ("draw-func"));
	tick_func  = scm_variable_ref(scm_c_lookup ("tick-func"));
	event_func = scm_variable_ref(scm_c_lookup ("event-func"));

	ready = true;
	return SCM_UNDEFINED;
}

static void* tick_objects (void* args) {
	my_for_each (tick_func, scm_call_1(get_tick_list,
				scm_call_0(current_scene)));
	return NULL;
}

static void* draw_objects (void* args) {
	my_for_each (draw_func, scm_call_1 (get_draw_list,
				scm_call_0(current_scene)));
	return NULL;
}

static void* event_objects (arg_struct* args) {
	SDL_Event* event;
	while (args != NULL) {
		if (args->type == EVENT) {
			event = args->arg.event;
			break;
		}
	}

	if (event == NULL)
		return NULL;

	switch (event->type) {
		case SDL_KEYDOWN:
		case SDL_KEYUP:
			scm_call_1 (event_func, bind_keyboard_event (&event->key));
			break;
		case SDL_MOUSEBUTTONDOWN:
		case SDL_MOUSEBUTTONUP:
			scm_call_1 (event_func, bind_mouse_btn (&event->button));
			break;
		case SDL_MOUSEMOTION:
			scm_call_1 (event_func, bind_mouse_move (&event->motion));
	}


	return NULL;
}

static void* call_funcs (void* args) {
	if (ready) {
		tick_objects (args);
		draw_objects (args);
		event_objects (args);
	}

	return NULL;
}
/*
 * These are defined here to allow for the guile thread
 * to acccess them upon startup.
 */
int argc;
char** argv;

static void inner_guile_main (void* data, int argc, char* argv[]) {
	scm_c_define_gsubr
		("ready!", 0, 0, 0, set_ready);

	scm_c_define_gsubr
		("draw-rect", 5, 0, 0, draw_rect);
	scm_c_define_gsubr
		("set-color", 3, 1, 0, set_color);
	scm_c_define_gsubr
		("draw-text", 3, 0, 0, draw_text);
	scm_c_define_gsubr
		("draw-line", 4, 0, 0, draw_line);

	scm_c_define_gsubr
		("load-image", 1, 0, 0, init_img);

	scm_c_define_gsubr
		("render-texture", 4, 0, 0, render_texture);

	scm_c_use_module ("scene");

	get_event_list = scm_variable_ref(scm_c_lookup ("get-event-list"));
	get_tick_list  = scm_variable_ref(scm_c_lookup ("get-tick-list"));
	get_draw_list  = scm_variable_ref(scm_c_lookup ("get-draw-list"));
	current_scene  = scm_variable_ref(scm_c_lookup ("current-scene"));

	scm_c_primitive_load ("scheme/code.scm");

	scm_shell (argc, argv);
}

void* init_guile_thread (void* args) {
	scm_boot_guile (argc, argv, inner_guile_main, 0);
	return 0;
}

void* f(void* args) {
	SCM func = scm_variable_ref(scm_c_lookup ("init-tile-set"));
	scm_call_0 (func);
}

//SDL_Texture* img = NULL;

int main(int _argc, char* _argv[]) {
	setenv("GUILE_LOAD_PATH", "scheme", 1);
	argc = _argc;
	argv = _argv;

	pthread_t guile_thread;
	pthread_create (&guile_thread, NULL, init_guile_thread, NULL);

    SDL_Window *window;                    // Declare a pointer

    SDL_Init(SDL_INIT_VIDEO);              // Initialize SDL2

	TTF_Init();
	char* sans_path = "fonts/FiraMono-Regular.ttf";
	sans = TTF_OpenFont(sans_path, 12);
	if (sans == NULL) {
		printf("Couldn't load font:\n");
		printf("%s\n", sans_path);
		printf("Exiting\n");
		exit (1);
	}

	int width, height;
	width = height = 512;

    // Create an application window with the following settings:
    window = SDL_CreateWindow(
        "Game Engine",                     // window title
        SDL_WINDOWPOS_UNDEFINED,           // initial x position
        SDL_WINDOWPOS_UNDEFINED,           // initial y position
        width,                             // width, in pixels
        height,                            // height, in pixels
        SDL_WINDOW_OPENGL                  // flags - see below
    );

    // Check that the window was successfully created
    if (window == NULL) {
        // In the case that the window could not be made...
        printf("Could not create window: %s\n", SDL_GetError());
        return 1;
    }

	renderer = SDL_CreateRenderer (window, -1, 0);

	scm_with_guile (f, NULL);


	/*
	img = IMG_LoadTexture (renderer, "assets/PathAndObjects_0.png");
	if (img == NULL) {
		puts("couldn't load image");
		return 1;
	}
	*/

	int arg_len = 1;
	arg_struct arg[arg_len];

	SDL_Event event;
	int has_event = false;
	SDL_Rect screen_rect = {.x = 0, .y = 0, .w = 512, .h = 512};
	SDL_Rect sprite_rect = {.x = 0, .y = 0, .w = 16, .h = 16};
	SDL_Rect screen_part = sprite_rect;
	while (true) {
		SDL_SetRenderDrawColor (renderer, 0, 0, 0, 0xFF);
		SDL_RenderClear (renderer);
		SDL_SetRenderDrawColor (renderer, 0xFF, 0, 0, 0xFF);

		//SDL_RenderCopy (renderer, img, &screen_rect, &screen_rect);
		/*
		for (int y = 0; y < 32; y++) {
			for (int x = 0; x < 32; x++) {
				screen_part.x = x * 16;
				screen_part.y = y * 16;
				SDL_RenderCopy (renderer, img, &sprite_rect, &screen_part);
			}
		}

		for (int i = 0; i < 512; i += 16) {
			SDL_RenderDrawLine (renderer, i, 0, i, 512);
			SDL_RenderDrawLine (renderer, 0, i, 512, i);
		}
		*/

		has_event = SDL_PollEvent(&event);
		if (has_event) {
			if(event.type == SDL_QUIT)
				break;
			arg[0].arg.event = &event;
			arg[0].type = EVENT;
		} else {
			arg[0].arg.event = NULL;
			//arg[0].type = NOEVENTT;
		}

		scm_with_guile (call_funcs, arg);

		SDL_RenderPresent (renderer);
	}

	TTF_Quit();

    // Close and destroy the window
	//SDL_DestroyTexture (img);
	SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);

    // Clean up
    SDL_Quit();
    return 0;
}
