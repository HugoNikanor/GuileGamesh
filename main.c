#include <libguile.h>
#include <SDL.h>
#include <pthread.h>
#include <stdbool.h>

bool ready = false;
SCM objects = SCM_EOL;
SCM func = SCM_UNDEFINED;

/*
 * Run this after creating the obj-disp function in scheme code.
 *
 * This binds it and informs the system that it's now safe to
 * try and draw objects.
 */
static SCM set_ready() {
	func = scm_variable_ref(scm_c_lookup ("obj-disp"));
	ready = true;
	return SCM_UNDEFINED;
}


/*
 * TODO ensure uniquness, also, there might be
 * faster constructs rather than linked lists.
 */
static SCM add_scm_disp_object(SCM obj) {
	objects = scm_cons (obj, objects);
	return SCM_UNDEFINED;
}

static SCM get_registered_objects() {
	return objects;
}

/*
 * I just don't understand scm_for_each
 */
static void my_for_each (SCM func, SCM list) {
	SCM nlist = list;
	while (! scm_is_null (nlist)) {
		scm_call_1 (func, scm_car (nlist));
		nlist = scm_cdr (nlist);
	}
}

static void* draw_objects(void* args) {
	if (ready)
		my_for_each (func, objects);
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
		("register-object!", 1, 0, 0, add_scm_disp_object);
	scm_c_define_gsubr
		("ready!", 0, 0, 0, set_ready);
	scm_c_define_gsubr
		("get-registered-objects", 0, 0, 0, get_registered_objects);

	scm_shell (argc, argv);
}

void* init_guile_thread (void* args) {
	scm_boot_guile (argc, argv, inner_guile_main, 0);
	return 0;
}


int main(int _argc, char* _argv[]) {
	argc = _argc;
	argv = _argv;

	pthread_t guile_thread;
	pthread_create (&guile_thread, NULL, init_guile_thread, NULL);

    SDL_Window *window;                    // Declare a pointer

    SDL_Init(SDL_INIT_VIDEO);              // Initialize SDL2

    // Create an application window with the following settings:
    window = SDL_CreateWindow(
        "Game Engine",                     // window title
        SDL_WINDOWPOS_UNDEFINED,           // initial x position
        SDL_WINDOWPOS_UNDEFINED,           // initial y position
        100,                               // width, in pixels
        100,                               // height, in pixels
        SDL_WINDOW_OPENGL                  // flags - see below
    );

    // Check that the window was successfully created
    if (window == NULL) {
        // In the case that the window could not be made...
        printf("Could not create window: %s\n", SDL_GetError());
        return 1;
    }

	SDL_Renderer* renderer = SDL_CreateRenderer (window, -1, 0);

	SDL_Event event;
	int hasEvent;
	while (true) {
		SDL_SetRenderDrawColor (renderer, 0xFF, 0, 0, 0xFF);
		SDL_RenderClear (renderer);

		hasEvent = SDL_PollEvent(&event);
		if (hasEvent && event.type == SDL_QUIT)
			break;

		scm_with_guile (draw_objects, NULL);

		SDL_RenderPresent (renderer);
	}

    // Close and destroy the window
	SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);

    // Clean up
    SDL_Quit();
    return 0;
}
