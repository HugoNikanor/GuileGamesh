#include <libguile.h>
#include <SDL.h>
#include <pthread.h>
#include <stdbool.h>

bool active;

static SCM toggle_active () {
	active = !active;
	return SCM_UNDEFINED;
}

int argc;
char** argv;

static void inner_guile_main (void* data, int argc, char* argv[]) {
	scm_c_define_gsubr ("toggle", 0, 0, 0, toggle_active);
	scm_shell (argc, argv);
}

void* init_guile_thread (void* args) {
	scm_boot_guile (argc, argv, inner_guile_main, 0);
	return 0;
}


int main(int _argc, char* _argv[]) {
	active = true;

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

		if (active)
			SDL_SetRenderDrawColor (renderer, 0, 0, 0, 0xFF);
		else
			SDL_SetRenderDrawColor (renderer, 0xFF, 0, 0, 0xFF);
		SDL_RenderClear (renderer);

		hasEvent = SDL_PollEvent(&event);
		if (hasEvent && event.type == SDL_QUIT)
			break;

		SDL_RenderPresent (renderer);
	}

    // Close and destroy the window
	SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);

    // Clean up
    SDL_Quit();
    return 0;
}
