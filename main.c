#include <libguile.h>
#include <SDL.h>
#include <pthread.h>
#include <stdbool.h>

//#include "override.h"

#define D1 printf("%i before\n", __LINE__)
#define D2 printf("%i after\n", __LINE__)

#define cons_mut(obj,list) list = scm_cons (obj, list)

typedef enum { EVENT } arg_t;

typedef union {
	SDL_Event* event;
} arg_types;

typedef struct {
	arg_t type;
	arg_types arg;
} arg_struct;

static SCM keyboard_event;

void init_sdl_event_type (void) {
	SCM name, slots;
	scm_t_struct_finalize finalizer;

	name = scm_from_utf8_symbol ("SDL_KeyboardEvent");
	slots = scm_list_n (
			scm_from_utf8_symbol ("type"),
			scm_from_utf8_symbol ("timestamp"),
			scm_from_utf8_symbol ("windowID"),
			scm_from_utf8_symbol ("state"),
			scm_from_utf8_symbol ("repeat"),
			scm_from_utf8_symbol ("keysym"),
			SCM_UNDEFINED
			);
	finalizer = NULL;

	keyboard_event =
		scm_make_foreign_object_type (name, slots, finalizer);
}

static SCM arr_to_list (int counter, SCM* arr) {
	if (counter == 1)
		return SCM_EOL;
	else
		return scm_cons (*arr, arr_to_list (counter - 1, arr + 1));
}

static SCM bind_keyboard_event (SDL_KeyboardEvent* event) {
	//return scm_make_foreign_object_1 (keyboard_event, event);
	printf ("%s, %zu, %i, %s, %i, %i\n",
		   	event->type == SDL_KEYUP ? "SDL_Keyup" : "SDL_Keydown",
			event->timestamp,
			event->windowID,
			event->state == SDL_PRESSED ? "SDL_PRESSED" : "SDL_RELEASED",
			event->repeat,
			event->keysym.sym);

	//SCM* items = malloc(sizeof(void*) * 6);
	SCM items[6];
	items[0] = scm_from_utf8_symbol(event->type == SDL_KEYUP ? "SDL_KEYUP" : "SDL_KEYDOWN");
	items[1] = scm_from_uint32 (event->timestamp);
	items[2] = scm_from_uint32 (event->windowID);
	items[3] = scm_from_uint8  (event->state);
	items[4] = scm_from_uint8  (event->repeat);

	items[5] = scm_from_int (0);

	SCM slots = 
		scm_list_n (
			scm_from_utf8_symbol ("type"),
			scm_from_utf8_symbol ("timestamp"),
			scm_from_utf8_symbol ("windowID"),
			scm_from_utf8_symbol ("state"),
			scm_from_utf8_symbol ("repeat"),
			scm_from_utf8_symbol ("keysym"),
			SCM_UNDEFINED
			);

	SCM list = scm_cons (scm_from_utf8_symbol ("<key-event>"), arr_to_list (6, items));
	return list;

	// (slot-set! obj 'slot val)
	/*
	SCM func = scm_variable_ref(scm_c_lookup ("define-class-cont"));
	scm_call_4 (func,
		  scm_from_utf8_symbol ("<SDL_KeyEvent>"),
	      scm_from_utf8_symbol ("make-keyevent"),
		  SCM_EOL,
		  slots);
		  */

	//return scm_apply_0 (scm_variable_ref(scm_c_lookup("make-keyevent")), list);
	//return my_scm_make_foreign_object_n (keyboard_event, list);
	//items[5] = (void*) event->keysym;
	//SCM obj = scm_make_foreign_object_0 (keyboard_event);
	/*
	scm_foreign_object_signed_set_x (obj, 0,  scm_from_utf8_symbol(event->type == SDL_KEYUP ? "SDL_KEYUP" : "SDL_KEYDOWN"));
	scm_foreign_object_signed_set_x (obj, 1, &event->timestamp);
	scm_foreign_object_signed_set_x (obj, 2, &event->windowID);
	scm_foreign_object_signed_set_x (obj, 3, &event->state);
	scm_foreign_object_signed_set_x (obj, 4, &event->repeat);
	scm_foreign_object_signed_set_x (obj, 5, &event->keysym.sym);
	*/
	//scm_foreign_object_[[un]signed]set_x (obj, n, val);
	//return scm_make_foreign_object_n (keyboard_event, 6, (void*) event);
	//return scm_make_foreign_object_n_alt (keyboard_event, 6, (void*) event);
	//return scm_make_foreign_object_n (keyboard_event, 5, (void*) items);
	//return scm_make_foreign_object_3 (keyboard_event, items[0], items[1], items[2]);
	//return obj;
}

bool ready = false;
SCM draw_list = SCM_EOL;
SCM tick_list = SCM_EOL;
SCM event_list = SCM_EOL;
SCM draw_func = SCM_UNDEFINED;
SCM tick_func = SCM_UNDEFINED;
SCM event_func = SCM_UNDEFINED;

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



/*
 * TODO ensure uniquness, also, there might be
 * faster constructs rather than linked lists.
 */
static SCM add_scm_draw_object (SCM obj) {
	//objects = scm_cons (obj, objects);
	cons_mut (obj, draw_list);
	return SCM_UNDEFINED;
}

static SCM add_scm_tick_object (SCM obj) {
	cons_mut (obj, tick_list);
	return SCM_UNDEFINED;
}

static SCM add_scm_event_object (SCM obj) {
	cons_mut (obj, event_list);
	return SCM_UNDEFINED;
}

/*
 * Returns all the registered object lists.
 */
static SCM get_registered_objects() {
	return scm_list_3(draw_list, tick_list, event_list);
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

static void my_for_each_1 (SCM func, SCM item, SCM list) {
	SCM nlist = list;
	while (! scm_is_null (nlist)) {
		scm_call_2 (func, scm_car (nlist), item);
		nlist = scm_cdr (nlist);
	}
}

SDL_Renderer* renderer;

static void* tick_objects (void* args) {
	my_for_each (tick_func, tick_list);
	return NULL;
}

static void* draw_objects (void* args) {
	my_for_each (draw_func, draw_list);
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

	if (event->type == SDL_KEYDOWN
			|| event->type == SDL_KEYUP) {
		//my_for_each_1 (event_func, bind_keyboard_event (&event->key), event_list);
		scm_call_1 (event_func, bind_keyboard_event (&event->key));
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

static SCM draw_rect (SCM fill_p, SCM x, SCM y, SCM w, SCM h) {
	SDL_Rect rect = {.x = scm_to_int (x),
	                 .y = scm_to_int (y),
					 .w = scm_to_int (w),
					 .h = scm_to_int (h) };
	(scm_is_true (fill_p) ? SDL_RenderFillRect : SDL_RenderDrawRect)
		(renderer, &rect);
	return SCM_UNDEFINED;
}

/*
 * These are defined here to allow for the guile thread
 * to acccess them upon startup.  
 */
int argc;
char** argv;

static void inner_guile_main (void* data, int argc, char* argv[]) {
	scm_c_define_gsubr
		("register-tick-object!", 1, 0, 0, add_scm_tick_object);
	scm_c_define_gsubr
		("register-draw-object!", 1, 0, 0, add_scm_draw_object);
	scm_c_define_gsubr
		("register-event-object!", 1, 0, 0, add_scm_event_object);

	scm_c_define_gsubr
		("ready!", 0, 0, 0, set_ready);

	scm_c_define_gsubr
		("get-registered-objects", 0, 0, 0, get_registered_objects);

	scm_c_define_gsubr
		("draw-rect", 5, 0, 0, draw_rect);

	init_sdl_event_type();

	//scm_c_eval_string ("(load \"code.scm\")");
	scm_c_primitive_load ("code.scm");

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

	renderer = SDL_CreateRenderer (window, -1, 0);

	int arg_len = 1;
	arg_struct arg[arg_len];

	SDL_Event event;
	int has_event = false;
	while (true) {
		SDL_SetRenderDrawColor (renderer, 0, 0, 0, 0xFF);
		SDL_RenderClear (renderer);
		SDL_SetRenderDrawColor (renderer, 0xFF, 0, 0, 0xFF);

		has_event = SDL_PollEvent(&event);
		if (has_event) {
			if(event.type == SDL_QUIT)
				break;
			arg[0].arg.event = &event;
			arg[0].type = EVENT;
		}

		scm_with_guile (call_funcs, arg);

		SDL_RenderPresent (renderer);
	}

    // Close and destroy the window
	SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);

    // Clean up
    SDL_Quit();
    return 0;
}
