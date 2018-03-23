#include "event.h"

/*
static SCM bind_keysym (SDL_Keysym sym) {
	// scancode
	// keycode sym
	// uint16 mod
	return scm_list_3 (scm_from_int (sym.scancode),
			scm_from_int (sym.sym),
			scm_from_uint16 (sym.mod));
}
*/

/*
void init_event_module () {
	scm_c_use_module ("(oop goops)");
	scm_c_use_module ("(event)");
}
*/

// make sure scheme is in the load path before calling these

SCM bind_keyboard_event (SDL_KeyboardEvent* event) {
	return scm_call_8
		( scm_c_public_ref ("event", "make-keyboard-event"),
		  scm_from_utf8_symbol(event->type == SDL_KEYUP
		                       ? "SDL_KEYUP"
		                       : "SDL_KEYDOWN"),
		  scm_from_uint32 (event->timestamp),
		  scm_from_uint32 (event->windowID),
		  scm_from_uint8  (event->state),
		  scm_from_uint8  (event->repeat),
		  scm_from_uint32 (event->keysym.scancode),
		  scm_from_uint32 (event->keysym.sym),
		  scm_from_uint32 (event->keysym.mod));
}

SCM bind_mouse_move (SDL_MouseMotionEvent* event) {
	return scm_call_9
		( scm_c_public_ref ("event", "make-mouse-motion-event"),
		  scm_from_utf8_symbol ("SDL_MOUSEMOTION"),
		  scm_from_uint32 (event->timestamp),
		  scm_from_uint32 (event->windowID),
		  scm_from_uint32 (event->which),
		  // scm_from_uint8 (event->button),
		  scm_from_uint8 (event->state),
		  // scm_from_uint8 (event->clicks),
		  scm_from_int32 (event->x),
		  scm_from_int32 (event->y),
		  scm_from_int32 (event->xrel),
		  scm_from_int32 (event->yrel));
		  
		  
}

SCM bind_mouse_btn (SDL_MouseButtonEvent* event) {
	return scm_call_9
		( scm_c_public_ref ("event", "make-mouse-button-event"),
		  scm_from_utf8_symbol(event->type == SDL_MOUSEBUTTONDOWN
		                       ? "SDL_MOUSEBUTTONDOWN"
		                       : "SDL_MOUSEBUTTONUP"),
		  scm_from_uint32 (event->timestamp),
		  scm_from_uint32 (event->windowID),
		  scm_from_uint32 (event->which),
		  scm_from_uint8 (event->button),
		  scm_from_uint8 (event->state),
		  scm_from_uint8 (event->clicks),
		  scm_from_int32 (event->x),
		  scm_from_int32 (event->y));

		
}
