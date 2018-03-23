#ifndef EVENT_H
#define EVENT_H

#include <libguile.h>
#include <SDL.h> 
#include "util.h"

//static SCM bind_keysym (SDL_Keysym sym);

SCM bind_keyboard_event (SDL_KeyboardEvent* event);
SCM bind_mouse_move (SDL_MouseMotionEvent* event);
SCM bind_mouse_btn (SDL_MouseButtonEvent* event);
SCM common_event ();

#endif /* EVENT_H */

