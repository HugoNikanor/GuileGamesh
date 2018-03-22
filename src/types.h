#ifndef TYPES_H
#define TYPES_H

typedef enum { NOEVENTT, EVENT } arg_t;

typedef union {
	SDL_Event* event;
} arg_types;

typedef struct {
	arg_t type;
	arg_types arg;
} arg_struct;

// TODO possibly possible to rewrite the above as:

/*
typedef struct {
	union { SDL_Event* event } type;
	enum { NOEVENT, EVENT } arg_t;
} arg_struct;
*/

#endif // TYPES_H
