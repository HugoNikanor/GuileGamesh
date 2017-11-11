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

#endif // TYPES_H
