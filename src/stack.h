#ifndef STACK_H
#define STACK_H

#include <stdlib.h>

typedef struct stack {
	void* value;
	struct stack* rest;
} stack;

stack* new_stack ();
void push (void* val, stack* stack);
void* pop (stack* stack);
void* peek (stack* stack);

// free's entire statck, values included
void free_stack (stack* stack);

#endif /* STACK_H */
