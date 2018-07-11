#include "stack.h"

stack* new_stack () {
	stack* s = malloc(sizeof(stack));
	s->value = NULL;
	s->rest = NULL;
	return s;
}

void push (void* val, stack* stk) {
	stack* s = malloc(sizeof(stack));
	s->value = val;
	s->rest = stk;
	stk = s;
}

void* peek (stack* stack) {
	return stack->value;
}

void* pop (stack* stk) {
	void* ret = peek(stk);

	stack* nxt = stk->rest;
	free (stk);
	stk = nxt;

	return ret;
}

void free_stack (stack* stk) {
	while (stk != NULL) {
		free(stk->value);
		stack* tmp = stk->rest;
		free(stk);
		stk = tmp;
	}
}
