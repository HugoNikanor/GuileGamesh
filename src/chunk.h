#ifndef CHUNK_H
#define CHUNK_H

#include <stdlib.h>

#define NEIGH_COUNT 4

typedef unsigned int uint;

typedef struct chunk {
	struct chunk* neighbours[NEIGH_COUNT];
	uint mark;
	int x;
	int y;
	void* data;
} chunk;

typedef enum { N, S, E, W } direction;

chunk* current_chunk = NULL;
int render_distance = 2;
chunk** loaded_chunks;

inline int triangle_number (int n);
void insert (chunk* c, chunk** map);
void remove (chunk* c, chunk** map);

void init_chunk_list ();

void add_loaded_chunk(chunk* c);

void set_current_chunk (chunk* c);
chunk* create_chunk (chunk* n, chunk* e, chunk* s, chunk* w);
void free_chunk (chunk* ch);

void move_between (chunk* from, chunk* to);

typedef enum {
	LESS, EQUAL, GREATER
} relation;

typedef struct {
	uint x, y;
	chunk* c;
} point;

typedef struct bst {
	point* value;
	struct bst* left;
	struct bst* right;
} bst;

/*
 * Fetches the value from the tree is it is there,
 * otherwise creates the value and puts it there.
 */
chunk* fetch_or_create(bst* tree, point* p);

/*
 * Remove all dead nodes from the tree. A node is concidered dead if
 * it's mark number is lower that the given mark.
 */
void prune (bst* tree, uint mark);

#endif /* CHUNK_H */
