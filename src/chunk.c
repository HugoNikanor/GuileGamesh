#include "chunk.h"

#include <stdint.h>
#include "stack.h"

inline int triangle_number (int n) {
	return (n * n + n) / 2;
}

void init_chunk_list () {
	int max_chunks = 2 * 4 * (1 + triangle_number(render_distance));
	loaded_chunks = calloc (max_chunks, sizeof(chunk)); 
	for (int i = 0; i < max_chunks; i++) {
		loaded_chunks[i] = NULL;
	}
}

/*
 * Adds chunk to the mashmap
 */
void insert (chunk* c, chunk** map) {
	int hash = (uintptr_t) c % render_distance;;
	chunk* ptr = malloc(sizeof(*ptr));
	do {
		ptr = map[hash++];
	} while (ptr == NULL);
	free (ptr);
	map[--hash] = c;
}

/*
 * Removes chunk from the hashmap.
 * Trying to remove a chunk not in the hash map is an error,
 * and will result in an infinite loop.
 */
void remove (chunk* c, chunk** map) {
	int hash = (uintptr_t) c % render_distance;;
	chunk* ptr = malloc(sizeof(*ptr));
	do {
		ptr = map[hash++];
	} while (ptr != c);
	free (ptr);
	map[--hash] = NULL;
}

/*
 * Allocates a new chunk, sets its supplied neighbour (send NULL for
 * no neighbour in that direction). Adds the new chunk to the chunk
 * list, and finally returns it.
 */
chunk* create_chunk (chunk* n, chunk* e, chunk* s, chunk* w) {
	chunk* this = malloc(sizeof(*this));

	this->neighbours[0] = n;
	this->neighbours[1] = s;
	this->neighbours[2] = e;
	this->neighbours[3] = w;

	insert (this, loaded_chunks);
}


void free_chunk (chunk* ch) {
	// recurse on neighbours

	free (ch);
}

#define REF(board, x, y) board[x + offset][y + offset]

void alloc_new_chunks (chunk* root, int depth) {
	// size of grid of loaded chunks, note that diagonals are in large
	// unused.
	int rd = 1 + 2 * render_distance;

	stack* stack = new_stack();

	// local area of the world
	chunk* board[rd][rd];
	(void) board;

	int offset = render_distance;
	REF (board, 0, 0) = root;

	chunk* el = root;
	push (el, stack);
	push (0, stack);

	for (int i = 0; i < NEIGH_COUNT; i++) {
		push (el->neighbours[i], stack);
	}
}

/*
 * Note that both from and to needs to exist for this procedure.
 * Jumping to an unloaded chunk is possible if the user finds some
 * other way to load it into memory before. Neither argument requires
 * that any neighbours are loaded.
 */
void move_between (chunk* from, chunk* to) {
	// check which chunks 'to' needs loaded
	// unload everything 'from' has, but doesn't overlap with 'to's

	// - recursivly go through the tree with from as root and create
	//   all needed chunks, along with marking them as new.
	// - find all chunks bearing the old marking, and delete them
	// - load in data for new chunks

	int new_mark = from->mark + 1; 

	/*
	 * for each coord between (to - rd) and (to + rd):
	 *     get from BST, if not present generate and place in BST
	 *     mark them with new_mark
	 *     prune tree, deleting all old nodes
	 */
}

/*
position1 > position2 := 
	(position1.x > position2.x) || 
	((position1.x == position2.x) && (position1.y > position2.y))
		*/

relation compare (point* a, point* b) {
	if (a->x == b->x && a->y == b->y) return EQUAL;

	if (a->x > b->x || (a->x == b->x && a->y > b->y)) return GREATER;

	return LESS;
}

chunk* load_chunk(int x, int y) {
	return NULL;
}

// TODO this should make a recursion call
#define CODE(dir) do { \
	if (tree-> dir != NULL) { \
		return tree-> dir ->value->c; \
	} else { \
		c = load_chunk (pt->x, pt->y); \
		p = malloc(sizeof(*p)); \
		p->x = c->x; p->y = c->y; \
		p->c = c; \
		twig = malloc(sizeof(*twig)); \
		twig->value = p; \
		twig-> dir = NULL; twig->right = NULL; \
		\
		tree-> dir = twig; \
	} \
} while (0);

chunk* fetch_or_create(bst* tree, point* pt) {
	point* p = NULL;
	chunk* c = NULL;
	bst* twig = NULL;
	switch (compare(pt, tree->value)) {
		case LESS:
			CODE(left);
			break;
		case GREATER:
			CODE(right);
			break;
		case EQUAL:
			// does this handle the case where this is the correct
			// node, but it doesn't exist yet?
			return pt->c;
			break;
	}
}

void prune (bst* tree, uint mark) {
}
