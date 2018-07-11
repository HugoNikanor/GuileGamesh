#include "sdl_scm.h"
#include "util.h"

SCM scm_primitive_draw_rect (SCM fill_p, SCM x, SCM y, SCM w, SCM h) {
	SDL_Rect rect = {.x = (int) scm_to_double (x),
	                 .y = (int) scm_to_double (y),
					 .w = (int) scm_to_double (w),
					 .h = (int) scm_to_double (h) };
	(scm_is_true (fill_p) ? SDL_RenderFillRect : SDL_RenderDrawRect)
		(renderer, &rect);
	return SCM_UNDEFINED;
}

SCM scm_set_color (SCM r, SCM g, SCM b, SCM a) {
	if (SCM_UNBNDP (a))
		a = scm_from_int(0xFF);

	SDL_SetRenderDrawColor (renderer,
			scm_to_int(r),
			scm_to_int(g),
			scm_to_int(b),
			scm_to_int(a));

	return SCM_UNDEFINED;
}

//TTF_Font* sans;
SDL_Color white = {0xFF, 0xFF, 0xFF};
SCM scm_primitive_draw_text (SCM text, SCM _x, SCM _y) {
	int x = scm_to_int (_x);
	int y = scm_to_int (_y);
	char* str = scm_to_utf8_string (text);
	SDL_Surface* surfaceMessage = TTF_RenderText_Blended(sans, str, white);
	SDL_Texture* texture = SDL_CreateTextureFromSurface(renderer, surfaceMessage);

	SDL_Rect rect = {.x = x, .y = y, .w = surfaceMessage->w, .h = surfaceMessage->h };

	SDL_RenderCopy(renderer, texture, NULL, &rect);

	SDL_FreeSurface(surfaceMessage);
	SDL_DestroyTexture(texture);

	return SCM_UNDEFINED;
}

// TODO this really should be a malloc in init_img 
SDL_Texture* img;
SCM scm_init_img (SCM filepath) {
	char* path = scm_to_utf8_string (filepath);
	img = IMG_LoadTexture (renderer, path);
	if (img == NULL) {
		puts ("_____couldn't load image______");
		puts (SDL_GetError());
	}
	int w, h;
	SDL_QueryTexture (img, NULL, NULL, &w, &h);
	printf ("Loaded texture with size %ix%i\n", w, h);
	return scm_from_long((long) img);
}

SCM scm_texture_size (SCM img_ptr) {
	img = (SDL_Texture*) scm_to_long (img_ptr);
	int w, h;
	SDL_QueryTexture (img, NULL, NULL, &w, &h);
	return scm_list_2 (scm_from_int (w), scm_from_int (h));
}

/*
 * img_ptr   - pointer to an SDL_Texture. Returned from init_img
 * _p_{x,y}  - Position in world to paint
 * _t_{w,h}  - size of the tile
 * _sp_{x,y} - position in spritesheet of tile, in tiles
 * All measures are in pixels except _sp_{x,y}.
 */
SCM scm_primitive_render_texture (SCM img_ptr, SCM _p_x, SCM _p_y,
		SCM _t_w, SCM _t_h, SCM _sp_x, SCM _sp_y) {

	img = (SDL_Texture*) scm_to_long (img_ptr);

	int sx = scm_to_int (_sp_x);
	int sy = scm_to_int (_sp_y);

	int tw = scm_to_int (_t_w);
	int th = scm_to_int (_t_h);

	int px = scm_to_int (_p_x);
	int py = scm_to_int (_p_y);

	SDL_Rect sprite_tile = {
		.x = sx * tw,
		.y = sy * th,
		.w = tw,
		.h = th };

	SDL_Rect board_space = {
		.x = px,
		.y = py,
		.w = tw,
		.h = th };

	SDL_RenderCopy (renderer, img, &sprite_tile, &board_space);

	return SCM_UNSPECIFIED;
}

/*
 * TODO ability to scale sprites
 */
SCM scm_primitive_render_single_sprite (SCM img_ptr, SCM board_pos) {
	img = (SDL_Texture*) scm_to_long (img_ptr);
	//int sx = scm_to_int (scm_list_ref (sprite_pos, scm_from_int(0)));
	//int sy = scm_to_int (scm_list_ref (sprite_pos, scm_from_int(1)));
	//int img_size = scm_to_int (_img_size);

	int w, h;
	SDL_QueryTexture (img, NULL, NULL, &w, &h);
	//printf ("Loaded texture with size %ix%i\n", w, h);

	int board_pos_x = scm_to_int (scm_list_ref (board_pos, scm_from_int (0)));
	int board_pos_y = scm_to_int (scm_list_ref (board_pos, scm_from_int (1)));

	SDL_Rect sprite_tile =
		{ .x = 0, .y = 0, .w = w, .h = h };
	SDL_Rect board_space = {
		.x = board_pos_x,
		.y = board_pos_y,
		.w = w, .h = h };

	SDL_RenderCopy (renderer, img, &sprite_tile, &board_space);

	return SCM_UNSPECIFIED;
}

SCM scm_primitive_draw_line (SCM _x1, SCM _y1, SCM _x2, SCM _y2) {
	int x1 = scm_to_double (_x1);
	int y1 = scm_to_double (_y1);
	int x2 = scm_to_double (_x2);
	int y2 = scm_to_double (_y2);

	SDL_RenderDrawLine (renderer, x1, y1, x2, y2);

	return SCM_UNSPECIFIED;
}

SCM scm_primitive_draw_pixel (SCM _x, SCM _y) {
	int x = scm_to_int (_x);
	int y = scm_to_int (_y);

	SDL_RenderDrawPoint (renderer, x, y);

	return SCM_UNSPECIFIED;
}

/*
 * This function most likely has a memory leak
 * for the points it creates for rendering
 */
SCM scm_primitive_draw_ellipse (SCM _r, SCM _x0, SCM _y0, SCM _d) {
	int x_0 = scm_to_int (_x0);
	int y_0 = scm_to_int (_y0);
	int r  = scm_to_int (_r);
	int d  = scm_to_int (_d); // distance from center to either focus

	float r_x = r / 2;
	float r_y = sqrt (r_x * r_x - d * d);

	int count = 100;
	//SDL_Point* pts = malloc (sizeof(*pts) * count);
	SDL_Point pts[count];

	for (int i = 0; i < count; i++) {
		float theta = (TAU / (count - 1)) * i;
		float x = r_x * cosf (theta);
		float y = r_y * sinf (theta);

		/*
		SDL_Point* pt = malloc (sizeof (*pt));
		pt->x = x + x_0;
		pt->y = y + y_0;

		pts [i] = *pt;
		*/
		SDL_Point pt = {
			.x = x + x_0,
			.y = y + y_0
		};

		pts [i] = pt;
	}

	SDL_RenderDrawLines (renderer, pts, count);

	/*
	printf ("sizeof %lu\n", sizeof (*pts));
	for (int i = 0; i < count; i++) {
		printf ("ptr: %p\n", &pts[i]);
		free (&pts[i]);
		//free (pts + i);
	}
	puts("done");
	//free (pts);
	*/

	return SCM_UNSPECIFIED;
}

SCM scm_set_window_size (SCM _width, SCM _height) {
	int width = scm_to_int(_width);
	int height = scm_to_int(_height);
	SDL_SetWindowSize (window, width, height);
	return SCM_UNSPECIFIED;
}
