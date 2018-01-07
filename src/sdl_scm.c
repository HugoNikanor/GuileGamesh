#include "sdl_scm.h"
#include "util.h"

SCM draw_rect (SCM fill_p, SCM x, SCM y, SCM w, SCM h) {
	SDL_Rect rect = {.x = (int) scm_to_double (x),
	                 .y = (int) scm_to_double (y),
					 .w = (int) scm_to_double (w),
					 .h = (int) scm_to_double (h) };
	(scm_is_true (fill_p) ? SDL_RenderFillRect : SDL_RenderDrawRect)
		(renderer, &rect);
	return SCM_UNDEFINED;
}

SCM set_color (SCM r, SCM g, SCM b, SCM a) {
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
SCM draw_text (SCM text, SCM _x, SCM _y) {
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

SDL_Texture* img;
SCM init_img (SCM filepath) {
	char* path = scm_to_utf8_string (filepath);
	img = IMG_LoadTexture (renderer, path);
	if (img == NULL) {
		puts ("_____couldn't load image______");
		puts (SDL_GetError());
	}
	return scm_from_long((long) img);
}

SCM render_texture (SCM img_ptr, SCM _tile_size, SCM sprite_pos, SCM board_pos) {
	img = (SDL_Texture*) scm_to_long (img_ptr);
	int sx = scm_to_int (scm_list_ref (sprite_pos, scm_from_int(0)));
	int sy = scm_to_int (scm_list_ref (sprite_pos, scm_from_int(1)));
	int tile_size = scm_to_int (_tile_size);

	int board_pos_x = scm_to_int (scm_list_ref (board_pos, scm_from_int (0)));
	int board_pos_y = scm_to_int (scm_list_ref (board_pos, scm_from_int (1)));

	SDL_Rect sprite_tile = {
		.x = sx * tile_size,
		.y = sy * tile_size,
		.w = tile_size,
		.h = tile_size };
	SDL_Rect board_space = {
		.x = board_pos_x * tile_size,
		.y = board_pos_y * tile_size,
		.w = tile_size,
		.h = tile_size };

	SDL_RenderCopy (renderer, img, &sprite_tile, &board_space);

	return SCM_UNSPECIFIED;
}

SCM draw_line (SCM _x1, SCM _y1, SCM _x2, SCM _y2) {
	int x1 = scm_to_double (_x1);
	int y1 = scm_to_double (_y1);
	int x2 = scm_to_double (_x2);
	int y2 = scm_to_double (_y2);

	SDL_RenderDrawLine (renderer, x1, y1, x2, y2);

	return SCM_UNSPECIFIED;
}

SCM draw_pixel (SCM _x, SCM _y) {
	int x = scm_to_int (_x);
	int y = scm_to_int (_y);

	SDL_RenderDrawPoint (renderer, x, y);

	return SCM_UNSPECIFIED;
}

/*
 * This function most likely has a memory leak
 * for the points it creates for rendering
 */
SCM draw_ellipse (SCM _r, SCM _x0, SCM _y0, SCM _d) {
	int x_0 = scm_to_int (_x0);
	int y_0 = scm_to_int (_y0);
	int r  = scm_to_int (_r);
	int d  = scm_to_int (_d); // distance from center to either focus

	float r_x = r / 2;
	float r_y = sqrt (r_x * r_x - d * d);

	int count = 100;
	SDL_Point* pts = malloc (sizeof(*pts) * count);

	for (int i = 0; i < count; i++) {
		float theta = (TAU / (count - 1)) * i;
		float x = r_x * cosf (theta);
		float y = r_y * sinf (theta);

		SDL_Point* pt = malloc (sizeof (*pt));
		pt->x = x + x_0;
		pt->y = y + y_0;

		pts [i] = *pt;
	}

	SDL_RenderDrawLines (renderer, pts, count);

	for (int i = 0; i < count; i++) {
		//free (pts + i * sizeof (*pts));
	}
	free (pts);

	return SCM_UNSPECIFIED;
}
