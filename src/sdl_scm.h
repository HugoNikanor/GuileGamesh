#ifndef SDL_SCM_H
#define SDL_SCM_H

#include <libguile.h>
#include <SDL.h>
#include <SDL_ttf.h>
#include <SDL_image.h>

SCM scm_primitive_draw_rect (SCM fill_p, SCM x, SCM y, SCM w, SCM h);
SCM scm_set_color (SCM r, SCM g, SCM b, SCM a);
TTF_Font* sans;
//SDL_Color white = {0xFF, 0xFF, 0xFF};
SCM scm_primitive_draw_text (SCM text, SCM _x, SCM _y);
extern SDL_Renderer* renderer;

SCM scm_init_img (SCM filepath);
SCM scm_primitive_render_texture (SCM img_ptr, SCM _p_x, SCM _p_y,
		SCM _ts_x, SCM _ts_y, SCM _sp_x, SCM _sp_y);
SCM scm_primitive_render_single_sprite (SCM img_ptr, SCM board_pos);
SCM scm_texture_size (SCM img_ptr);

SCM scm_primitive_draw_line (SCM _x1, SCM _y1, SCM _x2, SCM _y2);

SCM scm_primitive_draw_ellipse (SCM _r, SCM _x0, SCM _y0, SCM _d);
SCM scm_primitive_draw_pixel (SCM _x, SCM _y);

extern SDL_Window* window;

SCM scm_set_window_size (SCM _width, SCM _height);

#endif /* SDL_SCM_H */
