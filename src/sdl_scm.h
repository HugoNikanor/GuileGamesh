#ifndef SDL_SCM_H_
#define SDL_SCM_H_

#include <libguile.h>
#include <SDL.h>
#include <SDL_ttf.h>
#include <SDL_image.h>

SCM draw_rect (SCM fill_p, SCM x, SCM y, SCM w, SCM h);
SCM set_color (SCM r, SCM g, SCM b, SCM a);
TTF_Font* sans;
//SDL_Color white = {0xFF, 0xFF, 0xFF};
SCM draw_text (SCM text, SCM _x, SCM _y);
extern SDL_Renderer* renderer;

SCM init_img (SCM filepath);
SCM render_texture (SCM img_ptr, SCM _tile_size, SCM sprite_pos, SCM board_pos);

#endif // SDL_SCM_H_
