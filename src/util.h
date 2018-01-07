#ifndef UTIL_H
#define UTIL_H

#include <libguile.h>

#define D1 printf("%i before\n", __LINE__)
#define D2 printf("%i after\n", __LINE__)

#define cons_mut(obj,list) list = scm_cons (obj, (list))
#define SYMB(s) scm_from_utf8_symbol (s)

SCM arr_to_list (int counter, SCM* arr);
void my_for_each (SCM func, SCM list);
void my_for_each_1 (SCM func, SCM item, SCM list);

#define PI 3.141592653589793
#define TAU (2 * PI)

#endif // UTIL_H
