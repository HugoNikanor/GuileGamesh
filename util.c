#include "util.h"

SCM arr_to_list (int counter, SCM* arr) {
	if (counter == 1)
		return SCM_EOL;
	else
		return scm_cons (*arr, arr_to_list (counter - 1, arr + 1));
}

