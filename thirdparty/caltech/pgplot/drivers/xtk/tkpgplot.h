#ifndef tkpgplot_h
#define tkpgplot_h

#ifdef __cplusplus
extern "C" {
#endif

#include <tcl.h>
#include <tk.h>

/* Tk PGPLOT-widget package-initialization command */

int Tkpgplot_Init(Tcl_Interp *interp);

/*
 * Record the official PGPLOT device name of the widget driver.
 */
#define TK_PGPLOT_DEVICE "XTK"

#ifdef __cplusplus
}
#endif

#endif
