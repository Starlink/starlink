#ifndef rvpgplot_h
#define rvpgplot_h

#ifdef __cplusplus
extern "C" {
#endif

#define RIVETONLY
#include <rivet.h>

/* Rivet/Tk PGPLOT-widget package-initialization command */

int Rvpgplot_Init(Tcl_Interp *interp);

/*
 * Record the official PGPLOT device name of the widget driver.
 */
#define TK_PGPLOT_DEVICE "XRV"

/*
 * The following function returns an unambiguous PGPLOT device-specification
 * that can be used as the FILE argument of cpgbeg() to open a given PGPLOT
 * widget. It simply returns a string composed of the widget name, followed
 * by a "/" followed by TK_PGPLOT_DEVICE. Note that the returned string is
 * owned by the widget and must not be free()d or overwritten.
 */
char *rvp_device_name(Rivetobj);

/*
 * After a widget has been opened to PGPLOT (via pgopen or pgbeg), the
 * following function can be used to return the PGPLOT id of the device.
 * When multiple PGPLOT devices are open this id can then be used with
 * the PGPLOT cpgslct() function to select the widget as the currently
 * selected PGPLOT graphics device.
 *
 * If the specified widget has not been opened to pgplot, or has been
 * closed and not re-opened, then 0 will be returned.
 */
int rvp_device_id(Rivetobj);

/*
 * Convert from X window pixel coordinates (px,py) to PGPLOT
 * world coordinates (*wx,*wy).
 */
int rvp_xwin2world(Rivetobj widget, int px, int py, float *wx, float *wy);

/*
 * Convert from PGPLOT world coordinates (wx,wy) to X window
 * pixel coordinates (*px,*py).
 */
int rvp_world2xwin(Rivetobj widget, float wx, float wy, int *px, int *py);

#ifdef __cplusplus
}
#endif

#endif
