#ifndef XmPgplot_h
#define XmPgplot_h

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Define resource-name constants.
 */
#define XmpNminColors "minColors"
#define XmpCMinColors "MinColors"

#define XmpNmaxColors "maxColors"
#define XmpCMaxColors "MaxColors"

#define XmpNpadX "padX"
#define XmpCPadX "PadX"

#define XmpNpadY "padY"
#define XmpCPadY "PadY"

#define XmpNshare "share"
#define XmpCShare "Share"

#define XMP_NORM_CURSOR  0 /* Un-augmented X cursor */
#define XMP_LINE_CURSOR  1 /* Line cursor between ref and pointer */
#define XMP_RECT_CURSOR  2 /* Rectangular cursor between ref and pointer */
#define XMP_YRNG_CURSOR  3 /* Two horizontal lines, at ref.x and pointer.x */
#define XMP_XRNG_CURSOR  4 /* Two vertical lines, at ref.y and pointer.y */
#define XMP_HLINE_CURSOR 6 /* Horizontal line cursor at y=ref.y */
#define XMP_VLINE_CURSOR 5 /* Vertical line cursor at x=ref.x */
#define XMP_CROSS_CURSOR 7 /* Cross-hair cursor centered on the pointer */

/*
 * When a cursor-input callback [previously registered using xmp_arm_cursor()]
 * is called by the widget, the position of the cursor and the key
 * that the user pressed are recorded in a struct of the following
 * form. A pointer to this struct is then cast to (XtPointer) and
 * passed as the 'call_data' argument of the callback function. The
 * callback function should cast this argument back to
 * (XmpCursorCallbackStruct *) in order to access its fields.
 */

typedef struct {
  float x,y;      /* The world-coordinate position of the cursor */
  char key;       /* The key pressed by the user (Mouse buttons='A','D','X') */
} XmpCursorCallbackStruct;

int xmp_arm_cursor(Widget widget, int mode, float xref, float yref,
		   XtCallbackProc callback, void *client_data);
int xmp_disarm_cursor(Widget widget);

/*
 * Record the official PGPLOT device name of the widget driver.
 */
#define XMP_DEVICE "XMOTIF"

/*
 * The following function returns an unambiguous PGPLOT device-specification
 * that can be used as the FILE argument of cpgbeg() to open a given PGPLOT
 * widget. It simply returns a string composed of the widget name, followed
 * by a "/" followed by XMP_DEVICE. Note that the returned string is owned
 * by the widget and must not be free()d or overwritten.
 */
char *xmp_device_name(Widget widget);

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
int xmp_device_id(Widget widget);

/*
 * The following global is a pointer to the shared class context
 * descriptor and is what is passed to XtCreateManagedWidget()
 * to tell it what type of widget to create.
 */
externalref WidgetClass xmPgplotWidgetClass;

/*
 * Declare opaque aliases to the widget class and instance structures.
 */
typedef struct XmPgplotClassRec *XmPgplotWidgetClass;
typedef struct XmPgplotRec *XmPgplotWidget;

/*
 * Convenience widget creation functions.
 */
Widget XmCreatePgplot(Widget parent, char *name, ArgList arglist,
		      Cardinal argcount);
Widget XmCreateScrolledPgplot(Widget parent, char *name, ArgList arglist,
			      Cardinal argcount);

/*
 * The following functions allow conversions between PGPLOT world coordinates
 * and X-window pixel coordinates.
 */
int xmp_pixel_to_world(Widget widget, int px, int py, float *wx, float *wy);
int xmp_world_to_pixel(Widget widget, float wx, float wy, int *px, int *py);

#ifdef __cplusplus
}
#endif

#endif

