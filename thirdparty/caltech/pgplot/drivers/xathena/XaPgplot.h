#ifndef XaPgplot_h
#define XaPgplot_h

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Define resource-name constants.
 */
#define XapNminColors "minColors"
#define XapCMinColors "MinColors"

#define XapNmaxColors "maxColors"
#define XapCMaxColors "MaxColors"

#define XapNpadX "padX"
#define XapCPadX "PadX"

#define XapNpadY "padY"
#define XapCPadY "PadY"

#define XapNshare "share"
#define XapCShare "Share"

#define XAP_NORM_CURSOR  0 /* Un-augmented X cursor */
#define XAP_LINE_CURSOR  1 /* Line cursor between ref and pointer */
#define XAP_RECT_CURSOR  2 /* Rectangular cursor between ref and pointer */
#define XAP_YRNG_CURSOR  3 /* Two horizontal lines, at ref.x and pointer.x */
#define XAP_XRNG_CURSOR  4 /* Two vertical lines, at ref.y and pointer.y */
#define XAP_HLINE_CURSOR 6 /* Horizontal line cursor at y=ref.y */
#define XAP_VLINE_CURSOR 5 /* Vertical line cursor at x=ref.x */
#define XAP_CROSS_CURSOR 7 /* Cross-hair cursor centered on the pointer */

/*
 * When a cursor-input callback [previously registered using xap_arm_cursor()]
 * is called by the widget, the position of the cursor and the key
 * that the user pressed are recorded in a struct of the following
 * form. A pointer to this struct is then cast to (XtPointer) and
 * passed as the 'call_data' argument of the callback function. The
 * callback function should cast this argument back to
 * (XapCursorCallbackStruct *) in order to access its fields.
 */

typedef struct {
  float x,y;      /* The world-coordinate position of the cursor */
  char key;       /* The key pressed by the user (Mouse buttons='A','D','X') */
} XapCursorCallbackStruct;

int xap_arm_cursor(Widget widget, int mode, float xref, float yref,
		   XtCallbackProc callback, void *client_data);
int xap_disarm_cursor(Widget widget);

/*
 * Record the official PGPLOT device name of the widget driver.
 */
#define XAP_DEVICE "XATHENA"

/*
 * The following function returns an unambiguous PGPLOT device-specification
 * that can be used as the FILE argument of cpgbeg() to open a given PGPLOT
 * widget. It simply returns a string composed of the widget name, followed
 * by a "/" followed by XAP_DEVICE. Note that the returned string is owned
 * by the widget and must not be free()d or overwritten.
 */
char *xap_device_name(Widget widget);

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
int xap_device_id(Widget widget);

/*
 * The following global is a pointer to the shared class context
 * descriptor and is what is passed to XtCreateManagedWidget()
 * to tell it what type of widget to create.
 */
externalref WidgetClass xaPgplotWidgetClass;

/*
 * Declare opaque aliases to the widget class and instance structures.
 */
typedef struct XaPgplotClassRec *XaPgplotWidgetClass;
typedef struct XaPgplotRec *XaPgplotWidget;

/*
 * Convenience widget creation functions.
 */
Widget XaCreatePgplot(Widget parent, char *name, ArgList arglist,
		      Cardinal argcount);

/*
 * The following functions allow conversions between PGPLOT world coordinates
 * and X-window pixel coordinates.
 */
int xap_pixel_to_world(Widget widget, int px, int py, float *wx, float *wy);
int xap_world_to_pixel(Widget widget, float wx, float wy, int *px, int *py);

#ifdef __cplusplus
}
#endif

#endif

