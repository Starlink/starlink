/*
 * Changes:
 */

/* Athena pgplot widget class implementation */

#include <X11/StringDefs.h>
#include <X11/keysym.h>
#include <X11/IntrinsicP.h>
#include <X11/Xaw/SimpleP.h>
#include <X11/Shell.h>

#include <stdio.h>
#include <stdlib.h>

#ifndef convex
#include <string.h>
#endif

/*
 * VAX VMS includes etc..
 */
#ifdef VMS
#include <descrip.h>
#include <ssdef.h>
typedef struct dsc$descriptor_s VMS_string;
#define VMS_STRING(dsc, string) \
  dsc.dsc$w_length = strlen(string); \
  dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
  dsc.dsc$b_class = DSC$K_CLASS_S; \
  dsc.dsc$a_pointer = string;
#endif

/*
 * Allow xadriv to be calleable by FORTRAN using the two commonest
 * calling conventions. Both conventions append length arguments for
 * each FORTRAN string at the end of the argument list, and convert the
 * name to lower-case, but one post-pends an underscore to the function
 * name (PG_PPU) while the other doesn't. Note the VMS is handled
 * separately below. For other calling conventions you must write a
 * C wrapper routine to call xadriv() or xadriv_().
 */
#ifdef PG_PPU
#define XADRIV xadriv_
#else
#define XADRIV xadriv
#endif

#include "XaPgplotP.h"

/*
 * Set the default size of the widget.
 */
#define XAP_MIN_WIDTH 64     /* Minimum width (pixels) */
#define XAP_MIN_HEIGHT 64    /* Minimum height (pixels) */
#define XAP_DEF_WIDTH 256    /* Default width (pixels) */
#define XAP_DEF_HEIGHT 256   /* Default height (pixels) */
#define XAP_MIN_COLORS 2     /* Min number of colors per colormap */
#define XAP_DEF_COLORS 100   /* Default number of colors to try for */
#define XAP_MAX_COLORS 255   /* Max number of colors per colormap */
#define XAP_DEF_MARGIN 20    /* The number of pixels to assign to the margin */
#define XAP_DEF_SHARE 0      /* Default to allocating shared colors */

/*
 * Specify the name to prefix errors with.
 */
#define XAP_IDENT "PgplotWidget"

static void xap_GetDefaultBackgroundColor(Widget widget, int offset,
					  XrmValue *value);
static void xap_GetDefaultForegroundColor(Widget widget, int offset,
					  XrmValue *value);
/*
 * Define all the X resources that are to be understood by the
 * widget.
 */
static XtResource resources[] = {
  {
    XapNminColors,
    XapCMinColors,
    XtRInt,
    sizeof(int),
    XtOffsetOf(XaPgplotRec, pgplot.min_colors),
    XtRImmediate,
    (XtPointer) XAP_MIN_COLORS
  },
  {
    XapNmaxColors,
    XapCMaxColors,
    XtRInt,
    sizeof(int),
    XtOffsetOf(XaPgplotRec, pgplot.max_colors),
    XtRImmediate,
    (XtPointer) XAP_DEF_COLORS
  },
  {
    XtNvisual,
    XtCVisual,
    XtRVisual,
    sizeof(Visual *),
    XtOffsetOf(XaPgplotRec, pgplot.visual),
    XtRImmediate,
    (XtPointer) CopyFromParent
  },
  {
    XtNresizeCallback,
    XtCCallback, XtRCallback, sizeof(XtCallbackList),
    XtOffsetOf(XaPgplotRec, pgplot.resize_callback),
    XtRImmediate, (XtPointer) NULL
  },
  {
    XtNbackground,
    XtCBackground,
    XtRPixel, 
    sizeof(Pixel),
    XtOffsetOf(XaPgplotRec, core.background_pixel),
    XtRCallProc, (XtPointer) xap_GetDefaultBackgroundColor
  },
  {
    XtNforeground,
    XtCForeground,
    XtRPixel, 
    sizeof(Pixel),
    XtOffsetOf(XaPgplotRec, pgplot.fgpixel),
    XtRCallProc, (XtPointer) xap_GetDefaultForegroundColor
  },
  {
    XapNpadX,
    XapCPadX,
    XtRDimension,
    sizeof(Dimension),
    XtOffsetOf(XaPgplotRec, pgplot.pad_x),
    XtRImmediate,
    (XtPointer) XAP_DEF_MARGIN
  },
  {
    XapNpadY,
    XapCPadY,
    XtRDimension,
    sizeof(Dimension),
    XtOffsetOf(XaPgplotRec, pgplot.pad_y),
    XtRImmediate,
    (XtPointer) XAP_DEF_MARGIN
  },
  {
    XapNshare,
    XapCShare,
    XtRBoolean,
    sizeof(Boolean),
    XtOffsetOf(XaPgplotRec, pgplot.share),
    XtRImmediate,
    (XtPointer) XAP_DEF_SHARE
  },
};

/*
 * Declare class method functions.
 */
static void xap_ClassPartInit(WidgetClass w);
static void xap_Initialize(Widget request, Widget new_w, 
				     ArgList args, Cardinal *num_args);
static void xap_Realize(Widget widget, XtValueMask *mask, 
				  XSetWindowAttributes *attributes);
static void xap_Resize(Widget widget);
static void xap_Destroy(Widget widget);
static void xap_Expose(Widget widget, XEvent *event, Region region);
static Boolean xap_SetValues(Widget old_widget, Widget req_widget, 
				       Widget new_widget, 
				       ArgList args, Cardinal *num_args);
static XtGeometryResult xap_Query_Geometry(Widget widget, 
					    XtWidgetGeometry *request, 
					    XtWidgetGeometry *reply);

/*
 * Define the Athena Pgplot class shared context descriptor.
 */
externaldef(xapgplotclassrec) XaPgplotClassRec xaPgplotClassRec = {

  { /* core_class fields */
    /* superclass         */    (WidgetClass) &simpleClassRec,
    /* class_name         */    "XaPgplot",
    /* widget_size        */    sizeof(XaPgplotRec),
    /* class_initialize   */    NULL,
    /* class_part_initiali*/    xap_ClassPartInit,
    /* class_inited       */    FALSE,
    /* initialize         */    xap_Initialize,
    /* initialize_hook    */    NULL,
    /* realize            */    xap_Realize,
    /* actions            */    NULL,
    /* num_actions        */    0,
    /* resources          */    resources,
    /* num_resources      */    XtNumber(resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    TRUE,
    /* compress_exposure  */    XtExposeCompressMultiple,
    /* compress_enterleave*/    TRUE,
    /* visible_interest   */    FALSE,
    /* destroy            */    xap_Destroy,
    /* resize             */    xap_Resize,
    /* expose             */    xap_Expose,
    /* set_values         */    xap_SetValues,
    /* set_values_hook    */    NULL,
    /* set_values_almost  */    XtInheritSetValuesAlmost,
    /* get_values_hook    */    NULL,
    /* accept_focus       */    NULL,
    /* version            */    XtVersion,
    /* callback_private   */    NULL,
    /* tm_table           */    NULL,
    /* query_geometry     */    xap_Query_Geometry,
    /* display accel      */    XtInheritDisplayAccelerator,
    /* extension          */    NULL
  },
  { /* Simple class fields  */
    /* change_sensitive	    */	XtInheritChangeSensitive
  },
   { /* Pgplot class fields */
     /* widget_id_counter   */  0,
     /* active_widgets      */  {NULL},
     /* free_widgets        */  {NULL},
     /* Extension           */  NULL
   }
};

/*
 * Declare a global class pointer for use in XtCreateManagedWidget
 * calls.
 */
externaldef(xapgplotwidgetclass) WidgetClass xaPgplotWidgetClass = (WidgetClass) &xaPgplotClassRec;

/*
 * Private functions.
 */
static void xap_abort(XaPgplotPart *xap, char *msg);
static XaPgplotWidget xap_open_widget(char *name);
static XaPgplotWidget xap_close_widget(char *name);
static void xap_update_clip(XaPgplotWidget w);
static void xap_NewPixmap(PgxWin *pgx, unsigned width, 
				    unsigned height);
static void xap_CursorHandler(Widget widget, XtPointer client_data, 
			      XEvent *event, Boolean *cont);
static int xap_ArmCursor(XaPgplotWidget w, int mode, float xref, 
			   float yref, XtCallbackProc callback, void *client_data);
static int xap_DisarmCursor(XaPgplotWidget w);
static int xap_new_visual(XaPgplotWidget w);
static int xap_WorldToPixel(XaPgplotWidget w, float wx, float wy,
			    int *px, int *py);
static int xap_PixelToWorld(XaPgplotWidget w, int px, int py,
			    float *wx, float *wy);

/* Enumerate the PGPLOT class widget lists */

#define XAP_ACTIVE_WIDGETS 1
#define XAP_FREE_WIDGETS 2

static XaPgplotList *xap_WidgetList(int type);

static XaPgplotWidget xap_FindWidgetByName(char *name, int type, 
						     XaPgplotWidget *prev);
static XaPgplotWidget xap_FindWidgetByID(int xaslct_id, int type, 
						   XaPgplotWidget *prev);

static XaPgplotWidget xap_RemoveWidget(char *name, int type);
static XaPgplotWidget xap_PrependWidget(XaPgplotWidget w, int type);
static XaPgplotWidget xap_CurrentWidget(char *context);

/*.......................................................................
 * This is called once to initialize the PGPLOT part of the widget class
 * structure when the first widget instance of this type is created.
 *
 * Input:
 *  w   WidgetClass   The class record to be initialized.
 */
static void xap_ClassPartInit(WidgetClass w)
{
  XaPgplotWidgetClass class = (XaPgplotWidgetClass) w;
  XaPgplotClassPart *pgplot = &class->pgplot_class;
  pgplot->widget_id_counter = 0;
  pgplot->active_widgets.head = NULL;
  pgplot->free_widgets.head = NULL;
  pgplot->extension = NULL;
  return;
}

/*.......................................................................
 * This is called to check resource derived defaults and initialize
 * a PGPLOT widget instance.
 *
 * Input:
 *  request    Widget   The instance widget type with the resource values
 *                      requested by the client.
 *  new_w      Widget   The resulting instance widget with allowed
 *                      resource values.
 *  args      ArgList   The widget-creation argument list used to override
 *                      default values.
 *  num_args Cardinal   The number of arguments in args.
 */
static void xap_Initialize(Widget request, Widget new_w, ArgList args,
			     Cardinal *num_args)
{
  XaPgplotWidget w = (XaPgplotWidget) new_w;
  XaPgplotPart *xap = &w->pgplot;
/*
 * Initialize the private attributes.
 */
  xap->next = NULL;
  xap->xaslct_id = xaPgplotClassRec.pgplot_class.widget_id_counter++;
  xap->pgslct_id = 0;  /* This is filled in by the first select-plot opcode */
  xap->device = NULL;
  xap->app = XtWidgetToApplicationContext(new_w);
  xap->input.mask = 0;
  xap->input.callback = 0;
  xap->input.client_data = NULL;
  xap->bg.red = xap->bg.green = xap->bg.blue = 0;
  xap->fg.red = xap->fg.green = xap->fg.blue = 65535;
  xap->pgx = NULL;
/*
 * Record the RGB values of the default background and foreground
 * colors.
 */
  XtVaGetValues(new_w,
	XtVaTypedArg, XtNbackground, XtRColor, &xap->bg, sizeof(xap->bg),
	XtVaTypedArg, XtNforeground, XtRColor, &xap->fg, sizeof(xap->fg),
	NULL);
/*
 * Allocate the PGPLOT-window context descriptor.
 */
  xap->pgx = new_PgxWin(XtDisplay(new_w), 
			XScreenNumberOfScreen(XtScreen(new_w)), (void *) w,
			XtName(new_w), 0, xap_NewPixmap);
  if(!xap->pgx)
    xap_abort(xap, NULL);
/*
 * Compose a sample PGPLOT device-specification for use in opening this
 * widget to PGPLOT.
 */
  xap->device = (char *) malloc(sizeof(char) *
			(strlen(xap->pgx->name) + 1 + strlen(XAP_DEVICE) + 1));
  if(!xap->device)
    xap_abort(xap, "Insufficient memory.\n");
  sprintf(xap->device, "%s/%s", xap->pgx->name, XAP_DEVICE);
/*
 * Check the widget size.
 */
  if(w->core.width==0)
    w->core.width = XAP_DEF_WIDTH;
  if(w->core.height==0)
    w->core.height = XAP_DEF_HEIGHT;
  if(w->core.width < XAP_MIN_WIDTH)
    w->core.width = XAP_MIN_WIDTH;
  if(w->core.height < XAP_MIN_HEIGHT)
    w->core.height = XAP_MIN_HEIGHT;
/*
 * Check color resources.
 */
  if(xap->min_colors < XAP_MIN_COLORS)
    xap->min_colors = XAP_MIN_COLORS;
  if(xap->max_colors > XAP_MAX_COLORS)
    xap->max_colors = XAP_MAX_COLORS;
/*
 * See if the parent widget is a scrolled window.
 */
  return;
}

/*.......................................................................
 * Create and clear the window of a PGPLOT widget instance.
 *
 * Input:
 *  widget             Widget   The widget to be realized.
 *  mask          XtValueMask * The bit-mask that specifies which
 *                              attributes have been set in 'attr'.
 *  attr XSetWindowAttributes * The container of the pre-set window
 *                              attributes specified in 'mask'.
 */
static void xap_Realize(Widget widget, XtValueMask *mask,
		    XSetWindowAttributes *attr)
{
  XaPgplotWidget w = (XaPgplotWidget) widget;
  XaPgplotPart *xap = &w->pgplot;
  PgxWin *pgx = xap->pgx;
/*
 * Get a visual and colormap for the window if necessary.
 */
  if(!pgx->color) {
    if(!xap->share && xap_new_visual(w))  /* If requested try private first */
      xap->share = 1;
    if(xap->share && xap_new_visual(w)) { /* Try for shared colors */
      fprintf(stderr,
      "%s: There are insufficient colors, so black and white will be used.\n",
       XAP_IDENT);
      if(!pgx_bw_visual(pgx))
	xap_abort(xap, "No colors.\n");
    };
  };
/*
 * Keep the resource-value record of the chosen visual and colormap
 * in sync with the values in pgx->color.
 */
  xap->visual = pgx->color->vi->visual;
  w->core.colormap = pgx->color->cmap;
/*
 * Reset the background and foreground colors to match the current
 * X resource values.
 */
  pgx_set_background(pgx, &xap->bg);
  pgx_set_foreground(pgx, &xap->fg);
/*
 * The window attributes passed to this function are based on
 * attributes stored in the core record of the widget instance.
 * By default these attributes (set by X resources) cause the
 * new window to inherit colormap, depth, etc.. from the parent
 * window. Since the call to pgx_new_visual() may have invalidated this
 * we need to install new values. This involves both updating the
 * window attributes to be passed to XtCreateWindow() and modifying
 * the X resources of the core part of the instance structure.
 *
 * It is important to remember that the border and background pixmaps
 * need to be changed to avoid mismatches between their depths and the
 * depth of our chosen visual. To avoid such conflicts we will make sure
 * that pixmaps are not used and substitute solid colors.
 */
/*
 * Add and remove appropriate window attributes.
 */
  *mask |= CWDontPropagate | CWBackPixel | CWBorderPixel | CWColormap;
  *mask &= ~(CWBackPixmap | CWBorderPixmap);
  attr->do_not_propagate_mask = ButtonPressMask | ButtonReleaseMask |
    KeyPressMask | KeyReleaseMask;
  attr->background_pixel = xap->pgx->color->pixel[0];
  attr->border_pixel = xap->pgx->color->pixel[0];
  attr->colormap = w->core.colormap;
/*
 * Record the new attributes in the core part of the instance record.
 */
  XtVaSetValues(widget,
		XtNbackground, xap->pgx->color->pixel[0],
		XtNborderColor, xap->pgx->color->pixel[0],
		XtNdepth, xap->pgx->color->vi->depth,
		NULL);
/*
 * Create the window.
 */
  XtCreateWindow(widget, InputOutput, xap->visual, *mask, attr);
  pgx->window = XtWindow(widget);
/*
 * Create and initialize a graphical context descriptor. This is where
 * Line widths, line styles, fill styles, plot color etc.. are
 * recorded.
 */
  {
    XGCValues gcv;
    gcv.graphics_exposures = False;
    pgx_start_error_watch(pgx);
    pgx->expose_gc = XCreateGC(pgx->display, pgx->window, (unsigned long)
			       (GCGraphicsExposures), &gcv);
    if(pgx_end_error_watch(pgx) || pgx->expose_gc==NULL)
      xap_abort(xap, "Failed to allocate a graphical context.\n");
  };
/*
 * If the widget has scroll-bars make sure that they agree with the
 * window.
 */
  xap_update_clip(w);
/*
 * Prepend the new widget to the list of unassigned widgets to be
 * used by pgbeg().
 */
  xap_PrependWidget(w, XAP_FREE_WIDGETS);
  return;
}

/*.......................................................................
 * PGPLOT Widget destructor function.
 *
 * Input:
 *  widget    Widget   The widget whose resources are to be released.
 */
static void xap_Destroy(Widget widget)
{
  XaPgplotWidget w = (XaPgplotWidget) widget;
  XaPgplotPart *xap = &w->pgplot;
  PgxWin *pgx = xap->pgx;
/*
 * Remove the device from the appropriate list of PGPLOT Athena widgets.
 */
  xap_RemoveWidget(pgx->name, pgx->state ? XAP_ACTIVE_WIDGETS : XAP_FREE_WIDGETS);
/*
 * Delete the window context descriptor.
 */
  xap->pgx = del_PgxWin(pgx);
  return;
}

/*.......................................................................
 * PGPLOT Widget resize function.
 *
 * Input:
 *  widget    Widget   The widget that is being resized.
 */
static void xap_Resize(Widget widget)
{
  XaPgplotWidget w = (XaPgplotWidget) widget;
  xap_update_clip(w);
  if(XtIsRealized(widget)) {
    XtCallCallbackList(widget, w->pgplot.resize_callback, (XtPointer *)0);
  };
}

/*.......................................................................
 * The expose-event handler for PGPLOT widgets.
 *
 * Input:
 *  widget   Widget   The widget that is to be re-drawn.
 *  event    XEvent   The expose event that invoked the callback.
 *  region   Region   The area to be re-drawn.
 */
static void xap_Expose(Widget widget, XEvent *event, Region region)
{
  XaPgplotWidget w = (XaPgplotWidget) widget;
  XaPgplotPart *xap = &w->pgplot;
  PgxWin *pgx = xap->pgx;
  pgx_expose(pgx, event);
  return;
}

/*.......................................................................
 * This function is called whenever one or more resource values are
 * changed, to give the widget a chance to re-configure itself to
 * reflect the new values.
 *
 * Input:
 *  old_widget Widget   A copy of the widget before the change was made.
 *  req_widget Widget   The same as new_w but with the resources evaluated
 *                      by the superclasses.
 *  new_widget Widget   The widget with the new values in place. This
 *                      is also the output widget so changes made to
 *                      this widget will be in the final widget.
 *  args      ArgList   The resource argument list responsible for the
 *                      changes.
 *  num_args Cardinal * The number of arguments in args.
 * Output:
 *  return    Boolean   Return True if the widget needs to be re-displayed,
 *                      or False if not.
 */
static Boolean xap_SetValues(Widget old_widget, Widget req_widget,
			     Widget new_widget,
			     ArgList args, Cardinal *num_args)
{
  XaPgplotWidget old_w = (XaPgplotWidget) old_widget;
  XaPgplotWidget new_w = (XaPgplotWidget) new_widget;
  XaPgplotPart *old_xap = &old_w->pgplot;
  XaPgplotPart *new_xap = &new_w->pgplot;
  Bool redisplay_needed = False;
/*
 * Changes to color resources won't be seen once the widget is
 * realized. Warn the application writer.
 */
  if(new_xap->min_colors != old_xap->min_colors ||
     new_xap->max_colors != old_xap->max_colors ||
     new_xap->share != old_xap->share ||
     new_w->core.colormap != old_w->core.colormap ||
     new_xap->visual != old_xap->visual) {
    if(XtIsRealized(new_widget))
      XtWarning("XtSetValues (XaPgplot): Too late to change color settings.\n");
    new_xap->min_colors = old_xap->min_colors;
    new_xap->max_colors = old_xap->max_colors;
    new_w->core.colormap = old_w->core.colormap;
    new_xap->visual = old_xap->visual;
  };
/*
 * If the background or foreground colors changed, convert them to
 * RGB values and instantiate the new definitions. Note that keeping
 * and using RGB values rather than the pixel value is important
 * because we may not yet have created the window or its final colormap,
 * in which case the pixel will correspond to the wrong colormap. The
 * RGB values are used hereafter by xap_open_widget().
 */
  if(new_w->core.background_pixel != old_w->core.background_pixel) {
    XColor *bg = &new_xap->bg;
    XtVaGetValues(new_widget,
		  XtVaTypedArg, XtNbackground, XtRColor, bg, sizeof(*bg),
		  NULL);
    pgx_set_background(new_xap->pgx, bg);
  };
  if(new_w->pgplot.fgpixel != old_w->pgplot.fgpixel) {
    XColor *fg = &new_xap->fg;
    XtVaGetValues(new_widget,
		  XtVaTypedArg, XtNforeground, XtRColor, fg, sizeof(*fg),
		  NULL);
    pgx_set_foreground(new_xap->pgx, fg);
  };
/*
 * Change the margins?
 */
  if(new_xap->pad_x != old_xap->pad_x || new_xap->pad_y != old_xap->pad_y)
    pgx_set_margin(new_xap->pgx, new_xap->pad_x, new_xap->pad_y);
  return redisplay_needed;
}

/*.......................................................................
 * Whenever the parent widget wishes to resize a PGPLOT widget, this
 * function is called to give the widget a chance to veto or otherwise
 * modify the requested new geometry.
 *
 * Input:
 *  widget             Widget   The widget wrt which the geometry change
 *                              pertains.
 *  request  XtWidgetGeometry * The requested widget geometry.
 * Input/Output:
 *  reply    XtWidgetGeometry * The returned allowed widget geometry.
 * Output:
 *  return   XtGeometryResult   Return status from:
 *                               XtGeometryYes 
 *                                 The requested geometry is acceptible.
 *                               XtGeometryAlmost
 *                                 An acceptible revision of the requested
 *                                 geometry has been encoded in *reply.
 *                               XtGeometryNo
 *                                 The requested geometry is identical
 *                                 to the existing geometry.
 */
static XtGeometryResult xap_Query_Geometry(Widget widget,
					   XtWidgetGeometry *request,
					   XtWidgetGeometry *reply)
{
/*
 * Tell the parent which attributes we are interested in
 * and set them to their current values until otherwise
 * requested.
 */
  reply->request_mode = CWWidth | CWHeight;
  reply->width = widget->core.width;
  reply->height = widget->core.height;
/*
 * Check the requested width.
 */
  if(request->request_mode & CWWidth) {
    reply->width = request->width < XAP_MIN_WIDTH ? XAP_MIN_WIDTH :
      request->width;
  };
/*
 * Check the requested height.
 */
  if(request->request_mode & CWHeight) {
    reply->height = request->height < XAP_MIN_HEIGHT ? XAP_MIN_HEIGHT :
      request->height;
  };
/*
 * Determine the appropriate reply.
 */
  if((request->request_mode & (CWWidth | CWHeight)) == (CWWidth | CWHeight) &&
     request->width == reply->width &&
     request->height == reply->height)
    return XtGeometryYes;
  else if(reply->width == widget->core.width &&
	  reply->height == widget->core.height)
    return XtGeometryNo;
  else
    return XtGeometryAlmost;
}

/*.......................................................................
 * This function is called to abort the application after a fatal
 * error occurs. It doesn't return.
 *
 * Input:
 *  xap   XaPgplotPart *  The PGPLOT part of the widget instance structure.
 *  msg           char *  An error message to abort with, or NULL.
 */
static void xap_abort(XaPgplotPart *xap, char *msg)
{
  XtAppError(xap->app, msg ? msg : "Aborting");
}

/*.......................................................................
 * Find an inactive PGPLOT widget of a given name, open it to PGPLOT,
 * and move it to the head of the active list of widgets.
 *
 * Input:
 *  name         char *  The name of the widget to be opened.
 * Output:
 *  w  XaPgplotWidget    The selected widget, or NULL on error.
 */
static XaPgplotWidget xap_open_widget(char *name)
{
  XaPgplotWidget w; /* The PGPLOT widget to be opened */
/*
 * Remove the named widget from the free-widget list.
 */
  w = xap_RemoveWidget(name, XAP_FREE_WIDGETS);
  if(!w) {
    fprintf(stderr, "%s: Request to open non-existent widget (%s).\n",
	    XAP_IDENT, name ? name : "(null)");
    return NULL;
  };
/*
 * Pre-pend the widget to the active list.
 */
  xap_PrependWidget(w, XAP_ACTIVE_WIDGETS);
/*
 * Open the connection to the PgxWin library.
 */
  pgx_open(w->pgplot.pgx);
  if(!w->pgplot.pgx->state)
    xap_close_widget(name);
/*
 * Reset the background and foreground colors to match the current
 * X resource values.
 */
  pgx_set_background(w->pgplot.pgx, &w->pgplot.bg);
  pgx_set_foreground(w->pgplot.pgx, &w->pgplot.fg);
/*
 * Allow for margins.
 */
  pgx_set_margin(w->pgplot.pgx, w->pgplot.pad_x, w->pgplot.pad_y);
/*
 * Reset its scroll-bars.
 */
  return w;
}

/*.......................................................................
 * Find an active PGPLOT widget of a given name, close it to PGPLOT and
 * move it to the head of the inactive list of widgets.
 *
 * Input:
 *  name             char *  The name of the widget.
 * Output:
 *  return XaPgplotWidget    The selected widget, or NULL if not found.
 */
static XaPgplotWidget xap_close_widget(char *name)
{
  XaPgplotWidget w;
/*
 * Remove the widget from the active list.
 */
  w = xap_RemoveWidget(name, XAP_ACTIVE_WIDGETS);
  if(!w) {
    fprintf(stderr, "%s: Request to close non-existent widget (%s).\n",
	    XAP_IDENT, name ? name : "(null)");
    return NULL;
  };
/*
 * Remove cursor handler.
 */
  xap_DisarmCursor(w);
/*
 * Close the connection to the PgxWin library.
 */
  pgx_close(w->pgplot.pgx);
/*
 * Invalidate the pgslct() id. The next time that the widget is opened
 * to PGPLOT a different value will likely be used.
 */
  w->pgplot.pgslct_id = 0;
/*
 * Prepend the widget to the free list.
 */
  xap_PrependWidget(w, XAP_FREE_WIDGETS);
  return w;
}

/*.......................................................................
 * Lookup a widget by name from a given list of widgets.
 *
 * Input:
 *  name             char * The name of the widget.
 *  type              int   The enumerated name of the list to search,
 *                          from:
 *                            XAP_ACTIVE_WIDGETS
 *                            XAP_FREE_WIDGETS
 * Output:
 *  prev   XaPgplotWidget * *prev will either be NULL if the widget
 *                          was at the head of the list, or be the
 *                          widget in the list that immediately precedes
 *                          the specified widget.
 *  return XaPgplotWidget   The located widget, or NULL if not found.
 */
static XaPgplotWidget xap_FindWidgetByName(char *name, int type,
					   XaPgplotWidget *prev)
{
  XaPgplotList *widget_list; /* The list to be searched */
  widget_list = xap_WidgetList(type);
  if(widget_list && name) {
    XaPgplotWidget last = NULL;
    XaPgplotWidget node = widget_list->head;
    for( ; node; last = node, node = node->pgplot.next) {
      if(strcmp(node->pgplot.pgx->name, name)==0) {
	if(prev)
	  *prev = last;
	return node;
      };
    };
  };
/*
 * Widget not found.
 */
  if(prev)
    *prev = NULL;
  return NULL;
}

/*.......................................................................
 * Lookup a widget by its PGPLOT id from a given list of widgets.
 *
 * Input:
 *  xaslct_id         int   The number used by PGPLOT to select the
 *                          device.
 *  type              int   The enumerated name of the list to search,
 *                          from:
 *                            XAP_ACTIVE_WIDGETS
 *                            XAP_FREE_WIDGETS
 * Output:
 *  prev   XaPgplotWidget * *prev will either be NULL if the widget
 *                          was at the head of the list, or be the
 *                          widget in the list that immediately precedes
 *                          the specified widget.
 *  return XaPgplotWidget   The located widget, or NULL if not found.
 */
static XaPgplotWidget xap_FindWidgetByID(int xaslct_id, int type,
					   XaPgplotWidget *prev)
{
  XaPgplotList *widget_list; /* The list to be searched */
  widget_list = xap_WidgetList(type);
  if(widget_list) {
    XaPgplotWidget last = NULL;
    XaPgplotWidget node = widget_list->head;
    for( ; node; last = node, node = node->pgplot.next) {
      if(xaslct_id == node->pgplot.xaslct_id) {
	if(prev)
	  *prev = last;
	return node;
      };
    };
  };
/*
 * Widget not found.
 */
  if(prev)
    *prev = NULL;
  return NULL;
}

/*.......................................................................
 * Lookup one of the PGPLOT class widget lists by its enumerated type.
 *
 * Input:
 *  type            int   The enumerated name of the list, from:
 *                          XAP_ACTIVE_WIDGETS
 *                          XAP_FREE_WIDGETS
 * Output:
 *  return XaPgplotList * The widget list, or NULL if not recognized.
 */
static XaPgplotList *xap_WidgetList(int type)
{
  switch(type) {
  case XAP_ACTIVE_WIDGETS:
    return &xaPgplotClassRec.pgplot_class.active_widgets;
  case XAP_FREE_WIDGETS:
    return &xaPgplotClassRec.pgplot_class.free_widgets;
  default:
    fprintf(stderr, "xap_WidgetList: No such list.\n");
  };
  return NULL;
}

/*.......................................................................
 * Remove a given widget from one of the PGPLOT class widget lists.
 *
 * Input:
 *  name             char * The name of the widget to be removed from
 *                          the list.
 *  type              int   The enumerated name of the list from which to
 *                          remove the widget, from:
 *                            XAP_ACTIVE_WIDGETS
 *                            XAP_FREE_WIDGETS
 * Output:
 *  return XaPgplotWidget   The removed widget, or NULL if not found.
 */
static XaPgplotWidget xap_RemoveWidget(char *name, int type)
{
  XaPgplotList *widget_list; /* The list to remove the widget from */
  XaPgplotWidget w = NULL;   /* The widget being removed */
  XaPgplotWidget prev;       /* The widget preceding w in the list */
/*
 * Get the widget list.
 */
  widget_list = xap_WidgetList(type);
  if(widget_list) {
    w = xap_FindWidgetByName(name, type, &prev);
    if(w) {
      if(prev) {
	prev->pgplot.next = w->pgplot.next;
      } else {
	widget_list->head = w->pgplot.next;
      };
      w->pgplot.next = NULL;
    };
  };
  return w;
}

/*.......................................................................
 * Prepend a PGPLOT widget to a given PGPLOT class widget list.
 *
 * Input:
 *  w      XaPgplotWidget   The widget to add to the list.
 *  type              int   The enumerated name of the list to add to,
 *                          from:
 *                            XAP_ACTIVE_WIDGETS
 *                            XAP_FREE_WIDGETS
 * Output:
 *  return XaPgplotWidget   The added widget (the same as w), or NULL
 *                          on error.
 */
static XaPgplotWidget xap_PrependWidget(XaPgplotWidget w, int type)
{
  XaPgplotList *widget_list;  /* The list to prepend the widget to */
/*
 * Get the widget list.
 */
  widget_list = xap_WidgetList(type);
  if(widget_list) {
    w->pgplot.next = widget_list->head;
    widget_list->head = w;
  };
  return w;
}

/*.......................................................................
 * Return the currently selected PGPLOT device.
 *
 * Input:
 *  context           char * If no XaPgplot device is currently selected
 *                           and context!=NULL then, an error message of
 *                           the form printf("%s: ...\n", context) will
 *                           be written to stderr reporting that no
 *                           device is open.
 * Output:
 *  return  XaPgplotWidget   The currently selected PGPLOT device, or
 *                           NULL if no device is currently selected.
 */
static XaPgplotWidget xap_CurrentWidget(char *context)
{
  XaPgplotWidget w = xaPgplotClassRec.pgplot_class.active_widgets.head;
  if(w) {
/*
 * We need a window.
 */
    if(!XtIsRealized((Widget)w)) {
      if(context) {
	fprintf(stderr, "%s: PGPLOT widget \"%s\" is not realized.\n",
		context, w->pgplot.pgx->name);
      };
      w = NULL;
    };
  } else {
    if(context)
      fprintf(stderr, "%s: No /xa device is currently selected.\n", context);
  };
  return w;
}

/*.......................................................................
 * This is the only external entry point to the /xa device driver.
 * It is called by PGPLOT to open, perform operations on, return
 * information about and close /xathena windows.
 *
 * Input:
 *  ifunc   int *  The PGPLOT operation code to be executed.
 * Input/output:
 *  rbuf  float *  A general buffer for input/output of float values.
 *  nbuf    int *  Where relevant this is used to return the number of
 *                 elements in rbuf[]. Also used on input to specify
 *                 number of pixels in the line-of-pixels primitive.
 *  chr    char *  A general buffer for string I/O.
 *  lchr    int *  Where relevant this is used to send and return the
 *                 number of significant characters in chr.
 * Input:
 *  len     int    Added to the call line by the FORTRAN compiler.
 *                 This contains the declared size of chr[].  
 */
#ifdef VMS
void xadriv(ifunc, rbuf, nbuf, chrdsc, lchr)
     int *ifunc;
     float rbuf[];
     int *nbuf;
     struct dsc$descriptor_s *chrdsc; /* VMS FORTRAN string descriptor */
     int *lchr;
{
  int len = chrdsc->dsc$w_length;
  char *chr = chrdsc->dsc$a_pointer;
#else
void XADRIV(ifunc, rbuf, nbuf, chr, lchr, len)
 int   *ifunc, *nbuf, *lchr;
 int   len;
 float rbuf[];
 char  *chr;
{
#endif
/*
 * Get the active widget if there is one.
 */
  XaPgplotWidget w = xap_CurrentWidget(NULL);
  XaPgplotPart *xap = w ? &w->pgplot : NULL;
  PgxWin *pgx = xap ? xap->pgx : NULL;
  int i;
/*
 * Flush buffered opcodes.
 */
  pgx_pre_opcode(pgx, *ifunc);
/*
 * Branch on the specified PGPLOT opcode.
 */
  switch(*ifunc) {

/*--- IFUNC=1, Return device name ---------------------------------------*/

  case 1:
    {
      char *dev_name = "XATHENA (X window widget_name/xa)";
      strncpy(chr, dev_name, len);
      *lchr = strlen(dev_name);
      for(i = *lchr; i < len; i++)
	chr[i] = ' ';
    };
    break;

/*--- IFUNC=2, Return physical min and max for plot device, and range
               of color indices -----------------------------------------*/
  case 2:
    rbuf[0] = 0.0;
    rbuf[1] = -1.0;  /* Report no effective max plot width */
    rbuf[2] = 0.0;
    rbuf[3] = -1.0;  /* Report no effective max plot height */
    rbuf[4] = 0.0;
    rbuf[5] = (pgx && !pgx->bad_device) ? pgx->color->ncol-1 : 1;
    *nbuf = 6;
    break;

/*--- IFUNC=3, Return device resolution ---------------------------------*/

  case 3:
    pgx_get_resolution(pgx, &rbuf[0], &rbuf[1]);
    rbuf[2] = 1.0;		/* Device coordinates per pixel */
    *nbuf = 3;
    break;

/*--- IFUNC=4, Return misc device info ----------------------------------*/

  case 4:
    chr[0] = 'I'; /* Interactive device */
    chr[1] = 'X'; /* Cursor is available and opcode 27 is desired */
    chr[2] = 'N'; /* No dashed lines */
    chr[3] = 'A'; /* Area fill available */
    chr[4] = 'T'; /* Thick lines */
    chr[5] = 'R'; /* Rectangle fill available */
    chr[6] = 'P'; /* Line of pixels available */
    chr[7] = 'N'; /* Don't prompt on pgend */
    chr[8] = 'Y'; /* Can return color representation */
    chr[9] = 'N'; /* Not used */
    chr[10]= 'S'; /* Area-scroll available */
    *lchr = 11;
    break;

/*--- IFUNC=5, Return default file name ---------------------------------*/

  case 5:
    chr[0] = '\0';  /* Default name is "" */
    *lchr = 0;
    break;

/*--- IFUNC=6, Return default physical size of plot ---------------------*/

  case 6:
    pgx_def_size(pgx, XAP_DEF_WIDTH, XAP_DEF_HEIGHT, rbuf, nbuf);
    break;

/*--- IFUNC=7, Return misc defaults -------------------------------------*/

  case 7:
    rbuf[0] = 1.0;
    *nbuf = 1;
    break;

/*--- IFUNC=8, Select plot ----------------------------------------------*/

  case 8:
    {
      XaPgplotWidget new_w = xap_FindWidgetByID((int)(rbuf[1]+0.5),
						XAP_ACTIVE_WIDGETS, NULL);
      if(new_w) {
	new_w->pgplot.pgslct_id = (int) (rbuf[0]+0.5);
	xap_RemoveWidget(new_w->pgplot.pgx->name, XAP_ACTIVE_WIDGETS);
	xap_PrependWidget(new_w, XAP_ACTIVE_WIDGETS);
      } else {
	fprintf(stderr, "%s: [Select plot] No such open device.\n", XAP_IDENT);
      };
    };
    break;

/*--- IFUNC=9, Open workstation -----------------------------------------*/

  case 9:
/*
 * Assign the returned device unit number and success indicator.
 * Assume failure to open until the workstation is open.
 */
    rbuf[0] = rbuf[1] = 0.0;
    *nbuf = 2;
/*
 * Prepare the display name.
 */
    if(*lchr >= len) {
      fprintf(stderr, "%s: Widget name too long.\n", XAP_IDENT);
      return;
    } else {
      chr[*lchr] = '\0';
    };
/*
 * Get the requested widget from the free widget list.
 */
    w = xap_open_widget(chr);
    if(!w)
      return;
    rbuf[0] = w->pgplot.xaslct_id; /* The number used to select this device */
    rbuf[1] = 1.0;
    *nbuf = 2;
    break;

/*--- IFUNC=10, Close workstation ---------------------------------------*/

  case 10:
/*
 * Remove the device from the list of open devices.
 */
    if(pgx)
      xap_close_widget(pgx->name);
    break;

/*--- IFUNC=11, Begin picture -------------------------------------------*/

  case 11:
    pgx_begin_picture(pgx, rbuf);
    break;

/*--- IFUNC=12, Draw line -----------------------------------------------*/

  case 12:
    pgx_draw_line(pgx, rbuf);
    break;

/*--- IFUNC=13, Draw dot ------------------------------------------------*/

  case 13:
    pgx_draw_dot(pgx, rbuf);
    break;

/*--- IFUNC=14, End picture ---------------------------------------------*/

  case 14:
    break;

/*--- IFUNC=15, Select color index --------------------------------------*/

  case 15:
    pgx_set_ci(pgx, (int) (rbuf[0] + 0.5));
    break;

/*--- IFUNC=16, Flush buffer. -------------------------------------------*/

  case 16:
    pgx_flush(pgx);
    break;

/*--- IFUNC=17, Read cursor. --------------------------------------------*/

  case 17:
    if(w) {
      xap_DisarmCursor(w);
      if(!XtIsManaged((Widget)w))
	XtManageChild((Widget)w);
    };
    pgx_read_cursor(pgx, rbuf, chr, nbuf, lchr);
    break;

/*--- IFUNC=18, Erase alpha screen. -------------------------------------*/
  /* (Not implemented: no alpha screen) */
  case 18:
    break;

/*--- IFUNC=19, Set line style. -----------------------------------------*/
  /* (Not implemented: should not be called) */
  case 19:
    break;

/*--- IFUNC=20, Polygon fill. -------------------------------------------*/

  case 20:
    pgx_poly_fill(pgx, rbuf);
    break;

/*--- IFUNC=21, Set color representation. -------------------------------*/

  case 21:
    pgx_set_rgb(pgx, (int)(rbuf[0]+0.5), rbuf[1],rbuf[2],rbuf[3]);
    break;

/*--- IFUNC=22, Set line width. -----------------------------------------*/

  case 22:
    pgx_set_lw(pgx, rbuf[0]);
    break;

/*--- IFUNC=23, Escape --------------------------------------------------*/
    /* (Not implemented: ignored) */
  case 23:
    break;

/*--- IFUNC=24, Rectangle Fill. -----------------------------------------*/

  case 24:
    pgx_rect_fill(pgx, rbuf);
    break;

/*--- IFUNC=25, ---------------------------------------------------------*/
  /* (Not implemented: ignored) */
  case 25:
    break;

/*--- IFUNC=26, Line of pixels ------------------------------------------*/

  case 26:
    pgx_pix_line(pgx, rbuf, nbuf);
    break;

/*--- IFUNC=27, World-coordinate scaling --------------------------------*/

  case 27:
    pgx_set_world(pgx, rbuf);
    break;

/*--- IFUNC=29, Query color representation ------------------------------*/
  case 29:
    pgx_get_rgb(pgx, rbuf, nbuf);
    break;

/*--- IFUNC=30, Scroll rectangle ----------------------------------------*/
  case 30:
    pgx_scroll_rect(pgx, rbuf);
    break;

/*--- IFUNC=?, ----------------------------------------------------------*/

  default:
    fprintf(stderr, "%s: Ignoring unimplemented opcode=%d.\n",
	    XAP_IDENT, *ifunc);
    *nbuf = -1;
    break;
  };
  return;
}

/*.......................................................................
 * This function is called upon by the pgxwin toolkit whenever the
 * pixmap used as backing store needs to be resized.
 *
 * Input:
 *  pgx      PgxWin *   The pgxwin toolkit context descriptor.
 *  width  unsigned     The desired new pixmap width.
 *  height unsigned     The desired new pixmap height.
 */
static void xap_NewPixmap(PgxWin *pgx, unsigned width, unsigned height)
{
  pgx_new_pixmap(pgx, width, height);
  return;
}

/*.......................................................................
 * This function provides an asynchronous alternative to pgband() and
 * pgcurs(). It creates an event handler which ensures that the X cursor
 * is augmented with selected rubber-band graphics when visible, and
 * which calls a specified user cursor-input callback when the user
 * presses a key or button over the window. As with pgband() all
 * specified and reported coordinates are world coordinates. The
 * cursor is automatically disarmed in xadriv() if the pgband() opcode
 * is invoked. It is also disarmed when the pgplot close-workstation
 * opcode is invoked.
 *
 * Input:
 *  widget       Widget   The PGPLOT widget to connect a cursor to.
 *  mode            int   The type of cursor augmentation (see XaPgplot.h).
 *  xref,yref     float   The world-coordinate reference point for band-type
 *                        cursors.
 *  callback XtCallbackProc The callback function to call when input events
 *                        are received.
 *  client_data    void * Client-specific data to be sent to the callback
 *                        function.
 * Output:
 *  return          int   0 - OK.
 *                        1 - Error.
 */
int xap_arm_cursor(Widget widget, int mode, float xref, float yref,
		    XtCallbackProc callback, void *client_data)
{
  XaPgplotWidget w = (XaPgplotWidget) widget;
/*
 * Check the arguments.
 */
  if(!widget) {
    fprintf(stderr, "xap_arm_cursor: NULL widget.\n");
    return 1;
  };
/*
 * Make sure that the widget is currently open to PGPLOT.
 */
  if(w->pgplot.pgslct_id == 0) {
    fprintf(stderr, "xap_arm_cursor: The widget is not open to PGPLOT.\n");
    return 1;
  };
/*
 * Delegate the work to an internal function.
 */
  return xap_ArmCursor(w, mode, xref, yref, callback, client_data);
}

/*.......................................................................
 * Erase the cursor, remove input callbacks and remove the cursor
 * event handler.
 *
 * Input:
 *  widget     Widget   The PGPLOT widget to disconnect the cursor from.
 * Output:
 *  return        int   0 - OK.
 *                      1 - Error.
 */
int xap_disarm_cursor(Widget widget)
{
  if(!widget) {
    fprintf(stderr, "xap_disarm_cursor: NULL widget intercepted.\n");
    return 1;
  };
  return xap_DisarmCursor((XaPgplotWidget)widget);
}

/*.......................................................................
 * This is the cursor event handler registered by xap_arm_cursor().
 */
static void xap_CursorHandler(Widget widget, XtPointer client_data,
			      XEvent *event, Boolean *cont)
{
  XaPgplotWidget w = (XaPgplotWidget) widget;
  XaPgplotPart *xap = &w->pgplot;
  PgxWin *pgx = xap->pgx;
  float rbuf[2];
  char key;
/*
 * Handle the event.
 */
  if(pgx_cursor_event(pgx, event, rbuf, &key) && xap->input.callback) {
    XapCursorCallbackStruct call_data;
    pgx_dev2world(pgx, rbuf);
    call_data.x = rbuf[0];
    call_data.y = rbuf[1];
    call_data.key = key;
    (*xap->input.callback)(widget, (XtPointer) xap->input.client_data,
			   (XtPointer) &call_data);
  };
/*
 * Handle errors.
 */
  if(pgx->bad_device) {
    *cont = False;
    xap_DisarmCursor(w);
  } else {
    *cont = True;
  };
  return;
}

/*.......................................................................
 * The private work-horse function of xap_arm_cursor(). Note that
 * this function takes an XaPgplotWidget argument whereas xap_arm_cursor()
 * takes a generic Widget argument.
 *
 * Input:
 *  w    XaPgplotWidget   The PGPLOT widget to connect a cursor to.
 *  mode            int   The type of cursor augmentation (see XaPgplot.h).
 *  xref,yref     float   The world-coordinate reference point for band-type
 *                        cursors.
 *  callback XtCallbackProc The callback function to call when input events
 *                        are received, or 0 if keyboard and button
 *                        events are to be handled externally.
 *  client_data    void * Client-specific data to be sent to the callback
 *                        function.
 * Output:
 *  return          int   0 - OK.
 *                        1 - Error.
 */
static int xap_ArmCursor(XaPgplotWidget w, int mode, float xref, float yref,
			 XtCallbackProc callback, void *client_data)
{
  Widget widget = (Widget) w;
  XaPgplotPart *xap = &w->pgplot;
  PgxWin *pgx = xap->pgx;
  float rbuf[2];
/*
 * Remove any existing cursor.
 */
  xap_DisarmCursor(w);
/*
 * Convert xref, yref from world coordinates to device coordinates.
 */
  rbuf[0] = xref;
  rbuf[1] = yref;
  pgx_world2dev(pgx, rbuf);
/*
 * Raise the cursor.
 */
  if(pgx_set_cursor(pgx, -1, mode, 0, rbuf, rbuf))
    return 1;
/*
 * If the pointer is currently in the window, record its position
 * and draw the cursor.
 */
  if(pgx_locate_cursor(pgx))
    pgx_draw_cursor(pgx);
/*
 * Assemble the cursor-hander event-mask.
 */
  xap->input.mask = EnterWindowMask | LeaveWindowMask | PointerMotionMask;
/*
 * Only select for keyboard and button input if a callback was
 * provided.
 */
  if(callback)
    xap->input.mask |= KeyPressMask | ButtonPressMask;
/*
 * Record the callback and its data.
 */
  xap->input.callback = callback;
  xap->input.client_data = client_data;
/*
 * Register an event handler to handle asychronous cursor input.
 */
  XtAddEventHandler(widget, xap->input.mask, False, xap_CursorHandler,
		    (XtPointer) 0);
/*
 * Make sure that the widget is visible.
 */
  if(!XtIsManaged(widget))
    XtManageChild(widget);
  return 0;
}

/*.......................................................................
 * The private work-horse function of xap_disarm_cursor(). Note that
 * this function takes an XaPgplotWidget argument whereas
 * xap_disarm_cursor() takes a generic Widget argument.
 *
 *  w  XaPgplotWidget   The widget to disconnect the cursor from.
 * Output:
 *  return        int   0 - OK.
 *                      1 - Error.
 */
static int xap_DisarmCursor(XaPgplotWidget w)
{
  if(w) {
    XaPgplotPart *xap = &w->pgplot;
    PgxWin *pgx = xap->pgx;
/*
 * Do nothing if the cursor is inactive.
 */
    if(xap->input.mask == NoEventMask)
      return 0;
/*
 * Remove the current event handler.
 */
    XtRemoveEventHandler((Widget) w, xap->input.mask, False,
			 xap_CursorHandler, (XtPointer) 0);
/*
 * Remove the callback function and its data.
 */
    xap->input.callback = 0;
    xap->input.client_data = NULL;
/*
 * Erase the cursor.
 */
    if(pgx_erase_cursor(pgx) ||
       pgx_set_cursor(pgx, 0, PGX_NORM_CURSOR, 0, NULL, NULL))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Get the visual and colormap for a new window as specified by X
 * resource values.
 *
 * Input:
 *  w    XaPgplotWidget   The PGPLOT widget.
 * Output:
 *  return          int   0 - OK.
 *                        1 - Error.
 */
static int xap_new_visual(XaPgplotWidget w)
{
  XaPgplotPart *xap = &w->pgplot;
  PgxWin *pgx = xap->pgx;
/*
 * Allocate colors from parent visual and colormap?
 */
  if(xap->visual == CopyFromParent || w->core.colormap == CopyFromParent) {
/*
 * Find the first parent widget that has a window.
 */
    Widget parent = (Widget) w;
    do {
      parent = XtParent(parent);
    } while(parent && XtWindow(parent)==None);
    if(!parent) {
      fprintf(stderr, "xap_new_visual: No parent window found.\n");
      return 1;
    };
/*
 * Locate the visual and colormap of the parent and allocate colors from them.
 */
    if(!pgx_window_visual(pgx, XtWindow(parent), xap->min_colors,
			  xap->max_colors, xap->share))
      return 1;
  }
/*
 * Allocate colors from a specified colormap and visual.
 */
  else {
    if(!pgx_adopt_visual(pgx, XVisualIDFromVisual(xap->visual),
			 w->core.colormap, xap->min_colors, xap->max_colors,
			 xap->share))
      return 1;
  };
/*
 * Record what kind of colors were actually allocated.
 */
  xap->share = xap->pgx->color->readonly;
  return 0;
}

/*.......................................................................
 * Return an unambiguous PGPLOT device-specification that can be used
 * as the FILE argument of cpgbeg() to open a given PGPLOT widget.
 *
 * Input:
 *  widget  Widget   The PGPLOT widget to return a device string for.
 * Output:
 *  return    char * The PGPLOT device-specication. Note that the returned
 *                   string is owned by the widget driver and must not be
 *                   free()d or overwritten.
 */
char *xap_device_name(Widget widget)
{
  if(!widget || XtClass(widget) != xaPgplotWidgetClass) {
    fprintf(stderr, "xap_device_name: Not a Athena PGPLOT widget.\n");
    return "/null";
  };
  return ((XaPgplotWidget) widget)->pgplot.device;
}

/*.......................................................................
 * Return the pgslct_id of the given widget. This can then be used with
 * the cpgslct() function to select the widget as the currently
 * active widget.
 *
 * Input:
 *  widget  Widget   The PGPLOT widget to return a device string for.
 * Output:
 *  return     int   The PGPLOT device-id. This will be 0 if the widget
 *                   is not currently open to PGPLOT.
 */
int xap_device_id(Widget widget)
{
  if(!widget || XtClass(widget) != xaPgplotWidgetClass) {
    fprintf(stderr, "xap_device_name: Not a Athena PGPLOT widget.\n");
    return 0;
  } else {
    XaPgplotWidget w = (XaPgplotWidget) widget;
    if(w->pgplot.pgslct_id <= 0) {
      fprintf(stderr,
      "xap_device_id: The specified widget is not currently open to PGPLOT.\n");
    };
    return w->pgplot.pgslct_id;
  };
}

/*.......................................................................
 * The following is a convenience none-variadic function for creating
 * a PGPLOT widget. Note that XtManageChild() should be applied to the
 * returned widget.
 *
 * Input:
 *  parent     Widget    The parent widget to adopt.
 *  name         char *  The name to give the widget.
 *  arglist   ArgList    A list of X resources.
 *  argcount Cardinal    The number of X resources.
 * Output:
 *  return     Widget    The new PGPLOT widget.
 */
Widget XaCreatePgplot(Widget parent, char *name, ArgList arglist,
		      Cardinal argcount)
{
  return XtCreateWidget(name, xaPgplotWidgetClass, parent, arglist, argcount);
}

/*.......................................................................
 * Update the clip-area of the window to prevent pgxwin functions from
 * drawing over the highlight-borders.
 *
 * Input:
 *  w     XaPgplotWidget   The pgplot widget instance.
 */
static void xap_update_clip(XaPgplotWidget w)
{
  (void) pgx_update_clip(w->pgplot.pgx, 1, w->core.width, w->core.height,
			 0
			 );
}

/*.......................................................................
 * This function is a XtResourceDefaultProc function used to return
 * the default background color. It is used to initialize the
 * XtNbackground resource.
 */
static void xap_GetDefaultBackgroundColor(Widget widget, int offset,
					  XrmValue *value)
{
  static Pixel pixel;
  pixel = BlackPixel(XtDisplay(widget),XScreenNumberOfScreen(XtScreen(widget)));
  value->addr = (XtPointer) &pixel;
}
/*.......................................................................
 * This function is a XtResourceDefaultProc function used to return
 * the default foreground color. It is used to initialize the
 * XtNforeground resource.
 */
static void xap_GetDefaultForegroundColor(Widget widget, int offset,
					  XrmValue *value)
{
  static Pixel pixel;
  pixel = WhitePixel(XtDisplay(widget),XScreenNumberOfScreen(XtScreen(widget)));
  value->addr = (XtPointer) &pixel;
}
/*.......................................................................
 * This is an application-level utility function for converting from
 * PGPLOT world coordinates to X-window pixel coordinates.
 *
 * Input:
 *  widget  Widget    The PGPLOT widget whose coordinates are to be
 *                    converted.
 *  wx, wy   float    The PGPLOT world coordinates to be converted.
 * Output:
 *  px, py     int *  On output, *px and *py will be assigned with the
 *                    X-window pixel coordinates that correspond to wx,wy.
 *  return     int    0 - OK.
 *                    1 - Error.
 */
int xap_world_to_pixel(Widget widget, float wx, float wy, int *px, int *py)
{
  XaPgplotWidget w = (XaPgplotWidget) widget;
/*
 * Check the arguments.
 */
  if(!widget) {
    fprintf(stderr, "xap_world_to_pixel: NULL widget.\n");
    return 1;
  };
/*
 * Delegate the conversion to an internal function.
 */
  return xap_WorldToPixel(w, wx, wy, px, py);
}

/*.......................................................................
 * This is an application-level utility function for converting from
 * X-window pixel coordinates to PGPLOT world coordinates.
 *
 * Input:
 *  widget  Widget    The PGPLOT widget whose coordinates are to be
 *                    converted.
 *  px, py     int *  The X-window pixel coordinates to be converted.
 * Output:
 *  wx, wy   float *  On output, *wx and *wy will be assigned with the
 *                    PGPLOT world coordinates that correspond to px,py.
 *  return     int    0 - OK.
 *                    1 - Error.
 */
int xap_pixel_to_world(Widget widget, int px, int py, float *wx, float *wy)
{
  XaPgplotWidget w = (XaPgplotWidget) widget;
/*
 * Check the arguments.
 */
  if(!widget) {
    fprintf(stderr, "xap_pixel_to_world: NULL widget.\n");
    return 1;
  };
/*
 * Delegate the conversion to an internal function.
 */
  return xap_PixelToWorld(w, px, py, wx, wy);
}

/*.......................................................................
 * This is an internal function for converting from X-window pixel
 * coordinates to PGPLOT world coordinates.
 *
 * Input:
 *  w  XaPgplotWidget   The widget whose coordinates are to be converted.
 *  px, py        int * The X-window pixel coordinates to be converted.
 * Output:
 *  wx, wy      float * On output, *wx and *wy will be assigned with the
 *                      PGPLOT world coordinates that correspond to px,py.
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int xap_PixelToWorld(XaPgplotWidget w, int px, int py,
			    float *wx, float *wy)
{
  PgxWin *pgx = w->pgplot.pgx;
  float rbuf[2];
/*
 * Convert the specified pixel coordinates to world coordinates.
 */
  pgx_win2dev(pgx, px, py, rbuf);
  pgx_dev2world(pgx, rbuf);
/*
 * Assign the return values if possible.
 */
  if(wx) *wx = rbuf[0];
  if(wy) *wy = rbuf[1];
  return 0;
}

/*.......................................................................
 * This is an internal function for converting from PGPLOT world
 * coordinates to X-window pixel coordinates.
 *
 * Input:
 *  w  XaPgplotWidget   The widget whose coordinates are to be converted.
 *  wx, wy      float   The PGPLOT world coordinates to be converted.
 * Output:
 *  px, py       int *  On output, *px and *py will be assigned with the
 *                      X-window pixel coordinates that correspond to wx,wy.
 *  return        int   0 - OK.
 *                      1 - Error.
 */
static int xap_WorldToPixel(XaPgplotWidget w, float wx, float wy,
			    int *px, int *py)
{
  PgxWin *pgx = w->pgplot.pgx;
  float rbuf[2];
/*
 * Convert the world coordinate to pixel coordinates.
 */
  rbuf[0] = wx;
  rbuf[1] = wy;
  pgx_world2dev(pgx, rbuf);
  pgx_dev2win(pgx, rbuf, px, py);
  return 0;
}
