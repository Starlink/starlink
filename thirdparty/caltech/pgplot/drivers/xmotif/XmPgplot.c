/*
 * Changes:
 *
 * 17 March 97 - The XmNTraversalOn resource was being ignored by the
 *               cursor event-handler. Thus Tab characters and
 *               the first ButtonPress after loss of input-focus were
 *               interpretted as focus control events. This has now
 *               been remedied. When XmNTraversalOn is False these
 *               events are now treated as normal input events.
 */

/* Motif pgplot widget class implementation */

#include <Xm/XmP.h>
#include <X11/StringDefs.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <X11/keysym.h>

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
 * Allow xmdriv to be calleable by FORTRAN using the two commonest
 * calling conventions. Both conventions append length arguments for
 * each FORTRAN string at the end of the argument list, and convert the
 * name to lower-case, but one post-pends an underscore to the function
 * name (PG_PPU) while the other doesn't. Note the VMS is handled
 * separately below. For other calling conventions you must write a
 * C wrapper routine to call xmdriv() or xmdriv_().
 */
#ifdef PG_PPU
#define XMDRIV xmdriv_
#else
#define XMDRIV xmdriv
#endif

#include "XmPgplotP.h"

/*
 * Define the default attributes of the widget.
 */
#define XMP_MIN_WIDTH 64     /* Minimum width (pixels) */
#define XMP_MIN_HEIGHT 64    /* Minimum height (pixels) */
#define XMP_DEF_WIDTH 256    /* Default width (pixels) */
#define XMP_DEF_HEIGHT 256   /* Default height (pixels) */
#define XMP_MIN_COLORS 2     /* Min number of colors per colormap */
#define XMP_DEF_COLORS 100   /* Default number of colors to try for */
#define XMP_MAX_COLORS 255   /* Max number of colors per colormap */
#define XMP_DEF_MARGIN 20    /* The number of pixels to assign to the margin */
#define XMP_DEF_SHARE 0      /* Default to allocating shared colors */

/*
 * Specify the name to prefix errors with.
 */
#define XMP_IDENT "PgplotWidget"

static void xmp_GetDefaultHighlightColor(Widget widget, int offset,
					 XrmValue *value);
static void xmp_GetDefaultBackgroundColor(Widget widget, int offset,
					  XrmValue *value);
static void xmp_GetDefaultForegroundColor(Widget widget, int offset,
					  XrmValue *value);
/*
 * Define all the X resources that are to be understood by the
 * widget.
 */
static XtResource resources[] = {
  {
    XmpNminColors,
    XmpCMinColors,
    XtRInt,
    sizeof(int),
    XtOffsetOf(XmPgplotRec, pgplot.min_colors),
    XtRImmediate,
    (XtPointer) XMP_MIN_COLORS
  },
  {
    XmpNmaxColors,
    XmpCMaxColors,
    XtRInt,
    sizeof(int),
    XtOffsetOf(XmPgplotRec, pgplot.max_colors),
    XtRImmediate,
    (XtPointer) XMP_DEF_COLORS
  },
  {
    XmNvisual,
    XmCVisual,
#if XmVersion <= 1001
    XtRVisual,
#else
    XmRVisual,
#endif
    sizeof(Visual *),
    XtOffsetOf(XmPgplotRec, pgplot.visual),
    XtRImmediate,
    (XtPointer) CopyFromParent
  },
  {
    XmNresizeCallback,
    XmCCallback, XmRCallback, sizeof(XtCallbackList),
    XtOffsetOf(XmPgplotRec, pgplot.resize_callback),
    XmRImmediate, (XtPointer) NULL
  },
  {
    XmNhighlightColor,
    XmCHighlightColor,
    XmRPixel, 
    sizeof(Pixel),
    XtOffsetOf(XmPgplotRec, primitive.highlight_color),
    XmRCallProc, (XtPointer) xmp_GetDefaultHighlightColor
  },
  {
    XmNbackground,
    XmCBackground,
    XmRPixel, 
    sizeof(Pixel),
    XtOffsetOf(XmPgplotRec, core.background_pixel),
    XmRCallProc, (XtPointer) xmp_GetDefaultBackgroundColor
  },
  {
    XmNforeground,
    XmCForeground,
    XmRPixel, 
    sizeof(Pixel),
    XtOffsetOf(XmPgplotRec, primitive.foreground),
    XmRCallProc, (XtPointer) xmp_GetDefaultForegroundColor
  },
  {
    XmpNpadX,
    XmpCPadX,
    XtRDimension,
    sizeof(Dimension),
    XtOffsetOf(XmPgplotRec, pgplot.pad_x),
    XtRImmediate,
    (XtPointer) XMP_DEF_MARGIN
  },
  {
    XmpNpadY,
    XmpCPadY,
    XtRDimension,
    sizeof(Dimension),
    XtOffsetOf(XmPgplotRec, pgplot.pad_y),
    XtRImmediate,
    (XtPointer) XMP_DEF_MARGIN
  },
  {
    XmpNshare,
    XmpCShare,
    XtRBoolean,
    sizeof(Boolean),
    XtOffsetOf(XmPgplotRec, pgplot.share),
    XtRImmediate,
    (XtPointer) XMP_DEF_SHARE
  },
};

/*
 * Declare class method functions.
 */
static void xmp_ClassPartInit(WidgetClass w);
static void xmp_Initialize(Widget request, Widget new_w, 
				     ArgList args, Cardinal *num_args);
static void xmp_Realize(Widget widget, XtValueMask *mask, 
				  XSetWindowAttributes *attributes);
static void xmp_Resize(Widget widget);
static void xmp_Destroy(Widget widget);
static void xmp_Expose(Widget widget, XEvent *event, Region region);
static Boolean xmp_SetValues(Widget old_widget, Widget req_widget, 
				       Widget new_widget, 
				       ArgList args, Cardinal *num_args);
static XtGeometryResult xmp_Query_Geometry(Widget widget, 
					    XtWidgetGeometry *request, 
					    XtWidgetGeometry *reply);
static void XmpDestroyParentCallback(Widget w, 
                              XtPointer client_data, XtPointer call_data);

/*
 * Actions and their default translations should go here.
 * Note that specifying NULL for for the core.tm_table class
 * field results in the Motif Primitive initialization function
 * not augmenting the table with its keyboard traversal translations.
 * Thus we must supply a translation table, even if it is empty.
 */
static char default_translations[] = "";

/*
 * Define the Motif Pgplot class shared context descriptor.
 */
externaldef(xmpgplotclassrec) XmPgplotClassRec xmPgplotClassRec = {

  { /* core_class fields */
    /* superclass         */    (WidgetClass) &xmPrimitiveClassRec,
    /* class_name         */    "XmPgplot",
    /* widget_size        */    sizeof(XmPgplotRec),
    /* class_initialize   */    NULL,
    /* class_part_initiali*/    xmp_ClassPartInit,
    /* class_inited       */    FALSE,
    /* initialize         */    xmp_Initialize,
    /* initialize_hook    */    NULL,
    /* realize            */    xmp_Realize,
    /* actions            */    NULL,
    /* num_actions        */    0,
    /* resources          */    resources,
    /* num_resources      */    XtNumber(resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    TRUE,
    /* compress_exposure  */    XtExposeCompressMultiple,
    /* compress_enterleave*/    TRUE,
    /* visible_interest   */    FALSE,
    /* destroy            */    xmp_Destroy,
    /* resize             */    xmp_Resize,
    /* expose             */    xmp_Expose,
    /* set_values         */    xmp_SetValues,
    /* set_values_hook    */    NULL,
    /* set_values_almost  */    XtInheritSetValuesAlmost,
    /* get_values_hook    */    NULL,
    /* accept_focus       */    NULL,
    /* version            */    XtVersion,
    /* callback_private   */    NULL,
    /* tm_table           */    default_translations,
    /* query_geometry     */    xmp_Query_Geometry,
    /* display accel      */    XtInheritDisplayAccelerator,
    /* extension          */    NULL
  },

  { /* primitive_class fields       */
    /* border_highlight   */    (XtWidgetProc) _XtInherit,
    /* border_unhighlight */    (XtWidgetProc) _XtInherit,
    /* translations       */    XtInheritTranslations,
    /* arm_and_activate   */    NULL,
    /* syn resources      */    NULL,
    /* num get_resources  */    0,
    /* extension          */    NULL
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
externaldef(xmpgplotwidgetclass) WidgetClass xmPgplotWidgetClass = (WidgetClass) &xmPgplotClassRec;

/*
 * Private functions.
 */
static void xmp_abort(XmPgplotPart *xmp, char *msg);
static XmPgplotWidget xmp_open_widget(char *name);
static XmPgplotWidget xmp_close_widget(char *name);
static void xmp_update_scroll_bars(XmPgplotWidget w);
static void xmp_update_clip(XmPgplotWidget w);
static void xmp_NewPixmap(PgxWin *pgx, unsigned width, 
				    unsigned height);
static void xmp_scroll_callback(Widget widget, 
					  XtPointer client_data, 
					  XtPointer call_data);
static void xmp_CursorHandler(Widget widget, XtPointer client_data, 
			      XEvent *event, Boolean *cont);
static int xmp_ArmCursor(XmPgplotWidget w, int mode, float xref, 
			   float yref, XtCallbackProc callback, void *client_data);
static int xmp_DisarmCursor(XmPgplotWidget w);
static int xmp_new_visual(XmPgplotWidget w);
static int xmp_WorldToPixel(XmPgplotWidget w, float wx, float wy,
			    int *px, int *py);
static int xmp_PixelToWorld(XmPgplotWidget w, int px, int py,
			    float *wx, float *wy);

/* Enumerate the PGPLOT class widget lists */

#define XMP_ACTIVE_WIDGETS 1
#define XMP_FREE_WIDGETS 2

static XmPgplotList *xmp_WidgetList(int type);

static XmPgplotWidget xmp_FindWidgetByName(char *name, int type, 
						     XmPgplotWidget *prev);
static XmPgplotWidget xmp_FindWidgetByID(int xmslct_id, int type, 
						   XmPgplotWidget *prev);

static XmPgplotWidget xmp_RemoveWidget(char *name, int type);
static XmPgplotWidget xmp_PrependWidget(XmPgplotWidget w, int type);
static XmPgplotWidget xmp_CurrentWidget(char *context);

/*.......................................................................
 * This is called once to initialize the PGPLOT part of the widget class
 * structure when the first widget instance of this type is created.
 *
 * Input:
 *  w   WidgetClass   The class record to be initialized.
 */
static void xmp_ClassPartInit(WidgetClass w)
{
  XmPgplotWidgetClass class = (XmPgplotWidgetClass) w;
  XmPgplotClassPart *pgplot = &class->pgplot_class;
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
static void xmp_Initialize(Widget request, Widget new_w, ArgList args,
			     Cardinal *num_args)
{
  XmPgplotWidget w = (XmPgplotWidget) new_w;
  XmPgplotPart *xmp = &w->pgplot;
/*
 * Initialize the private attributes.
 */
  xmp->next = NULL;
  xmp->xmslct_id = xmPgplotClassRec.pgplot_class.widget_id_counter++;
  xmp->pgslct_id = 0;  /* This is filled in by the first select-plot opcode */
  xmp->device = NULL;
  xmp->app = XtWidgetToApplicationContext(new_w);
  xmp->scroll.is_scrolled = 0;
  xmp->scroll.w_hbar = NULL;
  xmp->scroll.w_vbar = NULL;
  xmp->scroll.width = 0;
  xmp->scroll.height = 0;
  xmp->scroll.x = 0;
  xmp->scroll.y = 0;
  xmp->input.mask = 0;
  xmp->input.callback = 0;
  xmp->input.client_data = NULL;
  xmp->bg.red = xmp->bg.green = xmp->bg.blue = 0;
  xmp->fg.red = xmp->fg.green = xmp->fg.blue = 65535;
  xmp->pgx = NULL;
/*
 * Record the RGB values of the default background and foreground
 * colors.
 */
  XtVaGetValues(new_w,
	XtVaTypedArg, XmNbackground, XmRColor, &xmp->bg, sizeof(xmp->bg),
	XtVaTypedArg, XmNforeground, XmRColor, &xmp->fg, sizeof(xmp->fg),
	NULL);
/*
 * Allocate the PGPLOT-window context descriptor.
 */
  xmp->pgx = new_PgxWin(XtDisplay(new_w), 
			XScreenNumberOfScreen(XtScreen(new_w)), (void *) w,
			XtName(new_w), 0, xmp_NewPixmap);
  if(!xmp->pgx)
    xmp_abort(xmp, NULL);
/*
 * Compose a sample PGPLOT device-specification for use in opening this
 * widget to PGPLOT.
 */
  xmp->device = (char *) malloc(sizeof(char) *
			(strlen(xmp->pgx->name) + 1 + strlen(XMP_DEVICE) + 1));
  if(!xmp->device)
    xmp_abort(xmp, "Insufficient memory.\n");
  sprintf(xmp->device, "%s/%s", xmp->pgx->name, XMP_DEVICE);
/*
 * Check the widget size.
 */
  if(w->core.width==0)
    w->core.width = XMP_DEF_WIDTH;
  if(w->core.height==0)
    w->core.height = XMP_DEF_HEIGHT;
  if(w->core.width < XMP_MIN_WIDTH)
    w->core.width = XMP_MIN_WIDTH;
  if(w->core.height < XMP_MIN_HEIGHT)
    w->core.height = XMP_MIN_HEIGHT;
/*
 * Check color resources.
 */
  if(xmp->min_colors < XMP_MIN_COLORS)
    xmp->min_colors = XMP_MIN_COLORS;
  if(xmp->max_colors > XMP_MAX_COLORS)
    xmp->max_colors = XMP_MAX_COLORS;
/*
 * See if the parent widget is a scrolled window.
 */
  if(XtIsSubclass(XtParent(new_w), xmScrolledWindowWidgetClass)) {
    Widget w_scroll = XtParent(new_w);
    XmpScroll *scroll = &xmp->scroll;
    scroll->is_scrolled = 1;
    scroll->width = w->core.width;
    scroll->height = w->core.height;
    scroll->w_hbar = XtVaCreateManagedWidget("hbar", xmScrollBarWidgetClass,
					  w_scroll,
					  XmNorientation, XmHORIZONTAL,
					  XmNminimum, 0,
					  XmNmaximum, scroll->width,
					  XmNsliderSize, scroll->width,
					  XmNpageIncrement, scroll->width,
					  XmNincrement, 1,
					  NULL);
    XtAddCallback(scroll->w_hbar, XmNvalueChangedCallback, xmp_scroll_callback,
		  w);
    XtAddCallback(scroll->w_hbar, XmNdragCallback, xmp_scroll_callback, w);
    scroll->w_vbar = XtVaCreateManagedWidget("vbar", xmScrollBarWidgetClass,
					  w_scroll,
					  XmNorientation, XmVERTICAL,
					  XmNminimum, 0,
					  XmNmaximum, scroll->height,
					  XmNsliderSize, scroll->height,
					  XmNpageIncrement, scroll->height,
					  XmNincrement, 1,
					  NULL);
    XtAddCallback(scroll->w_vbar, XmNvalueChangedCallback, xmp_scroll_callback,
		  w);
    XtAddCallback(scroll->w_vbar, XmNdragCallback, xmp_scroll_callback, w);
    XmScrolledWindowSetAreas(w_scroll, scroll->w_hbar, scroll->w_vbar, new_w);
  };
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
static void xmp_Realize(Widget widget, XtValueMask *mask,
		    XSetWindowAttributes *attr)
{
  XmPgplotWidget w = (XmPgplotWidget) widget;
  XmPgplotPart *xmp = &w->pgplot;
  PgxWin *pgx = xmp->pgx;
/*
 * Get a visual and colormap for the window if necessary.
 */
  if(!pgx->color) {
    if(!xmp->share && xmp_new_visual(w))  /* If requested try private first */
      xmp->share = 1;
    if(xmp->share && xmp_new_visual(w)) { /* Try for shared colors */
      fprintf(stderr,
      "%s: There are insufficient colors, so black and white will be used.\n",
       XMP_IDENT);
      if(!pgx_bw_visual(pgx))
	xmp_abort(xmp, "No colors.\n");
    };
  };
/*
 * Keep the resource-value record of the chosen visual and colormap
 * in sync with the values in pgx->color.
 */
  xmp->visual = pgx->color->vi->visual;
  w->core.colormap = pgx->color->cmap;
/*
 * Reset the background and foreground colors to match the current
 * X resource values.
 */
  pgx_set_background(pgx, &xmp->bg);
  pgx_set_foreground(pgx, &xmp->fg);
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
  attr->background_pixel = xmp->pgx->color->pixel[0];
  attr->border_pixel = xmp->pgx->color->pixel[0];
  attr->colormap = w->core.colormap;
/*
 * Record the new attributes in the core part of the instance record.
 */
  XtVaSetValues(widget,
		XmNbackground, xmp->pgx->color->pixel[0],
		XmNborderColor, xmp->pgx->color->pixel[0],
		XmNdepth, xmp->pgx->color->vi->depth,
		NULL);
/*
 * Create the window.
 */
  XtCreateWindow(widget, InputOutput, xmp->visual, *mask, attr);
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
      xmp_abort(xmp, "Failed to allocate a graphical context.\n");
  };
/*
 * If the widget has scroll-bars make sure that they agree with the
 * window.
 */
  xmp_update_scroll_bars(w);
  xmp_update_clip(w);
/*
 * Prepend the new widget to the list of unassigned widgets to be
 * used by pgbeg().
 */
  xmp_PrependWidget(w, XMP_FREE_WIDGETS);
  return;
}

/*.......................................................................
 * PGPLOT Widget destructor function.
 *
 * Input:
 *  widget    Widget   The widget whose resources are to be released.
 */
static void xmp_Destroy(Widget widget)
{
  XmPgplotWidget w = (XmPgplotWidget) widget;
  XmPgplotPart *xmp = &w->pgplot;
  PgxWin *pgx = xmp->pgx;
/*
 * Remove the device from the appropriate list of PGPLOT Motif widgets.
 */
  xmp_RemoveWidget(pgx->name, pgx->state ? XMP_ACTIVE_WIDGETS : XMP_FREE_WIDGETS);
/*
 * Delete the window context descriptor.
 */
  xmp->pgx = del_PgxWin(pgx);
  return;
}

/*.......................................................................
 * PGPLOT Widget resize function.
 *
 * Input:
 *  widget    Widget   The widget that is being resized.
 */
static void xmp_Resize(Widget widget)
{
  XmPgplotWidget w = (XmPgplotWidget) widget;
  xmp_update_clip(w);
  if(XtIsRealized(widget)) {
    w->pgplot.scroll.x = 0;
    w->pgplot.scroll.y = 0;
    xmp_update_scroll_bars(w);
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
static void xmp_Expose(Widget widget, XEvent *event, Region region)
{
  XmPgplotWidget w = (XmPgplotWidget) widget;
  XmPgplotPart *xmp = &w->pgplot;
  XmPgplotWidgetClass class = (XmPgplotWidgetClass) XtClass(widget);
  PgxWin *pgx = xmp->pgx;
/*
 * Re-draw highlight border.
 */
  if(w->primitive.highlighted) {
    class->primitive_class.border_highlight(widget);
  } else {
    class->primitive_class.border_unhighlight(widget);
  };
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
static Boolean xmp_SetValues(Widget old_widget, Widget req_widget,
			     Widget new_widget,
			     ArgList args, Cardinal *num_args)
{
  XmPgplotWidget old_w = (XmPgplotWidget) old_widget;
  XmPgplotWidget new_w = (XmPgplotWidget) new_widget;
  XmPgplotPart *old_xmp = &old_w->pgplot;
  XmPgplotPart *new_xmp = &new_w->pgplot;
  Bool redisplay_needed = False;
/*
 * Changes to color resources won't be seen once the widget is
 * realized. Warn the application writer.
 */
  if(new_xmp->min_colors != old_xmp->min_colors ||
     new_xmp->max_colors != old_xmp->max_colors ||
     new_xmp->share != old_xmp->share ||
     new_w->core.colormap != old_w->core.colormap ||
     new_xmp->visual != old_xmp->visual) {
    if(XtIsRealized(new_widget))
      XtWarning("XtSetValues (XmPgplot): Too late to change color settings.\n");
    new_xmp->min_colors = old_xmp->min_colors;
    new_xmp->max_colors = old_xmp->max_colors;
    new_xmp->share = old_xmp->share;
    new_w->core.colormap = old_w->core.colormap;
    new_xmp->visual = old_xmp->visual;
  };
/*
 * If the highlight border-thickness changed update the window
 * clip region to exclude the border from future pgxwin graphics.
 */
  if(new_w->primitive.highlight_thickness !=
     old_w->primitive.highlight_thickness) {
    xmp_update_clip(new_w);
    redisplay_needed = True;
  };
/*
 * If the background or foreground colors changed, convert them to
 * RGB values and instantiate the new definitions. Note that keeping
 * and using RGB values rather than the pixel value is important
 * because we may not yet have created the window or its final colormap,
 * in which case the pixel will correspond to the wrong colormap. The
 * RGB values are used hereafter by xmp_open_widget().
 */
  if(new_w->core.background_pixel != old_w->core.background_pixel) {
    XColor *bg = &new_xmp->bg;
    XtVaGetValues(new_widget,
		  XtVaTypedArg, XmNbackground, XmRColor, bg, sizeof(*bg),
		  NULL);
    pgx_set_background(new_xmp->pgx, bg);
  };
  if(new_w->primitive.foreground != old_w->primitive.foreground) {
    XColor *fg = &new_xmp->fg;
    XtVaGetValues(new_widget,
		  XtVaTypedArg, XmNforeground, XmRColor, fg, sizeof(*fg),
		  NULL);
    pgx_set_foreground(new_xmp->pgx, fg);
  };
/*
 * Change the margins?
 */
  if(new_xmp->pad_x != old_xmp->pad_x || new_xmp->pad_y != old_xmp->pad_y)
    pgx_set_margin(new_xmp->pgx, new_xmp->pad_x, new_xmp->pad_y);
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
static XtGeometryResult xmp_Query_Geometry(Widget widget,
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
    reply->width = request->width < XMP_MIN_WIDTH ? XMP_MIN_WIDTH :
      request->width;
  };
/*
 * Check the requested height.
 */
  if(request->request_mode & CWHeight) {
    reply->height = request->height < XMP_MIN_HEIGHT ? XMP_MIN_HEIGHT :
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
 *  xmp   XmPgplotPart *  The PGPLOT part of the widget instance structure.
 *  msg           char *  An error message to abort with, or NULL.
 */
static void xmp_abort(XmPgplotPart *xmp, char *msg)
{
  XtAppError(xmp->app, msg ? msg : "Aborting");
}

/*.......................................................................
 * Find an inactive PGPLOT widget of a given name, open it to PGPLOT,
 * and move it to the head of the active list of widgets.
 *
 * Input:
 *  name         char *  The name of the widget to be opened.
 * Output:
 *  w  XmPgplotWidget    The selected widget, or NULL on error.
 */
static XmPgplotWidget xmp_open_widget(char *name)
{
  XmPgplotWidget w; /* The PGPLOT widget to be opened */
/*
 * Remove the named widget from the free-widget list.
 */
  w = xmp_RemoveWidget(name, XMP_FREE_WIDGETS);
  if(!w) {
    fprintf(stderr, "%s: Request to open non-existent widget (%s).\n",
	    XMP_IDENT, name ? name : "(null)");
    return NULL;
  };
/*
 * Pre-pend the widget to the active list.
 */
  xmp_PrependWidget(w, XMP_ACTIVE_WIDGETS);
/*
 * Open the connection to the PgxWin library.
 */
  pgx_open(w->pgplot.pgx);
  if(!w->pgplot.pgx->state)
    xmp_close_widget(name);
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
  xmp_update_scroll_bars(w);
  return w;
}

/*.......................................................................
 * Find an active PGPLOT widget of a given name, close it to PGPLOT and
 * move it to the head of the inactive list of widgets.
 *
 * Input:
 *  name             char *  The name of the widget.
 * Output:
 *  return XmPgplotWidget    The selected widget, or NULL if not found.
 */
static XmPgplotWidget xmp_close_widget(char *name)
{
  XmPgplotWidget w;
/*
 * Remove the widget from the active list.
 */
  w = xmp_RemoveWidget(name, XMP_ACTIVE_WIDGETS);
  if(!w) {
    fprintf(stderr, "%s: Request to close non-existent widget (%s).\n",
	    XMP_IDENT, name ? name : "(null)");
    return NULL;
  };
/*
 * Remove cursor handler.
 */
  xmp_DisarmCursor(w);
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
  xmp_PrependWidget(w, XMP_FREE_WIDGETS);
  return w;
}

/*.......................................................................
 * Lookup a widget by name from a given list of widgets.
 *
 * Input:
 *  name             char * The name of the widget.
 *  type              int   The enumerated name of the list to search,
 *                          from:
 *                            XMP_ACTIVE_WIDGETS
 *                            XMP_FREE_WIDGETS
 * Output:
 *  prev   XmPgplotWidget * *prev will either be NULL if the widget
 *                          was at the head of the list, or be the
 *                          widget in the list that immediately precedes
 *                          the specified widget.
 *  return XmPgplotWidget   The located widget, or NULL if not found.
 */
static XmPgplotWidget xmp_FindWidgetByName(char *name, int type,
					   XmPgplotWidget *prev)
{
  XmPgplotList *widget_list; /* The list to be searched */
  widget_list = xmp_WidgetList(type);
  if(widget_list && name) {
    XmPgplotWidget last = NULL;
    XmPgplotWidget node = widget_list->head;
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
 *  xmslct_id         int   The number used by PGPLOT to select the
 *                          device.
 *  type              int   The enumerated name of the list to search,
 *                          from:
 *                            XMP_ACTIVE_WIDGETS
 *                            XMP_FREE_WIDGETS
 * Output:
 *  prev   XmPgplotWidget * *prev will either be NULL if the widget
 *                          was at the head of the list, or be the
 *                          widget in the list that immediately precedes
 *                          the specified widget.
 *  return XmPgplotWidget   The located widget, or NULL if not found.
 */
static XmPgplotWidget xmp_FindWidgetByID(int xmslct_id, int type,
					   XmPgplotWidget *prev)
{
  XmPgplotList *widget_list; /* The list to be searched */
  widget_list = xmp_WidgetList(type);
  if(widget_list) {
    XmPgplotWidget last = NULL;
    XmPgplotWidget node = widget_list->head;
    for( ; node; last = node, node = node->pgplot.next) {
      if(xmslct_id == node->pgplot.xmslct_id) {
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
 *                          XMP_ACTIVE_WIDGETS
 *                          XMP_FREE_WIDGETS
 * Output:
 *  return XmPgplotList * The widget list, or NULL if not recognized.
 */
static XmPgplotList *xmp_WidgetList(int type)
{
  switch(type) {
  case XMP_ACTIVE_WIDGETS:
    return &xmPgplotClassRec.pgplot_class.active_widgets;
  case XMP_FREE_WIDGETS:
    return &xmPgplotClassRec.pgplot_class.free_widgets;
  default:
    fprintf(stderr, "xmp_WidgetList: No such list.\n");
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
 *                            XMP_ACTIVE_WIDGETS
 *                            XMP_FREE_WIDGETS
 * Output:
 *  return XmPgplotWidget   The removed widget, or NULL if not found.
 */
static XmPgplotWidget xmp_RemoveWidget(char *name, int type)
{
  XmPgplotList *widget_list; /* The list to remove the widget from */
  XmPgplotWidget w = NULL;   /* The widget being removed */
  XmPgplotWidget prev;       /* The widget preceding w in the list */
/*
 * Get the widget list.
 */
  widget_list = xmp_WidgetList(type);
  if(widget_list) {
    w = xmp_FindWidgetByName(name, type, &prev);
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
 *  w      XmPgplotWidget   The widget to add to the list.
 *  type              int   The enumerated name of the list to add to,
 *                          from:
 *                            XMP_ACTIVE_WIDGETS
 *                            XMP_FREE_WIDGETS
 * Output:
 *  return XmPgplotWidget   The added widget (the same as w), or NULL
 *                          on error.
 */
static XmPgplotWidget xmp_PrependWidget(XmPgplotWidget w, int type)
{
  XmPgplotList *widget_list;  /* The list to prepend the widget to */
/*
 * Get the widget list.
 */
  widget_list = xmp_WidgetList(type);
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
 *  context           char * If no XmPgplot device is currently selected
 *                           and context!=NULL then, an error message of
 *                           the form printf("%s: ...\n", context) will
 *                           be written to stderr reporting that no
 *                           device is open.
 * Output:
 *  return  XmPgplotWidget   The currently selected PGPLOT device, or
 *                           NULL if no device is currently selected.
 */
static XmPgplotWidget xmp_CurrentWidget(char *context)
{
  XmPgplotWidget w = xmPgplotClassRec.pgplot_class.active_widgets.head;
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
      fprintf(stderr, "%s: No /xm device is currently selected.\n", context);
  };
  return w;
}

/*.......................................................................
 * This is the only external entry point to the /xm device driver.
 * It is called by PGPLOT to open, perform operations on, return
 * information about and close /xmotif windows.
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
void xmdriv(ifunc, rbuf, nbuf, chrdsc, lchr)
     int *ifunc;
     float rbuf[];
     int *nbuf;
     struct dsc$descriptor_s *chrdsc; /* VMS FORTRAN string descriptor */
     int *lchr;
{
  int len = chrdsc->dsc$w_length;
  char *chr = chrdsc->dsc$a_pointer;
#else
void XMDRIV(ifunc, rbuf, nbuf, chr, lchr, len)
 int   *ifunc, *nbuf, *lchr;
 int   len;
 float rbuf[];
 char  *chr;
{
#endif
/*
 * Get the active widget if there is one.
 */
  XmPgplotWidget w = xmp_CurrentWidget(NULL);
  XmPgplotPart *xmp = w ? &w->pgplot : NULL;
  PgxWin *pgx = xmp ? xmp->pgx : NULL;
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
      char *dev_name = "XMOTIF (X window widget_name/xm)";
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
    pgx_def_size(pgx, XMP_DEF_WIDTH, XMP_DEF_HEIGHT, rbuf, nbuf);
    break;

/*--- IFUNC=7, Return misc defaults -------------------------------------*/

  case 7:
    rbuf[0] = 1.0;
    *nbuf = 1;
    break;

/*--- IFUNC=8, Select plot ----------------------------------------------*/

  case 8:
    {
      XmPgplotWidget new_w = xmp_FindWidgetByID((int)(rbuf[1]+0.5),
						XMP_ACTIVE_WIDGETS, NULL);
      if(new_w) {
	new_w->pgplot.pgslct_id = (int) (rbuf[0]+0.5);
	xmp_RemoveWidget(new_w->pgplot.pgx->name, XMP_ACTIVE_WIDGETS);
	xmp_PrependWidget(new_w, XMP_ACTIVE_WIDGETS);
      } else {
	fprintf(stderr, "%s: [Select plot] No such open device.\n", XMP_IDENT);
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
      fprintf(stderr, "%s: Widget name too long.\n", XMP_IDENT);
      return;
    } else {
      chr[*lchr] = '\0';
    };
/*
 * Get the requested widget from the free widget list.
 */
    w = xmp_open_widget(chr);
    if(!w)
      return;
    rbuf[0] = w->pgplot.xmslct_id; /* The number used to select this device */
    rbuf[1] = 1.0;
    *nbuf = 2;
    break;

/*--- IFUNC=10, Close workstation ---------------------------------------*/

  case 10:
/*
 * Remove the device from the list of open devices.
 */
    if(pgx)
      xmp_close_widget(pgx->name);
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
      xmp_DisarmCursor(w);
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
	    XMP_IDENT, *ifunc);
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
static void xmp_NewPixmap(PgxWin *pgx, unsigned width, unsigned height)
{
  XmPgplotWidget w = (XmPgplotWidget) pgx->context;
/*
 * Record the requested dimensions then hand the job of allocating the
 * pixmap back to the pgxwin toolkit.
 */
  w->pgplot.scroll.width = width;
  w->pgplot.scroll.height = height;
  w->pgplot.scroll.x = 0;
  w->pgplot.scroll.y = 0;
  xmp_update_scroll_bars(w);
  pgx_new_pixmap(pgx, width, height);
  return;
}

/*.......................................................................
 * Whenever the size of a pixmap and/or window of a PGPLOT winget are
 * changed, this function should be called to adjust scroll bars if the
 * parent is a scrolled window widget.
 *
 * Input:
 *  w     XmPgplotWidget   The pgplot widget instance.
 */
static void xmp_update_scroll_bars(XmPgplotWidget w)
{
  if(w->pgplot.scroll.is_scrolled) {
    XmPgplotPart *xmp = &w->pgplot;
    XmpScroll *scroll = &xmp->scroll;
/*
 * Ensure that the scroll area has a finite size.
 */
    if(scroll->width < 1)
      scroll->width = 1;
    if(scroll->height < 1)
      scroll->height = 1;
/*
 * Update the horizontal scroll-bar.
 */
    XtVaSetValues(xmp->scroll.w_hbar,
		  XmNsliderSize, w->core.width,
		  XmNpageIncrement, w->core.width,
		  XmNmaximum, scroll->width > w->core.width ?
		              scroll->width : w->core.width,
		  XmNvalue, scroll->x / scroll->width,
		  NULL);
/*
 * Update the vertical scroll-bar.
 */
    XtVaSetValues(xmp->scroll.w_vbar,
		  XmNsliderSize, w->core.height,
		  XmNpageIncrement, w->core.height,
		  XmNmaximum, scroll->height > w->core.height ?
		              scroll->height : w->core.height,
		  XmNvalue, scroll->y / scroll->height,
		  NULL);
/*
 * Tell pgplot about the current scroll and pan values.
 */
    pgx_scroll(xmp->pgx, scroll->x, scroll->y);
  };
  return;
}

/*.......................................................................
 * This function is called whenever a scrollbar of a parent scrolled
 * window widget is moved.
 */
static void xmp_scroll_callback(Widget widget, XtPointer client_data,
				XtPointer call_data)
{
  XmScrollBarCallbackStruct *bar = (XmScrollBarCallbackStruct *) call_data;
  XmPgplotWidget w = (XmPgplotWidget) client_data;
  XmPgplotPart *xmp = &w->pgplot;
  XmpScroll *scroll = &xmp->scroll;
/*
 * Determine which scroll-bar was responsible for this call.
 */
  if(widget == scroll->w_hbar) {
    scroll->x = bar->value;
  } else if(widget == scroll->w_vbar) {
    scroll->y = bar->value;
  };
/*
 * Scroll the pixmap.
 */
  pgx_scroll(xmp->pgx, xmp->scroll.x, xmp->scroll.y);
  return;
}

/*.......................................................................
 * This function provides an asynchronous alternative to pgband() and
 * pgcurs(). It creates an event handler which ensures that the X cursor
 * is augmented with selected rubber-band graphics when visible, and
 * which calls a specified user cursor-input callback when the user
 * presses a key or button over the window. As with pgband() all
 * specified and reported coordinates are world coordinates. The
 * cursor is automatically disarmed in xmdriv() if the pgband() opcode
 * is invoked. It is also disarmed when the pgplot close-workstation
 * opcode is invoked.
 *
 * Input:
 *  widget       Widget   The PGPLOT widget to connect a cursor to.
 *  mode            int   The type of cursor augmentation (see XmPgplot.h).
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
int xmp_arm_cursor(Widget widget, int mode, float xref, float yref,
		    XtCallbackProc callback, void *client_data)
{
  XmPgplotWidget w = (XmPgplotWidget) widget;
/*
 * Check the arguments.
 */
  if(!widget) {
    fprintf(stderr, "xmp_arm_cursor: NULL widget.\n");
    return 1;
  };
/*
 * Make sure that the widget is currently open to PGPLOT.
 */
  if(w->pgplot.pgslct_id == 0) {
    fprintf(stderr, "xmp_arm_cursor: The widget is not open to PGPLOT.\n");
    return 1;
  };
/*
 * Delegate the work to an internal function.
 */
  return xmp_ArmCursor(w, mode, xref, yref, callback, client_data);
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
int xmp_disarm_cursor(Widget widget)
{
  if(!widget) {
    fprintf(stderr, "xmp_disarm_cursor: NULL widget intercepted.\n");
    return 1;
  };
  return xmp_DisarmCursor((XmPgplotWidget)widget);
}

/*.......................................................................
 * This is the cursor event handler registered by xmp_arm_cursor().
 */
static void xmp_CursorHandler(Widget widget, XtPointer client_data,
			      XEvent *event, Boolean *cont)
{
  XmPgplotWidget w = (XmPgplotWidget) widget;
  XmPgplotPart *xmp = &w->pgplot;
  PgxWin *pgx = xmp->pgx;
  float rbuf[2];
  char key;
/*
 * Perform input-focus management?
 */
  if(XmIsTraversable(widget)) {
/*
 * Allow Motif Tab-group traversal by ignoring all Tab-key groups.
 */
    if(event->type == KeyPress && XLookupKeysym(&event->xkey, 0) == XK_Tab)
      return;
/*
 * If the event that we are responding to is a button press and
 * the widget doesn't currently have keyboard input focus, turn
 * keyboard focus on and discard the button press.
 */
    if(event->type == ButtonPress && !w->primitive.have_traversal &&
       _XmGetFocusPolicy(widget) == XmEXPLICIT) {
      XmProcessTraversal(widget, XmTRAVERSE_CURRENT);
      return;
    };
  };
/*
 * Handle the event.
 */
  if(pgx_cursor_event(pgx, event, rbuf, &key) && xmp->input.callback) {
    XmpCursorCallbackStruct call_data;
    pgx_dev2world(pgx, rbuf);
    call_data.x = rbuf[0];
    call_data.y = rbuf[1];
    call_data.key = key;
    (*xmp->input.callback)(widget, (XtPointer) xmp->input.client_data,
			   (XtPointer) &call_data);
  };
/*
 * Handle errors.
 */
  if(pgx->bad_device) {
    *cont = False;
    xmp_DisarmCursor(w);
  } else {
    *cont = True;
  };
  return;
}

/*.......................................................................
 * The private work-horse function of xmp_arm_cursor(). Note that
 * this function takes an XmPgplotWidget argument whereas xmp_arm_cursor()
 * takes a generic Widget argument.
 *
 * Input:
 *  w    XmPgplotWidget   The PGPLOT widget to connect a cursor to.
 *  mode            int   The type of cursor augmentation (see XmPgplot.h).
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
static int xmp_ArmCursor(XmPgplotWidget w, int mode, float xref, float yref,
			 XtCallbackProc callback, void *client_data)
{
  Widget widget = (Widget) w;
  XmPgplotPart *xmp = &w->pgplot;
  PgxWin *pgx = xmp->pgx;
  float rbuf[2];
/*
 * Remove any existing cursor.
 */
  xmp_DisarmCursor(w);
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
  xmp->input.mask = EnterWindowMask | LeaveWindowMask | PointerMotionMask;
/*
 * Only select for keyboard and button input if a callback was
 * provided.
 */
  if(callback)
    xmp->input.mask |= KeyPressMask | ButtonPressMask;
/*
 * Record the callback and its data.
 */
  xmp->input.callback = callback;
  xmp->input.client_data = client_data;
/*
 * Register an event handler to handle asychronous cursor input.
 */
  XtAddEventHandler(widget, xmp->input.mask, False, xmp_CursorHandler,
		    (XtPointer) 0);
/*
 * Make sure that the widget is visible.
 */
  if(!XtIsManaged(widget))
    XtManageChild(widget);
  return 0;
}

/*.......................................................................
 * The private work-horse function of xmp_disarm_cursor(). Note that
 * this function takes an XmPgplotWidget argument whereas
 * xmp_disarm_cursor() takes a generic Widget argument.
 *
 *  w  XmPgplotWidget   The widget to disconnect the cursor from.
 * Output:
 *  return        int   0 - OK.
 *                      1 - Error.
 */
static int xmp_DisarmCursor(XmPgplotWidget w)
{
  if(w) {
    XmPgplotPart *xmp = &w->pgplot;
    PgxWin *pgx = xmp->pgx;
/*
 * Do nothing if the cursor is inactive.
 */
    if(xmp->input.mask == NoEventMask)
      return 0;
/*
 * Remove the current event handler.
 */
    XtRemoveEventHandler((Widget) w, xmp->input.mask, False,
			 xmp_CursorHandler, (XtPointer) 0);
/*
 * Remove the callback function and its data.
 */
    xmp->input.callback = 0;
    xmp->input.client_data = NULL;
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
 *  w    XmPgplotWidget   The PGPLOT widget.
 * Output:
 *  return          int   0 - OK.
 *                        1 - Error.
 */
static int xmp_new_visual(XmPgplotWidget w)
{
  XmPgplotPart *xmp = &w->pgplot;
  PgxWin *pgx = xmp->pgx;
/*
 * Allocate colors from parent visual and colormap?
 */
  if(xmp->visual == CopyFromParent || w->core.colormap == CopyFromParent) {
/*
 * Find the first parent widget that has a window.
 */
    Widget parent = (Widget) w;
    do {
      parent = XtParent(parent);
    } while(parent && XtWindow(parent)==None);
    if(!parent) {
      fprintf(stderr, "xmp_new_visual: No parent window found.\n");
      return 1;
    };
/*
 * Locate the visual and colormap of the parent and allocate colors from them.
 */
    if(!pgx_window_visual(pgx, XtWindow(parent), xmp->min_colors,
			  xmp->max_colors, xmp->share))
      return 1;
  }
/*
 * Allocate colors from a specified colormap and visual.
 */
  else {
    if(!pgx_adopt_visual(pgx, XVisualIDFromVisual(xmp->visual),
			 w->core.colormap, xmp->min_colors, xmp->max_colors,
			 xmp->share))
      return 1;
  };
/*
 * Record what kind of colors were actually allocated.
 */
  xmp->share = xmp->pgx->color->readonly;
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
char *xmp_device_name(Widget widget)
{
  if(!widget || XtClass(widget) != xmPgplotWidgetClass) {
    fprintf(stderr, "xmp_device_name: Not a Motif PGPLOT widget.\n");
    return "/null";
  };
  return ((XmPgplotWidget) widget)->pgplot.device;
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
int xmp_device_id(Widget widget)
{
  if(!widget || XtClass(widget) != xmPgplotWidgetClass) {
    fprintf(stderr, "xmp_device_id: Not a Motif PGPLOT widget.\n");
    return 0;
  } else {
    XmPgplotWidget w = (XmPgplotWidget) widget;
    if(w->pgplot.pgslct_id <= 0) {
      fprintf(stderr,
      "xmp_device_id: The specified widget is not currently open to PGPLOT.\n");
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
Widget XmCreatePgplot(Widget parent, char *name, ArgList arglist,
		      Cardinal argcount)
{
  return XtCreateWidget(name, xmPgplotWidgetClass, parent, arglist, argcount);
}

/*.......................................................................
 * The following is a convenience none-variadic function for creating
 * a PGPLOT widget with scroll-bars. Note that XtManageChild() should
 * be applied to the returned widget if you want the PGPLOT window to
 * fit exactly within the ScrollBar widget area.
 *
 * Input:
 *  parent     Widget    The parent widget to adopt.
 *  name         char *  The name to give the widget.
 *  arglist   ArgList    A list of X resources.
 *  argcount Cardinal    The number of X resources.
 * Output:
 *  return     Widget    The new PGPLOT widget.
 */
Widget XmCreateScrolledPgplot(Widget parent, char *name, ArgList arglist,
		      Cardinal argcount)
{
  char *scroll_name;     /* The name to give the scroll-bar widget */
  ArgList scroll_args;   /* The scroll-bar resource argument list */
  Cardinal scroll_count; /* The number of defined entries in scroll_args */
  Widget scroll_w;       /* The ScrollBar widget */
  Widget pgplot_w;       /* The PGPLOT widget */
  int i;
/*
 * We need to create a name for the scrolled window. Follow the
 * convention used by the Motif Text Widget of appending "SW" to the
 * existing name.
 */
  scroll_name = (char *) XtMalloc(sizeof(char) * strlen(name) + 3);
  sprintf(scroll_name, "%sSW", name);
/*
 * We need to construct a resource assignment list for the
 * ScrollBar widget. This is done by copying the input argument list
 * and appending some standard ScrollBar resources.
 */
  scroll_args = (ArgList) XtMalloc(sizeof(Arg) * (argcount + 4));
/*
 * Copy the input argument list.
 */
  scroll_count = 0;
  for(i=0; i<argcount; i++) {
    scroll_args[scroll_count].name = arglist[i].name;
    scroll_args[scroll_count].value = arglist[i].value;
    scroll_count++;
  };
/*
 * Append ScrollBar specific arguments.
 */
  XtSetArg (scroll_args[scroll_count], XmNscrollingPolicy,
	    (XtArgVal )XmAPPLICATION_DEFINED); scroll_count++;
  XtSetArg (scroll_args[scroll_count], XmNvisualPolicy,
	    (XtArgVal )XmVARIABLE); scroll_count++;
  XtSetArg (scroll_args[scroll_count], XmNscrollBarDisplayPolicy,
	    (XtArgVal )XmSTATIC); scroll_count++;
  XtSetArg (scroll_args[scroll_count], XmNshadowThickness, (XtArgVal ) 0); scroll_count++;
/*
 * Create the ScrollBar widget.
 */
  scroll_w = XtCreateManagedWidget(scroll_name, xmScrolledWindowWidgetClass,
				   parent, scroll_args, scroll_count);
/*
 * Reclaim redundant resources.
 */
  XtFree((char *) scroll_name);
  XtFree((char *) scroll_args);
/*
 * Create the PGPLOT widget with the ScrollBar widget as its parent.
 */
  pgplot_w = XtCreateWidget(name, xmPgplotWidgetClass, scroll_w, arglist,
			    argcount);
/*
 * Arrange that when the PGPLOT widget gets destroyed, so does its
 * parent ScrollBar widget.
 */
  XtAddCallback(pgplot_w, XmNdestroyCallback, XmpDestroyParentCallback, NULL);
/*
 * Finished. Return the PGPLOT widget (not the ScrollBar widget).
 */
  return pgplot_w;
}

/*.......................................................................
 * This is the callback used to destroy the ScrollBar parent of a widget.
 */
static void XmpDestroyParentCallback(Widget w, XtPointer client_data, XtPointer call_data)
{
  if(w)
    XtDestroyWidget(XtParent(w));
}

/*.......................................................................
 * Update the clip-area of the window to prevent pgxwin functions from
 * drawing over the highlight-borders.
 *
 * Input:
 *  w     XmPgplotWidget   The pgplot widget instance.
 */
static void xmp_update_clip(XmPgplotWidget w)
{
  (void) pgx_update_clip(w->pgplot.pgx, 1, w->core.width, w->core.height,
			 w->primitive.highlight_thickness);
}

/*.......................................................................
 * This function is a XtResourceDefaultProc function used to return
 * the default highlight color. It is used to initialize the
 * XmNhighlightColor resource.
 */
static void xmp_GetDefaultHighlightColor(Widget widget, int offset,
					XrmValue *value)
{
  static Pixel pixel;
  pixel = WhitePixel(XtDisplay(widget),XScreenNumberOfScreen(XtScreen(widget)));
  value->addr = (XtPointer) &pixel;
}
/*.......................................................................
 * This function is a XtResourceDefaultProc function used to return
 * the default background color. It is used to initialize the
 * XmNbackground resource.
 */
static void xmp_GetDefaultBackgroundColor(Widget widget, int offset,
					  XrmValue *value)
{
  static Pixel pixel;
  pixel = BlackPixel(XtDisplay(widget),XScreenNumberOfScreen(XtScreen(widget)));
  value->addr = (XtPointer) &pixel;
}
/*.......................................................................
 * This function is a XtResourceDefaultProc function used to return
 * the default foreground color. It is used to initialize the
 * XmNforeground resource.
 */
static void xmp_GetDefaultForegroundColor(Widget widget, int offset,
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
int xmp_world_to_pixel(Widget widget, float wx, float wy, int *px, int *py)
{
  XmPgplotWidget w = (XmPgplotWidget) widget;
/*
 * Check the arguments.
 */
  if(!widget) {
    fprintf(stderr, "xmp_world_to_pixel: NULL widget.\n");
    return 1;
  };
/*
 * Delegate the conversion to an internal function.
 */
  return xmp_WorldToPixel(w, wx, wy, px, py);
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
int xmp_pixel_to_world(Widget widget, int px, int py, float *wx, float *wy)
{
  XmPgplotWidget w = (XmPgplotWidget) widget;
/*
 * Check the arguments.
 */
  if(!widget) {
    fprintf(stderr, "xmp_pixel_to_world: NULL widget.\n");
    return 1;
  };
/*
 * Delegate the conversion to an internal function.
 */
  return xmp_PixelToWorld(w, px, py, wx, wy);
}

/*.......................................................................
 * This is an internal function for converting from X-window pixel
 * coordinates to PGPLOT world coordinates.
 *
 * Input:
 *  w  XmPgplotWidget   The widget whose coordinates are to be converted.
 *  px, py        int * The X-window pixel coordinates to be converted.
 * Output:
 *  wx, wy      float * On output, *wx and *wy will be assigned with the
 *                      PGPLOT world coordinates that correspond to px,py.
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int xmp_PixelToWorld(XmPgplotWidget w, int px, int py,
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
 *  w  XmPgplotWidget   The widget whose coordinates are to be converted.
 *  wx, wy      float   The PGPLOT world coordinates to be converted.
 * Output:
 *  px, py       int *  On output, *px and *py will be assigned with the
 *                      X-window pixel coordinates that correspond to wx,wy.
 *  return        int   0 - OK.
 *                      1 - Error.
 */
static int xmp_WorldToPixel(XmPgplotWidget w, float wx, float wy,
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


