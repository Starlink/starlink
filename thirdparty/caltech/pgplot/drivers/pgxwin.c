#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#ifndef convex
#include <string.h>
#endif

/* X-Window include files */

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#include "pgxwin.h"

#define PGX_IDENT "pgxwin"
#define PGX_IMAGE_LEN 1280  /* Length of the line-of-pixels buffer */
#define PGX_COLORMULT 65535 /* Normalized color intensity multiplier */
#define PGX_NCOLORS 16      /* Number of pre-defined PGPLOT colors */
#define PGX_QUERY_MAX 512   /* Max colormap query size in pgx_readonly_colors() */

/* A container used to record the geometry of the Pixmap */

typedef struct {
  float xpix_per_inch; /* Number of pixels per inch along X */
  float ypix_per_inch; /* Number of pixels per inch along Y */
  unsigned int width;  /* Width of window (pixels) */
  unsigned int height; /* Height of window (pixels) */
  int xmargin;         /* X-axis 1/4" margin in pixels */
  int ymargin;         /* Y-axis 1/4" margin in pixels */
  int xmin,xmax;       /* Min/max X-axis pixels excluding 1/4" margins */
  int ymin,ymax;       /* Min/max X-axis pixels excluding 1/4" margins */
} XWgeom;

/*
 * Declare a colormap update descriptor.
 * This keeps a record of the number of buffered colormap updates
 * that need to be sent to the display. This allows color-representations
 * from multiple consecutive calls to pgscr() and pgshls() to be cached.
 */
typedef struct {
  int nbuff;         /* The number of buffered color representation updates */
  int sbuff;         /* The index of the first buffered color representation */
} XWcolor;

/*
 * Declare a polygon descriptor.
 */
typedef struct {
  XPoint *points;  /* Temporary array of polygon vertexes */
  int npoint;      /* Number of points in polygon */
  int ndone;       /* The number of points received so far */
} XWpoly;

/*
 * Declare a container used to record the extent of the rectangular
 * pixmap area that has been modified since the last pgx_flush().
 */
typedef struct {
  int modified;    /* True if 'pixmap' has been modified since last update */
  int xmin,xmax;   /* X-axis extent of modified region (pixels) */
  int ymin,ymax;   /* Y-axis extent of modified region (pixels) */
} XWupdate;

/*
 * Declare a cursor state container.
 */
typedef struct {
  int drawn;             /* True when the cursor is drawn */
  GC gc;                 /* The graphical context of the cursor-band lines */
  int warp;              /* True to warp the cursor on first window entry */
  int type;              /* The cursor banding type PGX_..._CURSOR */
  XPoint vbeg, vend;     /* Start and end vertices of band cursors */
} XWcursor;

/*
 * Declare a world-coordinate coordinate conversion object.
 */
typedef struct {
  float xoff, xdiv;               /* world_x = (device_x - xoff) / xdiv */
  float yoff, ydiv;               /* world_y = (device_y - yoff) / ydiv */
} XWworld;

/*
 * Declare a container to encapsulate the buffers needed to
 * draw a line of pixels.
 */
typedef struct {
  XImage *xi;          /* Line of pixels Xlib image object */
} XWimage;

/*
 * Declare a function type, instances of which are to be called to flush
 * buffered opcodes, and return 0 if OK, or 1 on error.
 */
typedef int (*Flush_Opcode_fn) ARGS((PgxWin *));

/*
 * The following container is used to retain state information for /xw
 * connections.
 */
struct PgxState {
  XWgeom geom;       /* Pixmap geometry */
  XWcolor color;     /* Colormap state descriptor */
  XWpoly poly;       /* Polygon-fill accumulation descriptor */
  XWupdate update;   /* Descriptor of un-drawn area of pixmap */
  XWcursor cursor;   /* Cursor state context descriptor */
  XWworld world;     /* World-coordinate conversion descriptor */
  XWimage image;     /* Line of pixels container */
  XGCValues gcv;     /* Publicly visible contents of 'gc' */
  GC gc;             /* Graphical context descriptor */
  int last_opcode;   /* Index of last opcode */
  Flush_Opcode_fn flush_opcode_fn; /* Function to flush a buffered opcode */
};

static int pgx_error_handler ARGS((Display *display, XErrorEvent *event));
static PgxColor *pgx_find_visual ARGS((PgxWin *pgx, int class, int min_col, \
				 int max_col, int readonly));
static int pgx_get_colorcells ARGS((PgxWin *pgx, PgxColor *color, \
			      int min_col, int max_col));
static int pgx_get_mutable_colorcells ARGS((PgxWin *pgx, PgxColor *color, \
			      int min_col, int max_col));

static XVisualInfo *pgx_visual_info ARGS((Display *display, int screen, \
					  VisualID vid));
static int pgx_parse_visual ARGS((char *str));
static void pgx_xy_to_XPoint ARGS((PgxWin *pgx, float *xy, XPoint *xp));
static void pgx_XPoint_to_xy ARGS((PgxWin *pgx, XPoint *xp, float *xy));
static void pgx_mark_modified ARGS((PgxWin *pgx, int x, int y, int diameter));
static int pgx_init_colors ARGS((PgxWin *pgx));
static int pgx_update_colors ARGS((PgxWin *pgx));
static int pgx_flush_colors ARGS((PgxWin *pgx, int ci_start, int ncol));
static int pgx_restore_line ARGS((PgxWin *pgx, int xa, int ya, int xb, int yb));
static int pgx_handle_cursor ARGS((PgxWin *pgx, float *rbuf, char *key));
static void pgx_limit_pcoords ARGS((PgxWin *pgx, XPoint *coord));
static void pgx_limit_wcoords ARGS((PgxWin *pgx, XPoint *coord));
static void pgx_window_to_pixmap ARGS((PgxWin *pgx, XPoint *w_coord, \
				    XPoint *p_coord));
static void pgx_pixmap_to_window ARGS((PgxWin *pgx, XPoint *p_coord, \
				    XPoint *w_coord));
static int pgx_copy_area ARGS((PgxWin *pgx, int px, int py, unsigned w, \
			       unsigned h, int wx, int wy));
static int pgx_clear_area ARGS((PgxWin *pgx, int x, int y, unsigned w,
				unsigned h));
PgxColor *new_PgxColor ARGS((PgxWin *pgx, int max_col, int readonly, \
			     VisualID vid));
static PgxColor *del_PgxColor ARGS((PgxWin *pgx, PgxColor *color));
static int pgx_default_class ARGS((PgxWin *pgx));
static int pgx_nint ARGS((float f));

static int pgx_readonly_colors ARGS((PgxWin *pgx, int ncol, XColor *colors, \
				     unsigned long *pixels));
static int pgx_cmp_xcolor ARGS((const void *va, const void *vb));
static int pgx_nearest_color ARGS((XColor *colors, int ncol, XColor *c));

/*
 * pgx_ready() accepts a state bitmask that contains a union of the
 * following enumerated resource requirement bits.
 */
#define PGX_NEED_COLOR  1  /* Colormap allocated */
#define PGX_NEED_WINDOW 2  /* Window created */
#define PGX_NEED_PIXMAP 4  /* A valid pixmap exists */
#define PGX_NEED_PGOPEN 8  /* Open to PGPLOT */

static int pgx_ready ARGS((PgxWin *pgx, int state));

/*.......................................................................
 * This function should be called to instantiate PgxWin::state when
 * an existing device is opened with pgbeg() or pgopen(). Note that all
 * but the pixmap and state members of the passed PgxWin structure must have
 * been instantiated. The pixmap member may either be instantiated or
 * be initialized with the null-atom constant: None. The state member
 * must be NULL.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context to connect to.
 * Output:
 *  return PgxState *  The PGPLOT drawing-state descriptor, or NULL
 *                     on error.
 */
#ifdef __STDC__
PgxState *pgx_open(PgxWin *pgx)
#else
PgxState *pgx_open(pgx)
     PgxWin *pgx;
#endif
{
  PgxState *state;  /* The new state descriptor */
/*
 * Check the validity of the PgxWin structure.
 */
  if(!pgx || !pgx->display || pgx->window==None || !pgx->expose_gc ||
     pgx->bad_device || !pgx->name || !pgx->color) {
    fprintf(stderr, "pgx_open: Bad PgxWin descriptor.\n");
    return NULL;
  };
  if(pgx->state) {
    fprintf(stderr, "pgx_open: The specified device is already open.\n");
    return NULL;
  };
/*
 * Allocate the state container.
 */
  state = (PgxState *) malloc(sizeof(PgxState));
  if(!state) {
    fprintf(stderr, "pgx_open: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize
 * the container at least up to the point at which it is
 * safe to pass it to pgx_close().
 */
  pgx->state = state;
  state->geom.xpix_per_inch = 0.0;
  state->geom.ypix_per_inch = 0.0;
  state->geom.width = 0;
  state->geom.height = 0;
  state->geom.xmargin = 0;
  state->geom.ymargin = 0;
  state->geom.xmin = 0;
  state->geom.xmax = 0;
  state->geom.ymin = 0;
  state->geom.ymax = 0;
  state->color.nbuff = 0;
  state->color.sbuff = 0;
  state->poly.points = NULL;
  state->poly.npoint = 0;
  state->poly.ndone = 0;
  state->update.modified = 0;
  state->update.xmin = 0;
  state->update.xmax = 0;
  state->update.ymin = 0;
  state->update.ymax = 0;
  state->cursor.drawn = 0;
  state->cursor.gc = NULL;
  state->cursor.type = PGX_NORM_CURSOR;
  state->world.xoff = 0.0;
  state->world.yoff = 0.0;
  state->world.xdiv = 1.0;
  state->world.ydiv = 1.0;
  state->image.xi = NULL;
  state->gc = NULL;
  state->last_opcode = 0;
  state->flush_opcode_fn = 0;
/*
 * Create and initialize a graphical context descriptor. This is where
 * Line widths, line styles, fill styles, plot color etc.. are
 * recorded.
 */
  state->gcv.line_width = 1;
  state->gcv.cap_style = CapRound;
  state->gcv.join_style = JoinRound;
  state->gcv.fill_rule = EvenOddRule;
  state->gcv.graphics_exposures = False;
  state->gcv.foreground = WhitePixel(pgx->display, pgx->screen);
/*
 * Bracket the actual creation with pgx_start/end_error() calls, to
 * determine whether any allocation errors occur.
 */
  pgx_start_error_watch(pgx);
  state->gc = XCreateGC(pgx->display, pgx->window,
			(unsigned long) (GCLineWidth | GCCapStyle |
					 GCJoinStyle | GCFillRule |
					 GCGraphicsExposures | GCForeground),
			&state->gcv);
  if(pgx_end_error_watch(pgx) || !state->gc) {
    fprintf(stderr, "%s: Failed to allocate graphical context.\n", PGX_IDENT);
    return pgx_close(pgx);
  };
/*
 * Create the X image that we use to compose lines of pixels with given
 * colors.
 */
  pgx_start_error_watch(pgx);
  state->image.xi = XCreateImage(pgx->display, pgx->color->vi->visual,
				 (unsigned)pgx->color->vi->depth, ZPixmap, 0,
				 NULL, (unsigned)PGX_IMAGE_LEN, 1, 32, 0);
  if(pgx_end_error_watch(pgx) || !state->image.xi) {
    fprintf(stderr, "%s: Failed to allocate XImage descriptor.\n", PGX_IDENT);
    return pgx_close(pgx);
  };
/*
 * Allocate the image buffer.
 */
  state->image.xi->data = malloc((size_t) state->image.xi->bytes_per_line);
  if(!state->image.xi->data) {
    fprintf(stderr, "%s: Failed to allocate image buffer.\n", PGX_IDENT);
    return pgx_close(pgx);
  };
/*
 * Create the cursor graphical context.
 */
  pgx_start_error_watch(pgx);
  {
    XGCValues gcv;
    gcv.line_width = 0;
    gcv.graphics_exposures = False;
    gcv.foreground = WhitePixel(pgx->display, pgx->screen);
    state->cursor.gc = XCreateGC(pgx->display, pgx->window,
			(unsigned long) (GCLineWidth | GCGraphicsExposures |
					 GCForeground), &gcv);
  };
  if(pgx_end_error_watch(pgx) || !state->cursor.gc) {
    fprintf(stderr, "%s: Failed to allocate graphical context.\n", PGX_IDENT);
    return pgx_close(pgx);
  };
/*
 * Initialize attributes.
 */
  pgx_init_colors(pgx);
  pgx_set_ci(pgx, 1);
  pgx_set_lw(pgx, 1);
  pgx_set_cursor(pgx, 0, PGX_NORM_CURSOR, 0, NULL, NULL);
  return state;
}

/*.......................................................................
 * This function should be called to delete a pgx->state PGPLOT
 * drawing-state descriptor when pgend() is called. pgx->state will
 * also be assigned NULL.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context to disconnect.
 * Output:
 *  return PgxState *  The deleted PGPLOT drawing-state descriptor.
 *                     Always NULL.
 */
#ifdef __STDC__
PgxState *pgx_close(PgxWin *pgx)
#else
PgxState *pgx_close(pgx)
     PgxWin *pgx;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN)) {
    PgxState *state = pgx->state;
/*
 * Delete the graphical context descriptor.
 */
    if(state->gc)
      XFreeGC(pgx->display, state->gc);
    state->gc = NULL;
/*
 * Delete the image descriptor.
 */
    if(state->image.xi)
      XDestroyImage(state->image.xi);
    state->image.xi = NULL;
/*
 * Check for un-freed polygon points.
 */
    if(state->poly.points)
      free((char *)state->poly.points);
    state->poly.points = NULL;
/*
 * Delete the cursor graphical context descriptor.
 */
    if(state->cursor.gc)
      XFreeGC(pgx->display, state->cursor.gc);
    state->cursor.gc = NULL;
/*
 * Delete the container.
 */
    free(state);
    pgx->state = NULL;
  };
  return NULL;
}

/*.......................................................................
 * This function must be called before each opcode is handled by the
 * device-specific driver dispatch function.
 *
 * Input:
 *  pgx      PgxWin *    The PGPLOT window context.
 *  opcode      int      The new opcode.
 * Output:
 *  return      int      0 - OK.
 *                       1 - Error.
 */
#ifdef __STDC__
int pgx_pre_opcode(PgxWin *pgx, int opcode)
#else
int pgx_pre_opcode(pgx, opcode)
     PgxWin *pgx; int opcode;
#endif
{
/*
 * If there is a buffered opcode and the latest opcode is not the same
 * as the last opcode, call the given flush function for the
 * buffered opcode.
 */
  if(pgx_ready(pgx, PGX_NEED_PGOPEN)) {
    PgxState *state = pgx->state;
    if(state->last_opcode != opcode) {
      if(state->flush_opcode_fn != (Flush_Opcode_fn) 0) {
	(*state->flush_opcode_fn)(pgx);
	state->flush_opcode_fn = (Flush_Opcode_fn) 0;
      };
/*
 * Record the current opcode for next time.
 */
      state->last_opcode = opcode;
    };
  };
  return 0;
}

/*.......................................................................
 * This function returns non-zero if the specified descriptor is not
 * NULL, not marked as bad via pgx->bad_device, and has all of the
 * specified resources.
 *
 * Input:
 *  pgx     PgxWin *  The device descriptor to be checked.
 *  state      int    A bitmask of resources that are required.
 *                     PGX_NEED_COLOR  - Colormap allocated.
 *                     PGX_NEED_WINDOW - Window created.
 *                     PGX_NEED_PIXMAP - A valid pixmap exists.
 *                     PGX_NEED_PGOPEN - Open to PGPLOT. Note that
 *                         this implies that a colormap and window
 *                         exist but not that a pixmap exists.
 * Output:
 *  return     int    1 - Descriptor OK.
 *                    0 - Error - don't use pgx.
 */
#ifdef __STDC__
static int pgx_ready(PgxWin *pgx, int state)
#else
static int pgx_ready(pgx, state)
     PgxWin *pgx; int state;
#endif
{
  if(!pgx || pgx->bad_device)
    return 0;
  if(state & PGX_NEED_COLOR && !pgx->color)
    return 0;
  if(state & PGX_NEED_WINDOW && pgx->window == None)
    return 0;
  if(state & PGX_NEED_PIXMAP && pgx->pixmap == None)
    return 0;
  if(state & PGX_NEED_PGOPEN && !pgx->state)
    return 0;
  return 1;
}

/*.......................................................................
 * Call this function when an Expose event is received. It will then
 * re-draw the exposed region from the pgx->pixmap, taking acount of any
 * scroll offsets in pgx->scroll. The device need not be open to PGPLOT.
 *
 * Input:
 *  pgx     PgxWin *  The PGPLOT window context.
 *  event   XEvent *  The expose event.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
#ifdef __STDC__
int pgx_expose(PgxWin *pgx, XEvent *event)
#else
int pgx_expose(pgx, event)
     PgxWin *pgx; XEvent *event;
#endif
{
/*
 * Device error?
 */
  if(pgx_ready(pgx, PGX_NEED_PIXMAP | PGX_NEED_WINDOW) && event->type==Expose) {
    pgx_copy_area(pgx, (int)(event->xexpose.x + pgx->scroll.x),
		  (int)(event->xexpose.y + pgx->scroll.y),
		  (unsigned) event->xexpose.width,
		  (unsigned) event->xexpose.height,
		  event->xexpose.x, event->xexpose.y);
/*
 * Re-draw the possibly damaged cursor augmentation.
 */
    pgx_refresh_cursor(pgx);
/*
 * Ensure that the window responds immediately to the update.
 */
    XFlush(pgx->display);
    if(pgx->bad_device)
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Scroll the pixmap within the window area.
 *
 * Input:
 *  pgx     PgxWin *  The PGPLOT window context.
 *  x,y   unsigned    The position of the top left corner of the window
 *                    within the pixmap. This allows for clients that
 *                    want to scroll the pixmap within the window area.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
#ifdef __STDC__
int pgx_scroll(PgxWin *pgx, unsigned x, unsigned y)
#else
int pgx_scroll(pgx, x, y)
     PgxWin *pgx; unsigned x; unsigned y;
#endif
{
  if(pgx_ready(pgx, 0)) {
    XEvent event;
/*
 * Record the new scroll and pan values.
 */
    pgx->scroll.x = x;
    pgx->scroll.y = y;
/*
 * Redraw the scrolled contents of the window.
 */
    if(pgx_ready(pgx, PGX_NEED_WINDOW | PGX_NEED_PIXMAP)) {
      XPoint brc;  /* Bottom right corner of pixmap */
/*
 * We need to know the size of the window and the size of the pixmap.
 */
      XWindowAttributes attr;
      XGetWindowAttributes(pgx->display, pgx->window, &attr);
      if(pgx->bad_device)
	return 1;
/*
 * Record the bottom-right-corner pixmap coordinate in brc.
 */
      {
	Window root;
	int x_root, y_root;
	unsigned width, height, border, depth;
	XGetGeometry(pgx->display, pgx->pixmap, &root, &x_root, &y_root,
		     &width, &height, &border, &depth);
	brc.x = width - 1;
	brc.y = height - 1;
      };
/*
 * Determine the scrolled window coordinate at which the bottom right
 * corner of the pixmap lies.
 */
      pgx_pixmap_to_window(pgx, &brc, &brc);
/*
 * Clear the parts of the window that will not be covered by the pixmap.
 * Given that scroll.x and scroll.y are unsigned this can only be
 * areas to the left and bottom of the drawn area.
 */

      if(brc.x < attr.width) {
	pgx_clear_area(pgx, (brc.x + 1), 0,
		   (unsigned) (attr.width - brc.x - 1),
		   (unsigned) (attr.height));
      };
      if(brc.y < attr.height) {
	pgx_clear_area(pgx, 0, (brc.y + 1),
		   (unsigned)(attr.width),
		   (unsigned)(attr.height - brc.y - 1));
      };
/*
 * Set up a fake expose event to have the new pixmap area drawn.
 */
      event.type = Expose;
      event.xexpose.x = 0;
      event.xexpose.y = 0;
      event.xexpose.width = brc.x + 1;
      event.xexpose.height = brc.y + 1;
      return pgx_expose(pgx, &event);
    };
  };
  return 1;
}

/*.......................................................................
 * Update the recorded extent of the drawable area of the window.
 * This must be called whenever the window is resized.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context.
 *  doclip      int    0 - Disable clipping entirely.
 *                     1 - Clip all graphics outside the specified region.
 *  width  unsigned    The width of the window.
 *  height unsigned    The height of the window.
 *  border unsigned    The width of the window border.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
#ifdef __STDC__
int pgx_update_clip(PgxWin *pgx, int doclip, unsigned width, unsigned height,
		    unsigned border)
#else
int pgx_update_clip(pgx, doclip, width, height, border)
     PgxWin *pgx; int doclip; unsigned width; unsigned height; unsigned border;
#endif
{
  if(pgx_ready(pgx, 0)) {
    pgx->clip.doclip = doclip;
    pgx->clip.xmin = border;
    pgx->clip.ymin = border;
    pgx->clip.xmax = width - border - 1;
    pgx->clip.ymax = height - border - 1;
    return 0;
  };
  return 1;
}

/*.......................................................................
 * Update the dimensions of the optional X and Y axis margins. The new
 * dimensions will be ignored until the start of the next page.
 *
 * Margins are blank areas left around the plottable viewsurface, purely
 * for esthetic reasons.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context.
 *  xmargin     int    The number of pixels to leave either side of
 *                     the plot surface.
 *  ymargin     int    The number of pixels to leave above and below
 *                     the plot suface.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
#ifdef __STDC__
int pgx_set_margin(PgxWin *pgx, int xmargin, int ymargin)
#else
int pgx_set_margin(pgx, xmargin, ymargin)
     PgxWin *pgx; int xmargin; int ymargin;
#endif
{
  if(pgx_ready(pgx, 0)) {
    pgx->xmargin = xmargin > 0 ? xmargin : 0;
    pgx->ymargin = ymargin > 0 ? ymargin : 0;
    return 0;
  };
  return 1;
}

/*.......................................................................
 * Install a temporary error handler for use in detecting resource
 * allocation errors. The error handler will maintain a count of
 * errors, which will be returned by the matching function
 * pgx_end_error_watch(). These functions are intended to be used to
 * bracket resource allocation functions, in order to allow resource
 * allocation failures to be detected.
 *
 * Input:
 *  pgx   PgxWin *  The PGPLOT window context descriptor.
 */
#ifdef __STDC__
void pgx_start_error_watch(PgxWin *pgx)
#else
void pgx_start_error_watch(pgx)
     PgxWin *pgx;
#endif
{
  if(pgx_ready(pgx, 0)) {
/*
 * Force all errors from previous events to be flushed.
 */
    XSync(pgx->display, False);
/*
 * Clear the error-handler internal error count.
 */
    pgx_error_handler(pgx->display, (XErrorEvent *) 0);
/*
 * Install the error handler.
 */
    pgx->old_handler = XSetErrorHandler(pgx_error_handler);
  };
  return;
}

/*.......................................................................
 * This function is paired with pgx_start_error_watch() and and after
 * ensuring that all errors have been trapped by calling XSync(), it
 * returns the error count recorded by the error handler that said function
 * installed. It then removes the error handler.
 *
 * Input:
 *  pgx     PgxWin *  The PGPLOT window context descriptor.
 * Output:
 *  return     int    The number of errors that were counted.
 */
#ifdef __STDC__
int pgx_end_error_watch(PgxWin *pgx)
#else
int pgx_end_error_watch(pgx)
     PgxWin *pgx;
#endif
{
  if(pgx_ready(pgx, 0)) {
/*
 * Ensure that all error-events have delivered.
 */
    XSync(pgx->display, False);
/*
 * De-install the error handler and re-instate the one that it displaced.
 */
    XSetErrorHandler(pgx->old_handler);
    pgx->old_handler = 0;
/*
 * Return the current error-count.
 */
    return pgx_error_handler(pgx->display, (XErrorEvent *) 0);
  };
  return 0;
}

/*.......................................................................
 * This function is called by X whenever a non-fatal error occurs
 * on a given display connection. For the moment it does nothing but
 * count such errors in an internal static error counter. This counter
 * can then be queried and reset by sending a NULL error event pointer.
 *
 * Input:
 *  display    Display *  The display connection on which the error occured.
 *  event  XErrorEvent *  The descriptor of the error event, or NULL to
 *                        request that the error counter be queried and reset.
 * Output:
 *  return         int    The return value is not specified by Xlib, so
 *                        for Xlib calls we will simply return 0. For
 *                        none Xlib calls (distinguishable by sending
 *                        event==NULL), the value of the error counter
 *                        is returned.
 */
#ifdef __STDC__
static int pgx_error_handler(Display *display, XErrorEvent *event)
#else
static int pgx_error_handler(display, event)
     Display *display; XErrorEvent *event;
#endif
{
  static int error_count = 0;
/*
 * To query and reset the error counter, this program calls pgx_error_handler()
 * with a NULL error event pointer. This distinguishes it from a call
 * from Xlib.
 */
  if(!event) {
    int ret_count = error_count;
    error_count = 0;   /* Reset the error counter */
    return ret_count;  /* Return the pre-reset value of the error counter */
#ifdef DEBUG
  } else {
    char errtxt[81]; /* Buffer to receive error message in */
/*
 * Get a message describing the error.
 */
    XGetErrorText(display, (int)event->error_code, errtxt, (int)sizeof(errtxt));
    fprintf(stderr, "%s: XErrorEvent: %s\n", ERROR_PREFIX, errtxt);
/*
 * Report the operation that caused it. These opcode numbers are listed in
 * <X11/Xproto.h>.
 */
    fprintf(stderr, "%s: Major opcode: %d, Resource ID: 0x%lx%s.\n",
	 ERROR_PREFIX, (int) event->request_code,
	 (unsigned long) event->resourceid,
         (event->resourceid==DefaultRootWindow(display)?" (Root window)":""));
#endif
  };
/*
 * Keep a record of the number of errors that have occurred since the
 * error counter was last cleared.
 */
  error_count++;
  return 0;
}

/*.......................................................................
 * Allocate a visual/colormap context and initialize it with defaults.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context.
 *  max_col     int    The maximum number of colors to allow for.
 *  readonly    int    True if readonly colors are sufficient.
 *  vid    VisualID    The ID of the visual target visual.
 * Output:
 *  return PgxColor *  The new context descriptor, or NULL on error.
 */
#ifdef __STDC__
PgxColor *new_PgxColor(PgxWin *pgx, int max_col, int readonly, VisualID vid)
#else
PgxColor *new_PgxColor(pgx, max_col, readonly, vid)
     PgxWin *pgx; int max_col; int readonly; VisualID vid;
#endif
{
  PgxColor *color;  /* The new context descriptor */
  if(!pgx_ready(pgx, 0))
    return NULL;
/*
 * Allocate the context descriptor.
 */
  color = (PgxColor *) malloc(sizeof(PgxColor));
  if(!color) {
    fprintf(stderr, "%s: (new_PgxColor) Insufficient memory.\n", PGX_IDENT);
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * color descriptor at least up to the point at which it can safely be
 * passed to del_PgxColor().
 */
  color->vi = NULL;
  color->cmap = None;
  color->private = 0;
  color->ncol = 2;
  color->monochrome = 1;
  color->pixel = NULL;
  color->npixel = 0;
  color->xcolor = NULL;
  color->initialized = 0;
  color->default_class = 0;
  color->readonly = readonly;
  color->nwork = 0;
  color->work = NULL;
/*
 * Get the visual information object.
 */
  color->vi = pgx_visual_info(pgx->display, pgx->screen, vid);
  if(!color->vi)
    return del_PgxColor(pgx, color);
/*
 * See what type of color allocation will be needed.
 */
  switch(color->vi->class) {
  case PseudoColor:
  case GrayScale:
    color->readonly = readonly;
    break;
  case DirectColor:
    color->readonly = 0;
    break;
  case StaticColor:
  case TrueColor:
  case StaticGray:
    color->readonly = 1;
    break;
  default:
    fprintf(stderr, "%s: Unknown colormap type.\n", PGX_IDENT);
    return del_PgxColor(pgx, color);
    break;
  };
/*
 * We can only handle between 2 and 256 colors.
 */
  if(max_col < 2)
    max_col = 2;
  else if(max_col > 256)
    max_col = 256;
/*
 * Determine the class of the default visual.
 */
  color->default_class = pgx_default_class(pgx);
/*
 * Allocate an array to store pixel indexes in.
 */
  color->pixel = (unsigned long *) malloc(sizeof(unsigned long) * max_col);
  if(color->pixel==NULL) {
    fprintf(stderr, "%s: Insufficient memory for new PGPLOT window.\n",
	    PGX_IDENT);
    return del_PgxColor(pgx, color);
  };
/*
 * Allocate an array to store color representations in.
 */
  color->xcolor = (XColor *) malloc(sizeof(XColor) * max_col);
  if(!color->xcolor) {
    fprintf(stderr, "%s: Insufficient memory for new PGPLOT window.\n",
	    PGX_IDENT);
    return del_PgxColor(pgx, color);
  };
/*
 * Allocate a work array for use by pgx_readonly_colors().
 */
  if(readonly) {
    color->nwork = color->vi->colormap_size;
    if(color->nwork > PGX_QUERY_MAX)
      color->nwork = PGX_QUERY_MAX;
    color->work = (XColor *) malloc(sizeof(XColor) * color->nwork);
    if(!color->work) {
      fprintf(stderr, "%s: Insufficient memory for shared-color allocation.\n",
	      PGX_IDENT);
      return del_PgxColor(pgx, color);
    };
  };
/*
 * Leave the rest of the initialization to specific functions.
 */
  return color;
}

/*.......................................................................
 * Delete a color/visual context descriptor.
 *
 * Input:
 *  pgx       PgxWin *  The PGPLOT window context.
 *  color   PgxColor *  The descriptor to be deleted.
 * Output:
 *  return  PgxColor *  The deleted descriptor (always NULL).
 */
#ifdef __STDC__
static PgxColor *del_PgxColor(PgxWin *pgx, PgxColor *color)
#else
static PgxColor *del_PgxColor(pgx, color)
     PgxWin *pgx; PgxColor *color;
#endif
{
  if(color) {
    if(color->vi) {
      if(color->cmap) {
/*
 * Release allocated colorcells.
 */
	if(pgx->display && color->pixel && color->npixel > 0) {
	  XFreeColors(pgx->display, color->cmap, color->pixel,
		      color->npixel, (unsigned long)0);
	};
      };
/*
 * Delete the colormap only if we allocated it.
 */
      if(pgx->display && color->private &&
	 color->cmap != DefaultColormap(pgx->display, pgx->screen))
	XFreeColormap(pgx->display, color->cmap);
/*
 * Delete the visual information descriptor.
 */
      XFree((char *) color->vi);
    };
/*
 * Delete the pixel and color representation arrays.
 */
    if(color->pixel)
      free(color->pixel);
    if(color->xcolor)
      free(color->xcolor);
    if(color->work)
      free(color->work);
/*
 * Delete the container.
 */
    free(color);
  };
  return NULL;
}

/*.......................................................................
 * Return the colormap class of the default visual.
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context descriptor.
 * Output:
 *  return    int    The colormap class.
 */
#ifdef __STDC__
static int pgx_default_class(PgxWin *pgx)
#else
static int pgx_default_class(pgx)
     PgxWin *pgx;
#endif
{
  int class;
  XVisualInfo *vi = pgx_visual_info(pgx->display, pgx->screen,
	    XVisualIDFromVisual(DefaultVisual(pgx->display, pgx->screen)));
  if(!vi)
    return PseudoColor;
  class = vi->class;
  XFree((char *) vi);
  return class;
}

/*.......................................................................
 * Search for an appropriate visual for a PGPLOT window and create a
 * colormap for it (unless the default visual is used).
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context (We need the display
 *                     and screen members).
 *  class_name char *  The name of the desired visual class type.
 *  min_col     int    The minimum acceptable number of colors before
 *                     switching to monochrome.
 *  max_col     int    The maximum number of colors to allocate.
 *  readonly    int    If true, try to allocate readonly colors.
 *                     false try to allocate read/write colors where
 *                     available.
 * Output:
 *  return PgxColor *  The color/visual context descriprot, or NULL
 *                     on error.
 */
#ifdef __STDC__
PgxColor *pgx_new_visual(PgxWin *pgx, char *class_name, int min_col,
		   int max_col, int readonly)
#else
PgxColor *pgx_new_visual(pgx, class_name, min_col, max_col, readonly)
     PgxWin *pgx; char *class_name; int min_col; int max_col; int readonly;
#endif
{
  PgxColor *color = NULL;   /* The new visual/colormap context descriptor */
  int default_class;        /* The class of the default visual */
  int visual_class;         /* The desired visual class */
  if(!pgx_ready(pgx, 0))
    return NULL;
/*
 * Limit the number of colors to allowed values.
 */
  if(min_col < 0)
    min_col = 0;
  else if(min_col > 256)
    min_col = 256;
  if(max_col < 0)
    max_col = 0;
  else if(max_col > 256)
    max_col = 256;
/*
 * Determine the class of the default visual.
 */
  default_class = pgx_default_class(pgx);
/*
 * Decode the desired visual-class type.
 */
  visual_class = pgx_parse_visual(class_name);
/*
 * Use a specific visual class?
 */
  if(visual_class >= 0)
    color = pgx_find_visual(pgx, visual_class, min_col, max_col, readonly);
/*
 * Should we perform a search for a suitable visual class?
 */
  if(visual_class == -1 || !color) {
/*
 * Color display?
 */
    switch(default_class) {
    case PseudoColor:
    case StaticColor:
    case DirectColor:
    case TrueColor:
      color = pgx_find_visual(pgx, PseudoColor, min_col, max_col, readonly);
      if(!color)
	color = pgx_find_visual(pgx, StaticColor, min_col, max_col, readonly);
      if(!color)
	color = pgx_find_visual(pgx, TrueColor, min_col, max_col, readonly);
      break;
/*
 * Gray-scale display?
 */
    case GrayScale:
    case StaticGray:
      color = pgx_find_visual(pgx, GrayScale, min_col, max_col, readonly);
      if(!color)
	color = pgx_find_visual(pgx, StaticGray, min_col, max_col, readonly);
      break;
    };
  };
/*
 * If requested, or we failed to acquire the desired visual
 * use black and white colors from the default visual.
 */
  return color ? color : pgx_bw_visual(pgx);
}

/*.......................................................................
 * Return a black and white visual/color context descriptor.
 *
 * Input:
 *  pgx       PgxWin * The PGPLOT window context (We need the display
 *                     and screen members).
 * Output:
 *  return  PgxColor * The context descriptor, or NULL on error.
 */
#ifdef __STDC__
PgxColor *pgx_bw_visual(PgxWin *pgx)
#else
PgxColor *pgx_bw_visual(pgx)
     PgxWin *pgx;
#endif
{
  PgxColor *color;   /* The new descriptor */
  if(!pgx_ready(pgx, 0))
    return NULL;
/*
 * Allocate the context descriptor.
 */
  color = new_PgxColor(pgx, 2, 1,
	       XVisualIDFromVisual(DefaultVisual(pgx->display, pgx->screen)));
  if(!color)
    return NULL;
/*
 * Record the default-colormap ID.
 */
  color->cmap = DefaultColormap(pgx->display, pgx->screen);
/*
 * Record the two available colors.
 */
  color->pixel[0] = BlackPixel(pgx->display, pgx->screen);
  color->pixel[1] = WhitePixel(pgx->display, pgx->screen);
  color->ncol = 2;
/*
 * Record the fact that we are using monochrome.
 */
  color->monochrome = 1;
/*
 * Install the color context.
 */
  pgx->color = color;
/*
 * Initialize the colors.
 */
  if(pgx_init_colors(pgx))
    return pgx_del_visual(pgx);
  return color;
}

/*.......................................................................
 * Return a visual/color context descriptor for colors allocated from
 * the default colormap of the screen.
 *
 * Input:
 *  pgx       PgxWin * The PGPLOT window context (We need the display
 *                     and screen members).
 *  min_col     int    The minimum acceptable number of colors before
 *                     switching to monochrome.
 *  max_col     int    The maximum number of colors to allocate.
 *  readonly    int    If true, try to allocate readonly colors.
 *                     false try to allocate read/write colors where
 *                     available.
 * Output:
 *  return  PgxColor * The context descriptor, or NULL on error.
 */
#ifdef __STDC__
PgxColor *pgx_default_visual(PgxWin *pgx, int min_col, int max_col,int readonly)
#else
PgxColor *pgx_default_visual(pgx, min_col, max_col, readonly)
     PgxWin *pgx; int min_col; int max_col; int readonly;
#endif
{
  PgxColor *color;   /* The new descriptor */
  if(!pgx_ready(pgx, 0))
    return NULL;
/*
 * Limit the number of colors to allowed values.
 */
  if(min_col < 0)
    min_col = 0;
  else if(min_col > 256)
    min_col = 256;
  if(max_col < 0)
    max_col = 0;
  else if(max_col > 256)
    max_col = 256;
/*
 * Allocate the context descriptor.
 */
  color = new_PgxColor(pgx, max_col, readonly,
		XVisualIDFromVisual(DefaultVisual(pgx->display, pgx->screen)));
  if(!color)
    return NULL;
/*
 * Record the default-colormap ID.
 */
  color->cmap = DefaultColormap(pgx->display, pgx->screen);
/*
 * Attempt to allocate colorcells from the default colormap.
 * If this fails return a visual that uses the black and white
 * pixels of the default visual.
 */
  if(pgx_get_colorcells(pgx, color, min_col, max_col)) {
    color = del_PgxColor(pgx, color);
    return pgx_bw_visual(pgx);
  };
/*
 * Record the fact that we haven't reverted to monochrome.
 */
  color->monochrome = 0;
/*
 * Install the color context.
 */
  pgx->color = color;
/*
 * Initialize the colors.
 */
  if(pgx_init_colors(pgx))
    return pgx_del_visual(pgx);
  return color;
}

/*.......................................................................
 * Allocate colors from and return a visual/colormap context descriptor
 * for a specified existing visual and colormap.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context (We need the display
 *                     and screen members).
 *  vid    VisualID    The ID of the visual to be used. (Note that
 *                     XGetVisualIDFromVisual() can be used to get the
 *                     visual ID of a visual from a (Visual *).
 *  cmap   Colormap    The colormap to be used. This must be compatible
 *                     with the specified visual.
 *  min_col     int    The minimum acceptable number of colors before
 *                     switching to monochrome.
 *  max_col     int    The maximum number of colors to allocate.
 *  readonly    int    If true, try to allocate readonly colors. If
 *                     false try to allocate read/write colors where
 *                     available.
 * Output:
 *  return  PgxColor * The context descriptor, or NULL on error.
 */
#ifdef __STDC__
PgxColor *pgx_adopt_visual(PgxWin *pgx, VisualID vid, Colormap cmap,
			   int min_col, int max_col, int readonly)
#else
PgxColor *pgx_adopt_visual(pgx, vid, cmap, min_col, max_col, readonly)
     PgxWin *pgx; VisualID vid; Colormap cmap; int min_col; int max_col;
     int readonly;
#endif
{
  PgxColor *color;   /* The new descriptor */
  if(!pgx_ready(pgx, 0))
    return NULL;
/*
 * Limit the number of colors to allowed values.
 */
  if(min_col < 0)
    min_col = 0;
  else if(min_col > 256)
    min_col = 256;
  if(max_col < 0)
    max_col = 0;
  else if(max_col > 256)
    max_col = 256;
/*
 * Allocate the context descriptor.
 */
  color = new_PgxColor(pgx, max_col, readonly, vid);
  if(!color)
    return NULL;
/*
 * Record the colormap ID.
 */
  color->cmap = cmap;
/*
 * Attempt to allocate colors from the colormap.
 */
  if(pgx_get_colorcells(pgx, color, min_col, max_col))
    return del_PgxColor(pgx, color);
/*
 * Install the color context.
 */
  pgx->color = color;
/*
 * Initialize the colors.
 */
  if(pgx_init_colors(pgx))
    return pgx_del_visual(pgx);
  return color;
}

/*.......................................................................
 * Allocate colors from and return a visual/colormap context descriptor
 * describing the visual and colormap of a specified other window.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context (We need the display
 *                     and screen members).
 *  w        Window    The window to inherit the colormap and visual from.
 *  min_col     int    The minimum acceptable number of colors before
 *                     switching to monochrome.
 *  max_col     int    The maximum number of colors to allocate.
 *  readonly    int    If true, try to allocate readonly colors. If
 *                     false try to allocate read/write colors where
 *                     available.
 * Output:
 *  return  PgxColor * The context descriptor, or NULL on error.
 */
#ifdef __STDC__
PgxColor *pgx_window_visual(PgxWin *pgx, Window w, int min_col, int max_col,
			    int readonly)
#else
PgxColor *pgx_window_visual(pgx, w, min_col, max_col, readonly)
     PgxWin *pgx; Window w; int min_col; int max_col; int readonly;
#endif
{
  XWindowAttributes attr;  /* The attributes of the specified window */
  if(!pgx_ready(pgx, 0))
    return NULL;
/*
 * Acquire the attributes of the specified window.
 */
  if(!XGetWindowAttributes(pgx->display, w, &attr)) {
    fprintf(stderr,
      "%s: (pgx_window_visual) Unable to get attributes of window: 0x%lx.\n",
	    PGX_IDENT, (unsigned long) w);
    return NULL;
  };
/*
 * Install the visual and colormap recorded in the returned attributes.
 */
  return pgx_adopt_visual(pgx, XVisualIDFromVisual(attr.visual), attr.colormap,
			  min_col, max_col, readonly);
}

/*.......................................................................
 * Private function of pgx_new_visual(), used to find a visual of a given
 * class and at least min_col colors, allocate colors and return a
 * visual/colormap context descriptor for it.
 *
 * Input:
 *  pgx      PgxWin * The PGPLOT window context.
 *  class       int   The type of colormap required, chosen from:
 *                    PseudoColor,StaticColor,GrayScale,StaticGray.
 *  min_col     int   The minimum acceptable number of colors before
 *                    switching to monochrome.
 *  max_col     int   The maximum number of colors to allocate.
 *  readonly    int    If true, try to allocate readonly colors. If
 *                     false try to allocate read/write colors where
 *                     available.
 * Input/Output:
 *  return PgxColor * The context of the new visual/colormap, or NULL
 *                    if sufficient colors could not be obtained.
 */
#ifdef __STDC__
static PgxColor *pgx_find_visual(PgxWin *pgx, int class, int min_col,
				 int max_col, int readonly)
#else
static PgxColor *pgx_find_visual(pgx, class, min_col, max_col, readonly)
     PgxWin *pgx; int class; int min_col; int max_col; int readonly;
#endif
{
  PgxColor *color = NULL;      /* The colormap/visual context to be returned */
  XVisualInfo vi_template;     /* Visual search template */
  XVisualInfo *vi_list = NULL; /* List of matching visuals */
  int nmatch;                  /* Number of matching visuals in vi_list[] */
  VisualID vid;                /* The id of a chosen private visual */
/*
 * If the default colormap has the right class, see if sufficient
 * colors can be allocated from it.
 */
  if(class == pgx_default_class(pgx)) {
    color = pgx_default_visual(pgx, min_col, max_col, readonly);
    if(color->ncol >= max_col)
      return color;
    else
      color = del_PgxColor(pgx, color);
  };
/*
 * We need a private colormap.
 * Get a list of all visuals of the requested class.
 */
  vi_template.class = class;
  vi_list = XGetVisualInfo(pgx->display, (long)VisualClassMask, &vi_template,
			   &nmatch);
  if(!vi_list)
    return NULL;
/*
 * Search the list for a visual that has a colormap size that
 * best matches max_col. Note that the colormap_size memeber of
 * the visual info structure effectively provides the number of
 * "independant" color table entries. Thus the following algorithm
 * works even for colormaps of TrueColor and DirectColor where the
 * colormap_size attribute refers to the size of a single primary color
 * table.
 */
  {
    XVisualInfo *vi_below = NULL;
    XVisualInfo *vi_above = NULL;
    XVisualInfo *vi = NULL;
    for(vi=vi_list; vi<vi_list+nmatch; vi++) {
      if(vi->colormap_size < max_col) {
	if(!vi_below || vi->colormap_size > vi_below->colormap_size)
	  vi_below = vi;
      } else {
	if(!vi_above || vi->colormap_size < vi_above->colormap_size)
	  vi_above = vi;
      };
    };
/*
 * If available, use a visual that has at least max_col independant
 * colors.
 */
    if(vi_above)
      vi = vi_above;
    else if(vi_below)
      vi = vi_below;
    else
      vi = NULL;
/*
 * Get the ID of the visual if suitable.
 */
    vid = (vi && vi->colormap_size > 2) ? vi->visualid : None;
    XFree((char *) vi_list);
/*
 * Did we fail to get a usable visual?
 */
    if(vid == None)
      return NULL;
  };
/*
 * Allocate a visual/colormap context descriptor.
 */
  color = new_PgxColor(pgx, max_col, 0, vid);
  if(!color)
    return NULL;
/*
 * Bracket the colormap acquisition with pgx_start/end_error() calls, to
 * determine whether any allocation errors occur.
 */
  pgx_start_error_watch(pgx);
  color->cmap = XCreateColormap(pgx->display, DefaultRootWindow(pgx->display),
				color->vi->visual, AllocNone);
  if(pgx_end_error_watch(pgx) || color->cmap == None) {
    fprintf(stderr,
         "%s: XCreateColormap failed for visual: id=0x%lx class=%d depth=%u.\n",
	 PGX_IDENT, (unsigned long)color->vi->visualid, color->vi->class,
	    color->vi->depth);
    color->cmap = None;
    return del_PgxColor(pgx, color);
  };
/*
 * Allocate color-cells in the new colormap.
 */
  if(pgx_get_colorcells(pgx, color, min_col, max_col))
    return del_PgxColor(pgx, color);
  color->private = 1;
  color->monochrome = 0;
/*
 * Install the color context.
 */
  pgx->color = color;
/*
 * Initialize the colors.
 */
  if(pgx_init_colors(pgx))
    return pgx_del_visual(pgx);
  return color;
}

/*.......................................................................
 * Private function of pgx_find_visual(), used to allocate color cells for a
 * given colormap and return a count of the number allocated.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context.
 *  color  PgxColor *  The visual/colormap context descriptor.
 *                     The cmap and vi fields must be initialized before
 *                     calling this function.
 *  min_col     int    The minimum acceptable number of colors.
 *  max_col     int    The maximum number of colors to allocate.
 * Output:
 *  color->pixel[]     The colorcell indexes.
 *  color->ncol        The number of color pixels to use from color->pixel[].
 *  color->npixel      The number of private color-cells allocated in pixel[].
 *  return      int    0 - OK.
 *                     1 - Unable to acquire at least min_col colors.
 */
#ifdef __STDC__
static int pgx_get_colorcells(PgxWin *pgx, PgxColor *color,
			      int min_col, int max_col)
#else
static int pgx_get_colorcells(pgx, color, min_col, max_col, readonly)
     PgxWin *pgx; PgxColor *color; int min_col; int max_col;
#endif
{
  XVisualInfo *vi;      /* Visual information (color->vi) */
  Colormap  cmap;       /* Colormap ID (color->cmap) */
  unsigned long maxcol; /* The max number of cells to attempt to allocate */
  int i;
/*
 * Get local pointers to relevant parts of the color context
 * descriptor.
 */
  vi = color->vi;
  cmap = color->cmap;
/*
 * Record the fact that no colors have been allocated.
 */
  color->ncol = color->npixel = 0;
/*
 * Determine the number of color cells in the colormap.
 */
  switch(vi->class) {
  case PseudoColor:
  case GrayScale:
  case StaticColor:
  case StaticGray:
    maxcol = vi->colormap_size;
    break;
  case TrueColor:
  case DirectColor:
/*
 * Determine the maximum number of significant colors available
 * by looking at the total number of bits set in the pixel bit-masks.
 */
    maxcol = 1;
    {
      unsigned long rgb_mask = (vi->red_mask | vi->green_mask | vi->blue_mask);
      do {
	if(rgb_mask & (unsigned long)0x1)
	  maxcol <<= (unsigned long)1;
      } while(maxcol < max_col && (rgb_mask >>= (unsigned long)1) != 0);
    };
    break;
  default:
    maxcol = 0;
    break;
  };
/*
 * Limit the number of colorcells to the size of the color->pixel[] array.
 */
  if(maxcol > max_col)
    maxcol = max_col;
/*
 * Don't try to allocate anything if there are too few colors available.
 */
  if(maxcol < min_col) {
    color->ncol = 0;
/*
 * Defer shared color allocation to pgx_init_colors().
 */
  } else if(color->readonly) {
    for(i=0; i<maxcol; i++)
      color->pixel[i] = 0;
    color->ncol = maxcol;
    color->npixel = 0;  /* No pixels allocated yet */
/*
 * Allocate read/write colorcells.
 */
  } else {
    if(pgx_get_mutable_colorcells(pgx, color, min_col, max_col))
      return 1;
  };
/*
 * Too few colors?
 */
  if(color->ncol < min_col)
    return 1;
/*
 * We got more than two colors.
 */
  color->monochrome = 0;
  return 0;
}

/*.......................................................................
 * This is a private function of pgx_get_colorcells() used to allocate
 * read/write colorcells.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context.
 *  color  PgxColor *  The visual/colormap context descriptor.
 *                     The cmap and vi fields must be initialized before
 *                     calling this function.
 *  min_col     int    The minimum acceptable number of colors.
 *  max_col     int    The maximum number of colors to allocate.
 * Output:
 *  color->pixel[]     The colorcell indexes.
 *  color->ncol        The number of color pixels to use from color->pixel[].
 *  color->npixel      The number of private color-cells allocated in pixel[].
 *  return      int    0 - OK.
 *                     1 - Unable to acquire at least min_col colors.
 */
#ifdef __STDC__
static int pgx_get_mutable_colorcells(PgxWin *pgx, PgxColor *color,
			      int min_col, int max_col)
#else
static int pgx_get_mutable_colorcells(pgx, color, min_col, max_col)
     PgxWin *pgx; PgxColor *color; int min_col; int max_col;
#endif
{
  XVisualInfo *vi;      /* Visual information (color->vi) */
  Colormap  cmap;       /* Colormap ID (color->cmap) */
  unsigned long maxcol; /* The max number of cells to attempt to allocate */
  int ncol;             /* The number of color-cells allocated */
  unsigned long planes[1];  /* Dummy plane array needed by XAllocColorCells() */
  unsigned int nplanes = 0; /* Dummy plane count needed by XAllocColorCells() */
/*
 * Get local pointers to relevant parts of the color context
 * descriptor.
 */
  vi = color->vi;
  cmap = color->cmap;
/*
 * Determine the max number of cells to try to allocate.
 */
  maxcol = vi->colormap_size <= max_col ? vi->colormap_size : max_col;
/*
 * See if we can get all of the colors requested.
 */
  if(XAllocColorCells(pgx->display, cmap, False, planes, nplanes,
		      color->pixel, (unsigned) maxcol)) {
    ncol = maxcol;
/*
 * If there aren't at least min_col color cells available, then
 * give up on this colormap.
 */
  } else if(!XAllocColorCells(pgx->display, cmap, False, planes, nplanes,
			      color->pixel, (unsigned) min_col)) {
    ncol = 0;
  } else {
/*
 * Since we were able to allocate min_col cells, we may be able to
 * allocate more. First discard the min_col cells, so that we can
 * try for a bigger number.
 */
    XFreeColors(pgx->display, cmap, color->pixel, (int) min_col,
		(unsigned long)0);
/*
 * Since there is no direct method to determine the number of allocatable
 * color cells available in a colormap, perform a binary search for the
 * max number that can be allocated. Note that it is possible that another
 * client may allocate colors from the same colormap while we search. This
 * invalidates the result of the search and is the reason for the outer
 * while loop.
 */
    ncol = 0;
    do {
      int lo = min_col;
      int hi = maxcol;
      while(lo<=hi) {
	int mid = (lo+hi)/2;
	if(XAllocColorCells(pgx->display, cmap, False, planes, nplanes,
			    color->pixel, (unsigned) mid)) {
	  ncol = mid;
	  lo = mid + 1;
	  XFreeColors(pgx->display, cmap, color->pixel, mid,
		      (unsigned long)0);
	} else {
	  hi = mid - 1;
	};
      };
    } while(ncol >= min_col &&
	    !XAllocColorCells(pgx->display, cmap, False, planes, nplanes,
			      color->pixel, (unsigned) ncol));
  };
/*
 * Did we fail?
 */
  if(ncol < min_col)
    return 1;
/*
 * Record the number of pixels that will need to be free'd when
 * del_PgxColor() is eventually called.
 */
  color->npixel = ncol;
/*
 * Record the number of colors obtained.
 */
  color->ncol = ncol;
  return 0;
}

/*.......................................................................
 * Return a dynamically allocated visual info structure for a given
 * visual. This is simply a more convenient interface to XGetVisualInfo()
 * and XVisualIDFromVisual().
 *
 * Input:
 *  display    Display *   The display connection to which the visual
 *                         belongs.
 *  screen         int     The screen to which the visual belongs.
 *  vid       VisualID     The ID of the visual for which information is
 *                         required. Note that the ID of a visual can
 *                         be obtained from XVisualIDFromVisual().
 * Output:
 *  return XVisualInfo *   The required information descriptor, or NULL
 *                         on error.
 */
#ifdef __STDC__
static XVisualInfo *pgx_visual_info(Display *display, int screen, VisualID vid)
#else
static XVisualInfo *pgx_visual_info(display, screen, vid)
     Display *display; int screen; VisualID vid;
#endif
{
  XVisualInfo *vi=NULL;  /* The return descriptor */
  XVisualInfo template;  /* The search template */
  int nret = 0;          /* The number of descriptors returned */
/*
 * Using the visual ID and the screen should unambiguously select the
 * information for the specified visual.
 */
  template.visualid = vid;
  template.screen = screen;
  vi = XGetVisualInfo(display, (long)(VisualIDMask | VisualScreenMask),
		      &template, &nret);
  if(vi == NULL || nret < 1) {
    fprintf(stderr,
      "%s: Error getting visual information for visual ID 0x%lx, screen %d.\n",
      PGX_IDENT, (unsigned long)template.visualid, screen);
    vi = NULL;
  };
  return vi;
}

/*.......................................................................
 * Delete the contents of a pgx->color structure.
 *
 * Input:
 *  pgx      PgxWin * The PGPLOT window context.
 * Output:
 *  return PgxColor * The deleted color context (always NULL).
 */
#ifdef __STDC__
PgxColor *pgx_del_visual(PgxWin *pgx)
#else
PgxColor *pgx_del_visual(pgx)
     PgxWin *pgx;
#endif
{
  if(pgx)
    pgx->color = del_PgxColor(pgx, pgx->color);
  return NULL;
}

/*.......................................................................
 * Check a resource string value against visual class names.
 *
 * Input:
 *  str       char *  The string value to be tested (NULL is ok).
 * Output:
 *  return     int    The Visual class parsed, or -1 to select the default.
 */
#ifdef __STDC__
static int pgx_parse_visual(char *str)
#else
static int pgx_parse_visual(str)
     char *str;
#endif
{
/*
 * Create a lookup table of recognised visual classes.
 */
  static struct {
    char *name;  /* Name of visual class */
    int class;   /* Enumerated identifier of visual class */
  } classes[] = {
    {"monochrome",  -2},
    {"default",     -1},
    {"pseudocolor", PseudoColor},
    {"directcolor", TrueColor},    /* We can't handle DirectColor */
    {"staticcolor", StaticColor},
    {"truecolor",   TrueColor},
    {"grayscale",   GrayScale},
    {"staticgray",  StaticGray}
  };
  int i;
/*
 * Lookup the given class name.
 */
  if(str) {
    for(i=0; i<sizeof(classes)/sizeof(classes[0]); i++) {
      if(pgx_same_string(str, classes[i].name))
	return classes[i].class;
    };
/*
 * Class name not recognised.
 */
    fprintf(stderr, "%s: Unrecognised visual type: \"%s\".\n",
	    PGX_IDENT, str);
  };
  return -1;
}

/*.......................................................................
 * Perform a case-insensitive string comparison. Leading and trailing
 * white-space are not significant.
 *
 * Input:
 *  s1,s2  char *   The strings to be compared.
 * Output:
 *  return  int     0 - The strings are not the same.
 *                  1 - The strings are the same.
 */
#ifdef __STDC__
int pgx_same_string(char *s1, char *s2)
#else
int pgx_same_string(s1, s2)
     char *s1; char *s2;
#endif
{
/*
 * If either of the strings are NULL pointers, report that they are equal
 * only if both are NULL.
 */
  if(s1==NULL || s2==NULL)
    return s1==NULL && s2==NULL;
/*
 * Skip leading white-space.
 */
  while(*s1 && isspace(*s1))
    s1++;
  while(*s2 && isspace(*s2))
    s2++;
/*
 * Find the section of the strings that are identical.
 */
  while(*s1 && *s2 &&
   (islower(*s1) ? toupper(*s1) : *s1) == (islower(*s2) ? toupper(*s2) : *s2)) {
    s1++;
    s2++;
  };
/*
 * Skip trailing white-space.
 */
  while(*s1 && isspace(*s1))
    s1++;
  while(*s2 && isspace(*s2))
    s2++;
/*
 * Are the strings equal?
 */
  return (*s1=='\0' && *s2=='\0');
}

/*.......................................................................
 * After a fatal error has occured, this function should be called to
 * mark the specified device as unusable. It emits an error message
 * and sets pgx->bad_device=1.
 *
 * Input:
 *  pgx    PgxWin *  The descriptor of the device on which the error
 *                   occurred.
 * Output:
 *  pgx->bad_device  This flag is set to 1.
 *  return    int    Allways 1 (intended as a boolean to say that the
 *                   device is unusable). This can be used as the return
 *                   value for functions that use 1 to denote an error
 *                   return. eg.
 *                     if(error_occurred)
 *                       return pgx_bad_device(pgx);
 */
#ifdef __STDC__
int pgx_bad_device(PgxWin *pgx)
#else
int pgx_bad_device(pgx)
     PgxWin *pgx;
#endif
{
/*
 * Only report an error if this is the first time that this function
 * has been called on this device.
 */
  if(pgx && !pgx->bad_device) {
    fprintf(stderr, "%s: Lost PGPLOT window.\n", PGX_IDENT);
    pgx->bad_device = 1;
  };
  return 1;
}

/*.......................................................................
 * Draw a line segment in an open PGPLOT window.
 *
 * Input:
 *  pgx     PgxWin *  The PGPLOT window context.
 *  rbuf     float *  The array of float arguments sent by the PGPLOT
 *                    GREXEC() subroutine.
 */
#ifdef __STDC__
void pgx_draw_line(PgxWin *pgx, float *rbuf)
#else
void pgx_draw_line(pgx, rbuf)
     PgxWin *pgx; float *rbuf;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN | PGX_NEED_PIXMAP)) {
    PgxState *state = pgx->state;
/*
 * Convert from PGPLOT coordinates to X coordinates.
 */
    XPoint start;
    XPoint end;
    pgx_xy_to_XPoint(pgx, &rbuf[0], &start);
    pgx_xy_to_XPoint(pgx, &rbuf[2], &end);
/*
 * Draw the line segment.
 */
    XDrawLine(pgx->display, pgx->pixmap, state->gc,
	      start.x, start.y, end.x, end.y);
/*
 * Record the extent of the modified region of the pixmap.
 */
    pgx_mark_modified(pgx, start.x, start.y, state->gcv.line_width);
    pgx_mark_modified(pgx, end.x, end.y, state->gcv.line_width);
  };
  return;
}

/*.......................................................................
 * Draw a single dot in an open PGPLOT window.
 *
 * Input:
 *  pgx     PgxWin *  The PGPLOT window context.
 *  rbuf     float *  The array of float arguments sent by the PGPLOT
 *                    GREXEC() subroutine.
 */
#ifdef __STDC__
void pgx_draw_dot(PgxWin *pgx, float *rbuf)
#else
void pgx_draw_dot(pgx, rbuf)
     PgxWin *pgx; float *rbuf;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN | PGX_NEED_PIXMAP)) {
    PgxState *state = pgx->state;
    XPoint xp;
/*
 * Workd out the radius of the dot.
 */
    int radius = state->gcv.line_width/2;
/*
 * Convert from PGPLOT coordinates to window coordinates.
 */
    pgx_xy_to_XPoint(pgx, rbuf, &xp);
/*
 * Draw a pixel-sized point, or a larger circular dot?
 */
    if(radius < 1) {
      XDrawPoint(pgx->display, pgx->pixmap, state->gc, xp.x, xp.y);
    } else {
      unsigned int diameter = radius*2;
      int x = xp.x - radius;
      int y = xp.y - radius;
      XFillArc(pgx->display, pgx->pixmap, state->gc, x, y,
	       diameter, diameter, 0, 23040);
    };
/*
 * Record the extent of the modified region of the pixmap.
 */
    pgx_mark_modified(pgx, xp.x, xp.y, state->gcv.line_width);
  };
  return;
}

/*.......................................................................
 * Convert from the coordinates sent by PGPLOT in rbuf[...] to an
 * X-windows point in the coordinate system of the pixmap.
 *
 * Input:
 *  pgx    PgxWin *   The PGPLOT window context.
 *  xy      float [2] Array of two floats containing PGPLOT coordinates
 *                    arranged as x followed by y.
 * Output:
 *  xp     XPoint *   The converted coordinates will be assigned to xp->x
 *                    and xp->y.
 */
#ifdef __STDC__
static void pgx_xy_to_XPoint(PgxWin *pgx, float *xy, XPoint *xp)
#else
static void pgx_xy_to_XPoint(pgx, xy, xp)
     PgxWin *pgx; float *xy; XPoint *xp;
#endif
{
  PgxState *state = pgx->state;
  float x = xy[0];
  float y = xy[1];
/*
 * Limit the coordinates to lie within the pixmap.
 */
  if(x < 0)
    x = 0;
  if(x >= state->geom.width)
    x = state->geom.width;
  if(y < 0)
    y = 0;
  if(y >= state->geom.height)
    y = state->geom.height;
/*
 * Convert to pixmap coordinates.
 */
  xp->x = state->geom.xmin + (int)(x + 0.5);
  xp->y = state->geom.ymax - (int)(y + 0.5);
}

/*.......................................................................
 * Convert from pixmap pixel coordinates to PGPLOT coordinates, in a
 * form that can be returned to PGPLOT via rbuf[...].
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 *  xp     XPoint *  The pixmap pixel-coordinates to be converted.
 * Output:
 *  xy      float [2] Output array of two floats in which to place the
 *                    PGPLOT coordinates, arranged as x followed by y.
 */
#ifdef __STDC__
static void pgx_XPoint_to_xy(PgxWin *pgx, XPoint *xp, float *xy)
#else
static void pgx_XPoint_to_xy(pgx, xp, xy)
     PgxWin *pgx; XPoint *xp; float *xy;
#endif
{
  PgxState *state = pgx->state;
  xy[0] = (float) (xp->x - state->geom.xmin);
  xy[1] = (float) (state->geom.ymax - xp->y);
}

/*.......................................................................
 * Update the vertices of the rectangular area that has been modified
 * since the last time the window was updated from the pixmap.
 *
 * Input:
 *  pgx    PgxWin * The PGPLOT window context.
 *  x         int   The x-axis pixel index that the rectangular update area
 *                  must be extended to include.
 *  y         int   The y-axis pixel index that the rectangular update area
 *                  must be extended to include.
 *  diameter  int   The diameter of the locus in pixels. For line or
 *                  point drawing operations this is usually the line width.
 */
#ifdef __STDC__
static void pgx_mark_modified(PgxWin *pgx, int x, int y, int diameter)
#else
static void pgx_mark_modified(pgx, x, y, diameter)
     PgxWin *pgx; int x; int y; int diameter;
#endif
{
  PgxState *state = pgx->state;
  int radius = diameter/2;
/*
 * Expand the current rectangle to include point (x,y).
 */
  if(state->update.modified) {
    if(x - radius < state->update.xmin)
      state->update.xmin = x - radius;
    if(x + radius > state->update.xmax)
      state->update.xmax = x + radius;
    if(y - radius < state->update.ymin)
      state->update.ymin = y - radius;
    if(y + radius > state->update.ymax)
      state->update.ymax = y + radius;
  } else {
    state->update.xmin = x - radius;
    state->update.xmax = x + radius;
    state->update.ymin = y - radius;
    state->update.ymax = y + radius;
    state->update.modified = 1;
  };
  return;
}

/*.......................................................................
 * Flush changes in the pixmap to the window.
 *
 * Input:
 *  pgx   PgxWin * The PGPLOT window context.
 * Output:
 *  return   int    0 - OK.
 *                  1 - Error.
 */
#ifdef __STDC__
int pgx_flush(PgxWin *pgx)
#else
int pgx_flush(pgx)
     PgxWin *pgx;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN | PGX_NEED_PIXMAP)) {
    PgxState *state = pgx->state;
/*
 * Flush buffered opcodes if necessary.
 */
    if(state->flush_opcode_fn != (Flush_Opcode_fn) 0) {
      (*state->flush_opcode_fn)(pgx);
      state->flush_opcode_fn = (Flush_Opcode_fn) 0;
      if(pgx->bad_device)
	return 1;
    };
/*
 * Copy the modified rectangular area of the pixmap to the PGPLOT window.
 */
    if(state->update.modified) {
/*
 * Enforce bounds on the area to be updated.
 */
      if(state->update.xmin < 0)
	state->update.xmin = 0;
      if(state->update.ymin < 0)
	state->update.ymin = 0;
      if(state->update.xmax > state->geom.width - 1)
	state->update.xmax = state->geom.width - 1;
      if(state->update.ymax > state->geom.height - 1)
	state->update.ymax = state->geom.height - 1;
/*
 * Copy the area to be updated from the pixmap to the window.
 */
      if(!pgx->bad_device) {
	pgx_copy_area(pgx, state->update.xmin, state->update.ymin,
		      (unsigned) (state->update.xmax - state->update.xmin + 1),
		      (unsigned) (state->update.ymax - state->update.ymin + 1),
		      (int) (state->update.xmin - pgx->scroll.x),
		      (int) (state->update.ymin - pgx->scroll.y));
	if(pgx->bad_device)
	  return 1;
      };
      state->update.modified = 0;
    };
/*
 * Redraw the potentially damaged rubber-band cursor if it is active.
 */
    pgx_refresh_cursor(pgx);
/*
 * Make sure that the window is up to date.
 */
    XFlush(pgx->display);
    if(pgx->bad_device)
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Set the foreground color.
 *
 * Input:
 *  pgx  PgxWin *  The PGPLOT window context.
 *  ci      int    The PGPLOT color index to instate as the foreground
 *                 color.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
#ifdef __STDC__
int pgx_set_ci(PgxWin *pgx, int ci)
#else
int pgx_set_ci(pgx, ci)
     PgxWin *pgx; int ci;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN)) {
    PgxState *state = pgx->state;
/*
 * Assign white to out-of range color indexes.
 */
    if(ci < 0 || ci >= pgx->color->ncol)
      ci = 1;
/*
 * Determine the color pixel associated with the given color index.
 */
    state->gcv.foreground = pgx->color->pixel[ci];
/*
 * Instate the new foreground color.
 */
    XSetForeground(pgx->display, state->gc, state->gcv.foreground);
    if(pgx->bad_device)
      return 1;
  };
  return 0;
}

/*.......................................................................
 * This function is called mulitple times to accumulate a list of
 * polygon vertices and finally draw them. This protocol is mandated
 * by the PGPLOT GREXEC driver dispatch function.
 *
 * Input:
 *  pgx     PgxWin *  The PGPLOT window context.
 *  rbuf     float *  The array of float arguments sent by the PGPLOT
 *                    GREXEC() subroutine.
 */
#ifdef __STDC__
void pgx_poly_fill(PgxWin *pgx, float *rbuf)
#else
void pgx_poly_fill(pgx, rbuf)
     PgxWin *pgx; float *rbuf;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN | PGX_NEED_PIXMAP)) {
    PgxState *state = pgx->state;
/*
 * The first call specifies just the number of vertixes in the polygon.
 */
    if(state->poly.npoint == 0) {
      state->poly.npoint = (int) (rbuf[0] + 0.5);
      state->poly.points = (XPoint *) malloc(sizeof(XPoint) * state->poly.npoint);
      if(state->poly.points == NULL)
	fprintf(stderr, "%s: Insufficient memory for polygon points.\n",
		PGX_IDENT);
      state->poly.ndone = 0;
/*
 * The next state->poly.npoint calls specify the vertexes of the polygon.
 */
    } else {
/*
 * Ignore the points if the above malloc() failed.
 */
      if(state->poly.points) {
	XPoint *xp = &state->poly.points[state->poly.ndone];
	pgx_xy_to_XPoint(pgx, rbuf, xp);
	pgx_mark_modified(pgx, xp->x, xp->y, 1);
      };
/*
 * Maintain the count of the number of points, even if no memory for the
 * points is available. Thus we can just ignore all calls until
 * state->poly.ndone == state->poly.npoint.
 */
      state->poly.ndone++;
/*
 * On the last call display the filled polygon and release the memory used
 * to store its vertexes.
 */
      if(state->poly.ndone >= state->poly.npoint) {
	if(state->poly.points) {
	  XFillPolygon(pgx->display, pgx->pixmap, state->gc, state->poly.points,
		       state->poly.npoint, Complex, CoordModeOrigin); 
	  free((char *)state->poly.points);
	  state->poly.points = NULL;
	};
	state->poly.npoint = 0;
      };
    };
  };
  return;
}

/*.......................................................................
 * Draw a filled rectangle.
 *
 * Input:
 *  pgx     PgxWin *  The PGPLOT window context.
 *  rbuf     float *  The array of float arguments sent by the PGPLOT
 *                    GREXEC() subroutine.
 */
#ifdef __STDC__
void pgx_rect_fill(PgxWin *pgx, float *rbuf)
#else
void pgx_rect_fill(pgx, rbuf)
     PgxWin *pgx; float *rbuf;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN | PGX_NEED_PIXMAP)) {
/*
 * Convert from PGPLOT coordinates to X coordinates.
 */
    XPoint blc;
    XPoint trc;
    pgx_xy_to_XPoint(pgx, &rbuf[0], &blc);
    pgx_xy_to_XPoint(pgx, &rbuf[2], &trc);
/*
 * Fill the rectangle in the pixmap.
 */
    XFillRectangle(pgx->display, pgx->pixmap, pgx->state->gc, blc.x, trc.y,
		   (unsigned)(trc.x-blc.x+1), (unsigned)(blc.y-trc.y+1));
/*
 * Record the extent of the modified part of the pixmap.
 */
    pgx_mark_modified(pgx, blc.x, blc.y, 1);
    pgx_mark_modified(pgx, trc.x, trc.y, 1);
  };
  return;
}

/*.......................................................................
 * Set the line width for subsequent drawing.
 *
 * Input:
 *  pgx     PgxWin *  The PGPLOT window context.
 *  lw       float    The new line width in units of 0.005 inches.
 */
#ifdef __STDC__
void pgx_set_lw(PgxWin *pgx, float lw)
#else
void pgx_set_lw(pgx, lw)
     PgxWin *pgx; float lw;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN)) {
    PgxState *state = pgx->state;
/*
 * The line width is provided in multiples of 0.005 inches.
 */
    state->gcv.line_width = (lw * 0.005 * state->geom.xpix_per_inch) + 0.5;
    XChangeGC(pgx->display, state->gc, (unsigned long)GCLineWidth, &state->gcv);
  };
  return;
}

/*.......................................................................
 * Render a line of pixels.
 *
 * Input:
 *  pgx     PgxWin *  The PGPLOT window context.
 *  rbuf     float *  The array of float arguments sent by the PGPLOT
 *                    GREXEC() subroutine.
 *  nbuf       int *  The number of floats passed in rbuf[] by GREXEC.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
#ifdef __STDC__
int pgx_pix_line(PgxWin *pgx, float *rbuf, int *nbuf)
#else
int pgx_pix_line(pgx, rbuf, nbuf)
     PgxWin *pgx; float *rbuf; int *nbuf;
#endif
{
  int i;
  if(pgx_ready(pgx, PGX_NEED_PGOPEN | PGX_NEED_PIXMAP) &&
     !pgx->color->monochrome) {
    PgxState *state = pgx->state;  /* The PGPLOT drawing-state context */
    int ndone;                     /* The number of pixels drawn so far */
/*
 * Extract the array of pixels and the recorded number thereof.
 */
    float *cells = rbuf + 2;       /* Array of pixels */
    int ncell = *nbuf - 2;         /* The number of pixels in cells[] */
/*
 * Get the container of the X image used to transport lines of pixels.
 */
    XWimage *image = &pgx->state->image;
/*
 * The first two elements of the rbuf[] array contain the
 * X and Y PGPLOT coordinates of the start of the line of pixels.
 * Convert this to X coordinates.
 */
    XPoint start;
    pgx_xy_to_XPoint(pgx, rbuf, &start);
/*
 * Draw up to PGX_IMAGE_LEN pixels at a time. This is the size of the
 * buffer: state->xi->data[].
 */
    for(ndone=0; !pgx->bad_device && ndone<ncell; ndone += PGX_IMAGE_LEN) {
      int ntodo = ncell - ndone;
      int nimage = ntodo < PGX_IMAGE_LEN ? ntodo : PGX_IMAGE_LEN;
/*
 * Load the image buffer with the color cell indexes assigned to the
 * given PGPLOT color indexes.
 */
      if(pgx->color->vi->depth == 8) {
	for(i=0; i<nimage; i++)
	  image->xi->data[i] = pgx->color->pixel[(int) (cells[ndone+i] + 0.5)];
      } else {
	for(i=0; i<nimage; i++) {
	  XPutPixel(image->xi, i, 0,
		    pgx->color->pixel[(int) (cells[ndone+i] + 0.5)]);
	};
      };
/*
 * Display the image.
 */
      XPutImage(pgx->display, pgx->pixmap, state->gc, image->xi, 0, 0,
		start.x+ndone, start.y, (unsigned) nimage, (unsigned) 1);
    };
/*
 * Extend the region to be updated on the next flush.
 */
    pgx_mark_modified(pgx, start.x, start.y, 1);
    pgx_mark_modified(pgx, start.x + ncell - 1, start.y, 1);
  };
  if(pgx->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * Record the latest world-coordinate conversion parameters as provided
 * by the PGPLOT driver opcode 27. Note that opcode 27 only gets invoked
 * by PGPLOT if the second character of the attribute string returned by
 * opcode 4 is set to 'X'.
 *
 * Input:
 *  pgx     PgxWin *  The PGPLOT window context.
 *  rbuf     float *  The array of float arguments sent by the PGPLOT
 *                    GREXEC() subroutine for opcode 27. In order these
 *                    are: xoff, xdiv, yoff and ydiv; where:
 *                      world_x = (device_x - xoff) / xdiv
 *                      world_y = (device_y - yoff) / ydiv
 */
#ifdef __STDC__
void pgx_set_world(PgxWin *pgx, float *rbuf)
#else
void pgx_set_world(pgx, rbuf)
     PgxWin *pgx; float *rbuf;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN)) {
    XWworld *world = &pgx->state->world;
    world->xoff = rbuf[0];
    world->xdiv = rbuf[1];
    world->yoff = rbuf[2];
    world->ydiv = rbuf[3];
  };
}

/*.......................................................................
 * Assign a given RGB color representation to a given color index in
 * pgx->color->xcolor and if the device is open to PGPLOT, record the
 * extent of the modifications in pgx->state->update and
 * register pgx_update_colors() to pgx->state->flush_opcode_fn().
 *
 * Input:
 *  pgx  PgxWin *  The PGPLOT window context.
 *  ci      int    The color index to assign the color to. Out of range
 *                 indexes are quietly ignored.
 *  red   float    The fractional red brightness 0..1.
 *  green float    The fractional green brightness 0..1. 
 *  blue  float    The fractional blue brightness 0..1.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
#ifdef __STDC__
int pgx_set_rgb(PgxWin *pgx, int ci, float red, float green, float blue)
#else
int pgx_set_rgb(pgx, ci, red, green, blue)
     PgxWin *pgx; int ci; float red; float green; float blue;
#endif
{
  float gray;   /* Gray-scale intensity */
  XColor *xc;   /* The descriptor of the new color */
/*
 * Do we have a valid device.
 */
  if(pgx_ready(pgx, PGX_NEED_COLOR)) {
    PgxState *state = pgx->state;
/*
 * Limit RGB values to be between 0 and 1.
 */
    if(red   < 0.0) red   = 0.0;
    if(green < 0.0) green = 0.0;
    if(blue  < 0.0) blue  = 0.0;
    if(red   > 1.0) red   = 1.0;
    if(green > 1.0) green = 1.0;
    if(blue  > 1.0) blue  = 1.0;
/*
 * Color index in range?
 */
    if(!pgx->color->monochrome && ci >= 0 && ci < pgx->color->ncol) {
/*
 * Get the color representation descriptor.
 */
      xc = &pgx->color->xcolor[ci];
/*
 * Get the pixel to be assigned the new color representation.
 */
      xc->pixel = pgx->color->pixel[ci];
      xc->flags = DoRed | DoGreen | DoBlue;
      xc->pad   = 0;
/*
 * Determine the appropriate RGB values for the type of colormap.
 */
      switch(pgx->color->vi->class) {
      case PseudoColor:
      case StaticColor:
      case DirectColor:
      case TrueColor:
	xc->red   = (int) (red * PGX_COLORMULT + 0.5);
	xc->green = (int) (green * PGX_COLORMULT + 0.5);
	xc->blue  = (int) (blue * PGX_COLORMULT + 0.5);
	break;
      case GrayScale:
      case StaticGray:
/*
 * For gray-scale colormaps the red,green and blue intensities must all be
 * equal. Weight the colors so that what is brightest to the eye, is also
 * brighter in grayscale, and so that different colors of equal intensity
 * appear different in grayscale. Note that the 3 weights must add up to 1.0.
 * The black and white TV standard says to use 0.3*R+0.59*G+0.11*B.
 * Unfortunately blue pretty much dissapears in this scheme. The following
 * is a compromise between making all colors visible and making different
 * colors look different in grayscale.
 */
	gray = 0.35*red + 0.40*green + 0.25*blue;
	xc->red = xc->green = xc->blue = (int) (gray * PGX_COLORMULT + 0.5);
	break;
      };
/*
 * Update the recorded range of color indexes whose color representations
 * have been changed since the last call to pgx_update_colors().
 */
      if(state) {
	if(state->color.nbuff<=0) {
	  state->color.sbuff = ci;
	  state->color.nbuff = 1;
	} else if(ci < state->color.sbuff) {
	  state->color.nbuff += state->color.sbuff - ci;
	  state->color.sbuff = ci;
	} else if(ci > state->color.sbuff + state->color.nbuff-1) {
	  state->color.nbuff = ci - state->color.sbuff + 1;
	};
/*
 * Register pgx_update_colors() to be called to flush the colors to the
 * window. Don't do this if we are sharing readonly colors, because
 * these should not be reallocated until the start of the next page.
 */
	if(!pgx->color->readonly)
	  state->flush_opcode_fn = (Flush_Opcode_fn) pgx_update_colors;
      };
    };
  };
  return 0;
}

/*.......................................................................
 * Initialize the color representations in the color table.
 * pgx_get_visual() must have been called prior to calling this function,
 * so that we have a visual and colormap to define the colors in.
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 * Output:
 *  pgx->color->xcolor[0..ncol] The color pixel definitions.
 *  return    int    0 - OK.
 *                   1 - Error.
 */
#ifdef __STDC__
static int pgx_init_colors(PgxWin *pgx)
#else
static int pgx_init_colors(pgx)
     PgxWin *pgx;
#endif
{
/*
 * Define the standard PGPLOT line colors (RGB).
 */
  static float ctable[PGX_NCOLORS][3] = {
    {0.0,0.0,0.0}, {1.0,1.0,1.0}, {1.0,0.0,0.0}, {0.0,1.0,0.0},
    {0.0,0.0,1.0}, {0.0,1.0,1.0}, {1.0,0.0,1.0}, {1.0,1.0,0.0},
    {1.0,0.5,0.0}, {0.5,1.0,0.0}, {0.0,1.0,0.5}, {0.0,0.5,1.0},
    {0.5,0.0,1.0}, {1.0,0.0,0.5}, {0.333,0.333,0.333}, 
    {0.667,0.667,0.667}
  };
  int i;
  if(pgx_ready(pgx, PGX_NEED_COLOR)) {
/*
 * Initialize the color-table with the standard PGPLOT line colors.
 */
    if(!pgx->color->monochrome) {
      int ncol = (PGX_NCOLORS < pgx->color->ncol) ? PGX_NCOLORS:pgx->color->ncol;
      for(i=0; i<ncol; i++) {
	if(pgx_set_rgb(pgx, i, ctable[i][0], ctable[i][1], ctable[i][2]))
	  return 1;
      };
/*
 * Initialize the rest of the colors with a grey-scale ramp.
 */
      for(i=ncol; i<pgx->color->ncol; i++) {
	float grey= (float) (i-PGX_NCOLORS) / 
	            (float) (pgx->color->ncol-1-PGX_NCOLORS);
	if(pgx_set_rgb(pgx, i, grey, grey, grey))
	  return 1;
      };
    };
/*
 * Flush the new color definitions to the display.
 */
    if(pgx_flush_colors(pgx, 0, pgx->color->ncol))
      return 1;
/*
 * Start with the foreground color set to white.
 */
    if(pgx_set_ci(pgx, 1))
      return 1;
/*
 * Record the color-cells as allocated.
 */
    pgx->color->initialized = 1;
  };
  return 0;
}

/*.......................................................................
 * Return the color representation of a given color index.
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 * Input/Output:
 *  rbuf    float *  The return array passed from GREXEC.
 *  nbuf      int *  The output number of float results for GREXEC.
 */
#ifdef __STDC__
void pgx_get_rgb(PgxWin *pgx, float *rbuf, int *nbuf)
#else
void pgx_get_rgb(pgx, rbuf, nbuf)
     PgxWin *pgx; float *rbuf; int *nbuf;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_COLOR)) {
    int ci = (int) (rbuf[0] + 0.5);
    rbuf[1] = (float) pgx->color->xcolor[ci].red / (float) PGX_COLORMULT;
    rbuf[2] = (float) pgx->color->xcolor[ci].green / (float) PGX_COLORMULT;
    rbuf[3] = (float) pgx->color->xcolor[ci].blue / (float) PGX_COLORMULT;
  } else {
    rbuf[1] = rbuf[2] = rbuf[3] = 0;
  };
  *nbuf = 4;
  return;
}

/*.......................................................................
 * Flush color-representation changes made by xw_set_rgb() to the window.
 * This updates the window colormap. If color index 0 is changed
 * then the background color is also updated.
 * 
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 *  ci_start  int    The first color index to update.
 *  ncol      int    The number of colors to update.
 *                   Color indexes ci_state -> ci_start + ncol - 1 will
 *                   be updated.
 * Output:
 *  return    int      0 - OK.
 *                     1 - Error.
 */
#ifdef __STDC__
static int pgx_flush_colors(PgxWin *pgx, int ci_start, int ncol)
#else
static int pgx_flush_colors(pgx, ci_start, ncol)
     PgxWin *pgx; int ci_start; int ncol;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_COLOR) && !pgx->color->monochrome) {
/*
 * If the range of color indexes is invalid, warn the user and
 * then arrange to update the whole colormap.
 */
    if(ci_start < 0 || ci_start + ncol - 1 >= pgx->color->ncol) {
      fprintf(stderr, "%s: pgx_flush_colors: color indexes out of range.\n",
	      PGX_IDENT);
      ci_start = 0;
      ncol = pgx->color->ncol;
    };
/*
 * Are there any colors to be updated?
 */
    if(ncol > 0) {
      XColor *xc = &pgx->color->xcolor[ci_start];
      unsigned long *pixel = &pgx->color->pixel[ci_start];
/*
 * Allocate shared colorcells?
 */
      if(pgx->color->readonly) {
	if(pgx_readonly_colors(pgx, ncol, xc, pixel))
	  return 1;
/*
 * Modify existing read/write colorcells.
 */
      } else {
	XStoreColors(pgx->display, pgx->color->cmap, xc, ncol);
      };
/*
 * Device error?
 */
      if(pgx->bad_device)
	return 1;
/*
 * Update the background color?
 */
      if(ci_start == 0 && pgx->window != None)
	XSetWindowBackground(pgx->display, pgx->window, pixel[0]);
    };
  };
  return pgx->bad_device!=0;
}

/*.......................................................................
 * This is a front end to pgx_flush_colors() to be used when pgplot has
 * the window open and there are buffered colormap updates from previous
 * pgscr() opcodes.
 *
 * 
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 * Output:
 *  return    int      0 - OK.
 *                     1 - Error.
 */
#ifdef __STDC__
static int pgx_update_colors(PgxWin *pgx)
#else
static int pgx_update_colors(pgx)
     PgxWin *pgx;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN)) {
    PgxState *state = pgx->state;
/*
 * Are there any colors to be updated?
 */
    if(state->color.nbuff > 0) {
      if(pgx_flush_colors(pgx, state->color.sbuff, state->color.nbuff))
	return 1;
/*
 * Reset buffer pointers.
 */
      state->color.nbuff = 0;
      state->color.sbuff = 0;
    };
  };
  return 0;
}

/*.......................................................................
 * Clear a PGPLOT window and (if allocated) the associated pixmap, to
 * the background color of the window. This function may be called
 * before pgx_open() is called.
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Error.
 */
#ifdef __STDC__
int pgx_clear_window(PgxWin *pgx)
#else
int pgx_clear_window(pgx)
     PgxWin *pgx;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_WINDOW)) {
/*
 * Clear the window itself.
 */
    if(pgx->clip.doclip) {
      pgx_clear_area(pgx, pgx->clip.xmin, pgx->clip.ymin,
		     (pgx->clip.xmax - pgx->clip.xmin + 1),
		     (pgx->clip.ymax - pgx->clip.ymin + 1));
    } else {
      XClearWindow(pgx->display, pgx->window);
    };
    if(pgx->bad_device)
      return 1;
/*
 * Flush any defered readonly color-cell updates.
 */
    if(pgx_update_colors(pgx))
      return 1;
/*
 * Fill the pixmap with the background color.
 */
    if(pgx_ready(pgx, PGX_NEED_COLOR | PGX_NEED_PIXMAP)) {
      Window root;
      int x, y;
      unsigned width, height, border, depth;
/*
 * Determine the size of the pixmap.
 */
      XGetGeometry(pgx->display, pgx->pixmap, &root, &x, &y, &width, &height,
		   &border, &depth);
/*
 * Clear the pixmap by drawing an opaque rectangle over it in the background
 * color. Use the exposure graphical context to avoid changing the
 * current foreground color and to allow this function to be called
 * before pgx_open().
 */
      XSetForeground(pgx->display, pgx->expose_gc, pgx->color->pixel[0]);
      XFillRectangle(pgx->display, pgx->pixmap, pgx->expose_gc, 0, 0,
		     width, height);
      if(pgx->bad_device)
	return 1;
/*
 * Mark the pixmap as unmodified.
 */
      if(pgx->state)
	pgx->state->update.modified = 0;
    };
    XFlush(pgx->display);
    if(pgx->bad_device)
      return 1;
  };
  return pgx->bad_device ? 1 : 0;
}

/*.......................................................................
 * Return the device resolution in pixels per inch. This can be used
 * to implement PGPLOT opcode 3.
 * If pgx==NULL or pgx->display==NULL or pgx->bad_device!=0, then dummy
 * values will be returned.
 *
 * Input:
 *  pgx           PgxWin *  The PGPLOT window context.
 * Input/Output:
 *  xpix_per_inch  float *  The number of pixels per inch along X.
 *  ypix_per_inch  float *  The number of pixels per inch along Y.
 */
#ifdef __STDC__
void pgx_get_resolution(PgxWin *pgx, float *xpix_per_inch, float *ypix_per_inch)
#else
void pgx_get_resolution(pgx, xpix_per_inch, ypix_per_inch)
     PgxWin *pgx; float *xpix_per_inch; float *ypix_per_inch;
#endif
{
  if(pgx && pgx->display && !pgx->bad_device) {
    Display *display = pgx->display;
    int screen = DefaultScreen(display);
    unsigned int d_pix_width = DisplayWidth(display, screen);
    unsigned int d_pix_height = DisplayHeight(display, screen);
    unsigned int d_mm_width = DisplayWidthMM(display, screen);
    unsigned int d_mm_height = DisplayHeightMM(display, screen);
/*
 * Determine the device resolution in pixels per inch.
 */
    if(xpix_per_inch)
      *xpix_per_inch = 25.4 * ((double)d_pix_width / (double)d_mm_width);
    if(ypix_per_inch)
      *ypix_per_inch = 25.4 * ((double)d_pix_height / (double)d_mm_height);
    return;
  } else {
    if(xpix_per_inch)
      *xpix_per_inch = 1.0;
    if(ypix_per_inch)
      *ypix_per_inch = 1.0;
  };
  return;
}

/*.......................................................................
 * Return the default size of the plot area in the form required by
 * PGPLOT opcode 6.
 *
 * If pgx==NULL, pgx->display==NULL or pgx->bad_device!=0, then specified
 * default values will be returned.
 *
 * Input:
 *  pgx        PgxWin *  The PGPLOT window context.
 *  d_width  unsigned    The default width to be specified if the
 *                       device is not open or is in an innapropriate
 *                       state.
 *  d_height unsigned    The default height to be specified if the
 *                       device is not open or is in an innapropriate
 *                       state.
 * Input/Output:
 *  rbuf    float *  The return array passed from GREXEC.
 *  nbuf      int *  The output number of float results for GREXEC.
 */
#ifdef __STDC__
void pgx_def_size(PgxWin *pgx, unsigned d_width, unsigned d_height,
		 float *rbuf, int *nbuf)
#else
void pgx_def_size(pgx, d_width, d_height, rbuf, nbuf)
     PgxWin *pgx; unsigned d_width; unsigned d_height; float *rbuf; int *nbuf;
#endif
{
/*
 * Set invariants.
 */
  rbuf[0] = 0.0;
  rbuf[2] = 0.0;
  *nbuf = 4;
/*
 * If we have sufficient information, return the current size of the
 * window excluding margins.
 */
  if(pgx && pgx->display && !pgx->bad_device) {
    XWindowAttributes attr;
    XGetWindowAttributes(pgx->display, pgx->window, &attr);
    if(!pgx->bad_device) {
      rbuf[1] = (float) (attr.width - 2 * pgx->xmargin);
      rbuf[3] = (float) (attr.height - 2 * pgx->ymargin);
      if(rbuf[1] > 2 && rbuf[3] > 2)
	return;
    };
  };
/*
 * If the device is not yet open, or if an error occured above, substitute
 * the default dimensions.
 */
  rbuf[1] = d_width;
  rbuf[3] = d_height;
  return;
}

/*.......................................................................
 * Start a new page of a specified size. This includes resizing the
 * window and pixmap if necessary and clearing them. It also includes
 * initializing pgx->state->geom to reflect the size of the pixmap.
 *
 * Input:
 *  pgx        PgxWin *  The PGPLOT window context.
 *  rbuf        float *  The array of float arguments sent by the PGPLOT
 *                       GREXEC() subroutine.
 */
#ifdef __STDC__
void pgx_begin_picture(PgxWin *pgx, float *rbuf)
#else
void pgx_begin_picture(pgx, rbuf)
     PgxWin *pgx; float *rbuf;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN)) {
    PgxState *state = pgx->state;
/*
 * Determine the device resolution.
 */
    pgx_get_resolution(pgx, &state->geom.xpix_per_inch,
		       &state->geom.ypix_per_inch);
/*
 * Determine the X and Y axis margins.
 */
    state->geom.xmargin = pgx->xmargin;
    state->geom.ymargin = pgx->ymargin;
/*
 * Convert the passed max X and Y coordinates into the total width of the
 * new window and pixmap. Add margins to the requested area.
 */
    state->geom.width  = (int) (rbuf[0] + 0.5) + 2 * state->geom.xmargin;
    state->geom.height = (int) (rbuf[1] + 0.5) + 2 * state->geom.ymargin;
/*
 * Record the coordinate bounds of the required window area.
 */
    state->geom.xmin = state->geom.xmargin;
    state->geom.xmax = state->geom.width - state->geom.xmargin;
    state->geom.ymin = state->geom.ymargin;
    state->geom.ymax = state->geom.height - state->geom.ymargin;
/*
 * Resize the window if necessary.
 */
    if(!pgx->bad_device) {
      XWindowAttributes attr;
      XGetWindowAttributes(pgx->display, pgx->window, &attr);
      if(!pgx->bad_device)
	if(pgx->resize_fn && (attr.width != state->geom.width ||
			      attr.height != state->geom.height)) {
	  (*pgx->resize_fn)(pgx, state->geom.width, state->geom.height);
	};
    };
/*
 * If a pixmap exists and has a different size to that requested,
 * delete it.
 */
    if(!pgx->bad_device && pgx->pixmap != None) {
      Window root;
      int x, y;
      unsigned width, height, border, depth;
/*
 * Determine the size of the existing pixmap.
 */
      XGetGeometry(pgx->display, pgx->pixmap, &root, &x, &y, &width, &height,
		   &border, &depth);
/*
 * Delete it if it has the wrong size.
 */
      if(width != state->geom.width || height != state->geom.height) {
	XFreePixmap(pgx->display, pgx->pixmap);
	pgx->pixmap = None;
      };
    };
/*
 * Create a new pixmap if necessary.
 */
    if(!pgx->bad_device && pgx->pixmap==None) {
      if(!pgx->new_pixmap_fn)
	pgx->new_pixmap_fn = pgx_new_pixmap;
      (*pgx->new_pixmap_fn)(pgx, state->geom.width, state->geom.height);
    };
  };
/*
 * Clear the window and pixmap.
 */
  pgx_clear_window(pgx);
/*
 * Reset the scroll and pan offsets.
 */
  pgx_scroll(pgx, 0, 0);
  return;
}

/*.......................................................................
 * Allocate a new Pixmap for a PGPLOT window of a given size.
 * Note that pgx->pixmap should be deleted and assigned None before
 * this function is called.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context.
 *  width  unsigned    The required width of the pixmap (pixels).
 *  height unsigned    The required height of the pixmap (pixels).
 */
#ifdef __STDC__
void pgx_new_pixmap(PgxWin *pgx, unsigned width, unsigned height)
#else
void pgx_new_pixmap(pgx, width, height)
     PgxWin *pgx; unsigned width; unsigned height;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_COLOR | PGX_NEED_WINDOW)) {
/*
 * Bracket the pixmap acquisition with pgx_start/end_error() calls, to
 * determine whether any allocation errors occur.
 */
    pgx_start_error_watch(pgx);
    pgx->pixmap = XCreatePixmap(pgx->display, pgx->window, width, height,
				(unsigned) pgx->color->vi->depth);
    if(pgx_end_error_watch(pgx) || pgx->pixmap==None) {
      fprintf(stderr, "%s: Failed to allocate %dx%d pixmap.\n", PGX_IDENT,
	      width, height);
      pgx->pixmap = None;
    };
  };
  return;
}

/*.......................................................................
 * Change the appearance and position of specific graphical augmentations
 * to the cursor.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context.
 *  ci          int    The color index to use when drawing the cursor,
 *                     or -1 to select the current foreground color.
 *  type        int    A cursor type from:
 *                      PGX_NORM_CURSOR  - No augmentation will be drawn.
 *                      PGX_LINE_CURSOR  - Line cursor between rbeg and rend.
 *                      PGX_RECT_CURSOR  - Rectangular cursor with opposing
 *                                         vertices at rbeg and rend.
 *                      PGX_VLINE_CURSOR - Vertical line cursor at x=rbeg[0].
 *                      PGX_HLINE_CURSOR - Horizontal line cursor at y=rbeg[1].
 *                      PGX_CROSS_CURSOR - Cross-hair cursor at rbeg[0],rbeg.[1]
 *  warp        int    If true, the cursor will be warped to rbeg when it
 *                     first enters the window.
 *  rbeg      float *  The PGPLOT x,y device coordinates of the origin
 *                     of the cursor, as rbeg[0]=x and rbeg[1]=y.
 *                     This can be NULL for type==PGX_NORM_CURSOR.
 *  rend      float *  The PGPLOT x,y device coordinates of the end-point
 *                     of the cursor, as rend[0]=x and rend[1]=y. This
 *                     can be NULL for cursor types that only need rbeg.
 * Output:
 *  return      int    0 - OK.
 *                     1 - Error.
 */
#ifdef __STDC__
int pgx_set_cursor(PgxWin *pgx, int ci, int type, int warp,
		   float *rbeg, float *rend)
#else
int pgx_set_cursor(pgx, ci, type, warp, rbeg, rend)
     PgxWin *pgx; int ci; int type; int warp; float *rbeg; float *rend;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN)) {
    PgxState *state = pgx->state;
    state->cursor.type = PGX_NORM_CURSOR;
    state->cursor.warp = warp;
/*
 * Record the cursor coordinates according to cursor type.
 */
    switch(type) {
    case PGX_NORM_CURSOR:
      break;
    case PGX_LINE_CURSOR:
    case PGX_RECT_CURSOR:
    case PGX_YRNG_CURSOR:
    case PGX_XRNG_CURSOR:
      if(rbeg && rend) {
	state->cursor.type = type;
	pgx_xy_to_XPoint(pgx, rbeg, &state->cursor.vbeg);
	pgx_xy_to_XPoint(pgx, rend, &state->cursor.vend);
      };
      break;
    case PGX_HLINE_CURSOR:
    case PGX_VLINE_CURSOR:
    case PGX_CROSS_CURSOR:
      if(rbeg) {
	state->cursor.type = type;
	pgx_xy_to_XPoint(pgx, rbeg, &state->cursor.vbeg);
      };
      break;
    };
/*
 * Establish a color for drawing the cursor.
 */
    if(type != PGX_NORM_CURSOR) {
      unsigned long pixel;              /* The colormap pixel to draw with */
      if(ci < 0)
	pixel = state->gcv.foreground;  /* The current foreground color */
      else if(ci < pgx->color->ncol)
	pixel = pgx->color->pixel[ci];  /* The specified color index */
      else
	pixel = pgx->color->pixel[1];   /* Out-of range, so use color index 1 */
      XSetForeground(pgx->display, state->cursor.gc, pixel);
    };
    return pgx->bad_device!=0;
  };
  return 1;
}

/*.......................................................................
 * If the cursor is currently marked as having been drawn, redraw it to
 * account for damage from expose events or pgplot drawing operations
 * that have taken place since the cursor was last drawn.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context.
 *  rbeg      float *  The PGPLOT x,y device coordinates of the origin
 *                     of the cursor, as rbeg[0]=x and rbeg[1]=y.
 *                     This can be NULL for type==PGX_NORM_CURSOR.
 *  rend      float *  The PGPLOT x,y device coordinates of the end-point
 *                     of the cursor, as rend[0]=x and rend[1]=y. This
 *                     can be NULL for cursor types that only need rbeg.
 * Output:
 *  return      int    0 - OK.
 *                     1 - Error.
 */
#ifdef __STDC__
int pgx_refresh_cursor(PgxWin *pgx)
#else
int pgx_refresh_cursor(pgx)
     PgxWin *pgx;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN)) {
    PgxState *state = pgx->state;
    if(state->cursor.drawn && pgx_draw_cursor(pgx))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Augment the X cursor with non-destructive line graphics by drawing
 * the graphics directly to the X window instead of to the pixmap. THe
 * underlying graphics can then be completely restored from the pixmap.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context.
 * Output:
 *  return      int    0 - OK.
 *                     1 - Error.
 */
#ifdef __STDC__
int pgx_draw_cursor(PgxWin *pgx)
#else
int pgx_draw_cursor(pgx)
     PgxWin *pgx;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN | PGX_NEED_PIXMAP)) {
    PgxState *state = pgx->state;
    XWcursor *cursor = &state->cursor;
    int ymin, ymax;
    int xmin, xmax;
/*
 * Convert from pixmap coordinates to window coordinates.
 */
    XPoint vbeg;
    XPoint vend;
    pgx_pixmap_to_window(pgx, &cursor->vbeg, &vbeg);
    pgx_pixmap_to_window(pgx, &cursor->vend, &vend);
/*
 * Get the drawable limits of the window.
 */
    if(pgx->clip.doclip) {
      xmin = pgx->clip.xmin;
      xmax = pgx->clip.xmax;
      ymin = pgx->clip.ymin;
      ymax = pgx->clip.ymax;
    } else {
      xmin = 0;
      xmax = (int)state->geom.width - 1;
      ymin = 0;
      ymax = (int)state->geom.height - 1;
    };
/*
 * Record the fact that the cursor is being drawn.
 */
    cursor->drawn = 1;
/*
 * Draw the requested cursor augmentation graphics.
 */
    switch(cursor->type) {
    case PGX_NORM_CURSOR:
    default:
      break;
    case PGX_LINE_CURSOR:
      XDrawLine(pgx->display, pgx->window, cursor->gc,
		vbeg.x, vbeg.y,
		vend.x, vend.y);
      break;
    case PGX_RECT_CURSOR:  /* Draw a rectangle */
      {
	int x = vbeg.x<vend.x ? vbeg.x : vend.x;
	int y = vbeg.y<vend.y ? vbeg.y : vend.y;
	unsigned width = (unsigned int) abs(vbeg.x - vend.x);
	unsigned height = (unsigned int) abs(vbeg.y - vend.y);
	XDrawRectangle(pgx->display, pgx->window, cursor->gc, x, y,
		       width, height);
      };
      break;
    case PGX_YRNG_CURSOR:  /* Two horizontal lines */
      XDrawLine(pgx->display, pgx->window, cursor->gc, xmin, vend.y,
		xmax, vend.y);
      if(pgx->bad_device)
	return 1;
      XDrawLine(pgx->display, pgx->window, cursor->gc, xmin, vbeg.y,
		xmax, vbeg.y);
      break;
    case PGX_XRNG_CURSOR:  /* Two vertical lines */
      XDrawLine(pgx->display, pgx->window, cursor->gc, vend.x, ymin,
		vend.x, ymax);
      if(pgx->bad_device)
	return 1;
      XDrawLine(pgx->display, pgx->window, cursor->gc, vbeg.x, ymin,
		vbeg.x, ymax);
      break;
    case PGX_HLINE_CURSOR: /* One horizontal line through the cursor */
      XDrawLine(pgx->display, pgx->window, cursor->gc, xmin, vend.y,
		xmax, vend.y);
      break;
    case PGX_VLINE_CURSOR: /* One vertical line through the cursor */
      XDrawLine(pgx->display, pgx->window, cursor->gc, vend.x, ymin,
		vend.x, ymax);
      break;
    case PGX_CROSS_CURSOR: /* Cross hair */
      XDrawLine(pgx->display, pgx->window, cursor->gc, xmin, vend.y,
		xmax, vend.y);
      if(pgx->bad_device)
	return 1;
      XDrawLine(pgx->display, pgx->window, cursor->gc, vend.x, ymin,
		vend.x, ymax);
      break;
    };
    XFlush(pgx->display);
    return pgx->bad_device != 0;
  };
  return 1;
}

/*.......................................................................
 * Erase graphics drawn by pgx_draw_cursor() by restoring the damaged
 * region from the underlying pixmap.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context.
 * Output:
 *  return      int    0 - OK.
 *                     1 - Error.
 */
#ifdef __STDC__
int pgx_erase_cursor(PgxWin *pgx)
#else
int pgx_erase_cursor(pgx)
     PgxWin *pgx;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN | PGX_NEED_PIXMAP)) {
    PgxState *state = pgx->state;
    XWcursor *cursor = &state->cursor;
/*
 * Convert from pixmap coordinates to window coordinates.
 */
    XPoint vbeg;
    XPoint vend;
    pgx_pixmap_to_window(pgx, &cursor->vbeg, &vbeg);
    pgx_pixmap_to_window(pgx, &cursor->vend, &vend);
/*
 * Record the erasure.
 */
    cursor->drawn = 0;
/*
 * Erase the cursor.
 */
    switch(cursor->type) {
    case PGX_NORM_CURSOR:
    default:
      break;
    case PGX_LINE_CURSOR:   /* Line cursor */
      if(pgx_restore_line(pgx, vbeg.x, vbeg.y,
			      vend.x, vend.y))
	return 1;
      break;
    case PGX_RECT_CURSOR:   /* Rectangle cursor */
      if(pgx_restore_line(pgx, vbeg.x, vbeg.y,
			      vbeg.x, vend.y) ||
	 pgx_restore_line(pgx, vbeg.x, vend.y,
			      vend.x, vend.y) ||
	 pgx_restore_line(pgx, vend.x, vend.y,
			      vend.x, vbeg.y) ||
	 pgx_restore_line(pgx, vend.x, vbeg.y,
			      vbeg.x, vbeg.y))
	return 1;
      break;
    case PGX_YRNG_CURSOR:  /* Two horizontal lines */
      if(pgx_restore_line(pgx, 0, vend.y,
			      (int)state->geom.width-1, vend.y) ||
	 pgx_restore_line(pgx, 0, vbeg.y,
			      (int)state->geom.width-1,vbeg.y))
	return 1;
      break;
    case PGX_XRNG_CURSOR:  /* Two vertical lines */
      if(pgx_restore_line(pgx, vend.x, 0,
			      vend.x, (int)state->geom.height-1) ||
	 pgx_restore_line(pgx, vbeg.x, 0,
			      vbeg.x, (int)state->geom.height-1))
	return 1;
      break;
    case PGX_HLINE_CURSOR: /* One horizontal line through the cursor */
      if(pgx_restore_line(pgx, 0, vend.y,
			      (int)state->geom.width-1,vend.y))
	return 1;
      break;
    case PGX_VLINE_CURSOR: /* One vertical line through the cursor */
      if(pgx_restore_line(pgx, vend.x, 0,
			      vend.x, (int)state->geom.height-1))
	return 1;
      break;
    case PGX_CROSS_CURSOR: /* Cross hair */
      if(pgx_restore_line(pgx, 0, vend.y,
			      (int)state->geom.width-1, vend.y) ||
	 pgx_restore_line(pgx, vend.x, 0,
			      vend.x, (int)state->geom.height-1))
	return 1;
      break;
    };
    return pgx->bad_device != 0;
  };
  return 1;
}

/*.......................................................................
 * Restore the pixels under a given line.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context.
 *  xa, ya      int    The start pixel of the line (window coordinates).
 *  xb, yb      int    The end pixel of the line (window coordinates).
 * Output:
 *  return      int    0 - OK.
 *                     1 - Error.
 */
#ifdef __STDC__
static int pgx_restore_line(PgxWin *pgx, int xa, int ya, int xb, int yb)
#else
static int pgx_restore_line(pgx, xa, ya, xb, yb)
     PgxWin *pgx; int xa; int ya; int xb; int yb;
#endif
{
  int xlen = xb - xa;  /* X-axis displacement of line */
  int ylen = yb - ya;  /* Y-axis displacement of line */
  int xmin,xmax;       /* Min/max X-axis end points */
  int ymin,ymax;       /* Min/max Y-axis end points */
#define PIXINC 51
/*
 * Device error?
 */
  if(pgx->bad_device)
    return 1;
/*
 * Get sorted versions of xa and xb.
 */
  if(xlen > 0) {
    xmin = xa;
    xmax = xb;
  } else {
    xmin = xb;
    xmax = xa;
  };
/*
 * Get sorted versions of ya and yb.
 */
  if(ylen > 0) {
    ymin = ya;
    ymax = yb;
  } else {
    ymin = yb;
    ymax = ya;
  };
/*
 * Vertical line?
 */
  if(xlen==0) {
    pgx_copy_area(pgx, (int)(xmin + pgx->scroll.x), (int)(ymin + pgx->scroll.y),
		  (unsigned) 1, (unsigned) (ymax-ymin+1),
		  xmin, ymin);
  }
/*
 * Horizontal line?
 */
  else if(ylen==0) {
    pgx_copy_area(pgx, (int)(xmin + pgx->scroll.x), (int)(ymin + pgx->scroll.y),
		  (unsigned) (xmax-xmin+1), (unsigned) 1,
		  xmin, ymin);
  }
/*
 * Diagonal line encompasing fewer x-axis lines that y-axis lines?
 */
  else if(abs(xlen) <= abs(ylen)) {
    int x;       /* The X coordinate of the line of pixels being drawn */
    int y1,y2;   /* The start and end Y coordinates of the pixel line */
    double yperx = (double) ylen / (double) xlen;
    double yhalf = 0.5 * yperx;         /* Y-step over half a pixel */
    double ydelt = (PIXINC+0.5) * yperx; /* Y-step over PIXINC+0.5 pixels */
    double ylo = yperx > 0 ? yhalf : -ydelt;
    double yhi = yperx > 0 ? ydelt : -yhalf;
/*
 * Draw the block of pixels that encompases the line between X-axis
 * pixels the outer edges of pixels x -> x+PIXINC, for each consecutive
 * block of PIXINC pixels along X.
 */
    for(x=xmin; x <= xmax; x += PIXINC+1) {
      double ycent = ya + (x - xa) * yperx;
      y1 = (int)(ycent - ylo);    /* Note round-down semantics */
      y2 = (int)(ycent + yhi+0.5);/* Note round-up semantics */
      pgx_copy_area(pgx, (int)(x + pgx->scroll.x), (int)(y1 + pgx->scroll.y),
		    (unsigned) (PIXINC+1), (unsigned) (y2-y1+1),
		    x, y1);
    };
/*
 * Diagonal line encompasing fewer y-axis lines that x-axis lines?
 */
  } else {
    int y;       /* The Y coordinate of the line of pixels being drawn */
    int x1,x2;   /* The start and end X coordinates of the pixel line */
    double xpery = (double) xlen / (double) ylen;
    double xhalf = 0.5 * xpery;         /* X-step over half a pixel */
    double xdelt = (PIXINC+0.5) * xpery; /* X-step over PIXINC+0.5 pixels */
    double xlo = xpery > 0 ? xhalf : -xdelt;
    double xhi = xpery > 0 ? xdelt : -xhalf;
/*
 * Draw the block of pixels that encompases the line between Y-axis
 * pixels the outer edges of pixels y -> y+PIXINC, for each consecutive
 * block of PIXINC pixels along Y.
 */
    for(y=ymin; y <= ymax; y += PIXINC+1) {
      double xcent = xa + (y - ya) * xpery;
      x1 = (int)(xcent - xlo);    /* Note round-down semantics */
      x2 = (int)(xcent + xhi+0.5);/* Note round-up semantics */
      pgx_copy_area(pgx, (int)(x1 + pgx->scroll.x), (int)(y + pgx->scroll.y),
		    (unsigned) (x2-x1+1), (unsigned) (PIXINC+1),
		    x1, y);
    };
  };
/*
 * Check for device errors.
 */
  if(pgx->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * When the cursor is active, this function should be called whenever
 * one of the following event types are received.
 *
 *  ButtonPress, KeyPress, MotionNotify, EnterWindow, LeaveWindow.
 *
 * In addition, Expose events will be handled if presented. Other event
 * types are quietly ignored.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context.
 *  event    XEvent *  The X input event to be processed.
 * Input/Output:
 *  rbuf      float *  If event->type is ButtonPress, KeyPress or
 *                     MotionNotify and rbuf!=NULL then the device
 *                     coordinates of the input event will be encoded
 *                     in PGPLOT x,y device coordinates as rbuf[0]=x
 *                     and rbuf[1]=y.
 *  key        char *  If key!=NULL then:
 *                     1. If the event is a key-press or key-release
 *                        event and the key is a single-ascii
 *                        character then *key will contain that
 *                        character.
 *                     2. If the event is a button-press or
 *                        button-release event then the button will be
 *                        encoded as characters in *key as:
 *                         Button 1 => 'A',
 *                         Button 2 => 'D',
 *                         Button 3 => 'X'.
 *                     3. Otherwise *key='\0'.
 * Output:
 *  return      int    0 - No position selected.
 *                     1 - rbuf and key contain the latest cursor
 *                         selection details.
 */
#ifdef __STDC__
int pgx_cursor_event(PgxWin *pgx, XEvent *event, float *rbuf, char *key)
#else
int pgx_cursor_event(pgx, event, rbuf, key)
     PgxWin *pgx; XEvent *event; float *rbuf; char *key;
#endif
{
  char ret_key = '\0';   /* The key to be returned */
  XPoint coord;          /* Pointer coordinate */
  int have_posn = 0;     /* If true, a position has been encoded in coord */
/*
 * Cursor ready?
 */
  if(pgx_ready(pgx, PGX_NEED_PGOPEN | PGX_NEED_PIXMAP)) {
    PgxState *state = pgx->state;
    XWcursor *cursor = &state->cursor;
/*
 * Place the pointer position in coord.x,y.
 */
    switch(event->type) {
    case Expose:
      if(pgx_expose(pgx, event))
	return 0;
      break;
    case KeyPress:
      coord.x = event->xkey.x;
      coord.y = event->xkey.y;
/*
 * Get the ASCII encoding associated with the key.
 */
      {
	char buffer[10];   /* Buffer to read key definition into */
	KeySym keysym;     /* Key code of pressed keyboard key */
	int nret;          /* The number of characters returned in buffer[]*/
	nret = XLookupString(&event->xkey, buffer,
		          (int) (sizeof(buffer)/sizeof(char)), &keysym, NULL);
	if(pgx->bad_device)
	  return 0;
/*
 * Ignore modifier keys and all but single character keys.
 */
	if(nret==1 && (keysym < XK_Shift_L || keysym > XK_Hyper_R)) {
	  ret_key = buffer[0];
	  have_posn = 1;
	};
      };
      break;
    case ButtonPress:
      coord.x = event->xbutton.x;
      coord.y = event->xbutton.y;
      have_posn = 1;
      switch(event->xbutton.button) {
      case Button1:
	ret_key = 'A';
	break;
      case Button2:
	ret_key = 'D';
	break;
      default:
	ret_key = 'X';
	break;
      };
      break;
    case EnterNotify:
/*
 * The cursor may still be drawn if a button was pressed when the
 * cursor was last moved out of the window. The resulting
 * passive grab will have continued to deliver motion events to
 * the PGPLOT window.
 */
      if(pgx_erase_cursor(pgx))
	return 0;
/*
 * If the cursor is in the window, locate its position and record it
 * in pgx->state->cursor.vend. If this is the first time that the
 * cursor has been in the window and warping has been requested,
 * this also inolves pre-positioning the cursor.
 */
      if(pgx_locate_cursor(pgx)) {
/*
 * Draw the cursor if it isn't already drawn.
 */
	if(pgx->bad_device || pgx_draw_cursor(pgx))
	  return 0;
      };
      break;
    case LeaveNotify:
      if(pgx_erase_cursor(pgx))
	return 0;
      break;
    case MotionNotify:
/*
 * Discard all but the last MotionNotify event.
 */
      while(XCheckWindowEvent(pgx->display, pgx->window,
			      (long)(PointerMotionMask), event) == True);
      if(pgx->bad_device || pgx_erase_cursor(pgx))
	return 0;
/*
 * Erase the out-of-date cursor.
 */
      if(pgx_erase_cursor(pgx))
	return 0;
/*
 * Convert from window coordinates to pixmap coordinates.
 */
      cursor->vend.x = event->xmotion.x + pgx->scroll.x;
      cursor->vend.y = event->xmotion.y + pgx->scroll.y;
/*
 * Redraw the cursor at the new position.
 */
      if(pgx_draw_cursor(pgx))
	return 0;
      break;
    default:
      break;
    };
/*
 * Convert new pointer coordinates to pgplot device coordinates.
 */
    if(have_posn) {
/*
 * Convert from window coordinates to pixmap coordinates.
 */
      coord.x = coord.x + pgx->scroll.x;
      coord.y = coord.y + pgx->scroll.y;
/*
 * Convert to PGPLOT device coordinates.
 */
      if(rbuf)
	pgx_XPoint_to_xy(pgx, &coord, rbuf);
      if(key)
	*key = ret_key;
      return 1;
    };
  };
  return 0;
}

/*.......................................................................
 * Determine whether the cursor is within the plot window. If it is
 * and (cursor=&pgx->state->cursor) cursor->warp is true, warp the cursor
 * to cursor->vbeg then reset cursor->warp to 0.
 * Record the final position of the cursor in cursor->vend.
 *
 * Input:
 *  pgx      PgxWin *  The PGPLOT window context.
 * Output:
 *  return  int   0 - Cursor not in window.
 *                1 - Cursor is in window.
 */
#ifdef __STDC__
int pgx_locate_cursor(PgxWin *pgx)
#else
int pgx_locate_cursor(pgx)
     PgxWin *pgx;
#endif
{
  XPoint pointer;         /* Pointer coordinates */
  Window parent;          /* The parent window */
/*
 * The following are all for use with XQueryPointer().
 */
  Window p_child;         /* The child of pgx->window (None) */
  int p_win_x, p_win_y;   /* The pointer coordinates in pgx->window */
  int p_root_x, p_root_y; /* The pointer coordinates in the root window */
  Window p_root_win;      /* The root window containing the cursor */
  unsigned int p_mask;    /* Bit mask of button states etc.. */
/*
 * Device error?
 */
  if(pgx_ready(pgx, PGX_NEED_WINDOW | PGX_NEED_PGOPEN)) {
    PgxState *state = pgx->state;
    XWcursor *cursor = &state->cursor;
/*
 * Get the parent window.
 */
    parent = pgx_parent_window(pgx);
    if(parent == None)
      return 0;
/*
 * See if the pointer is currently in the PGPLOT window.
 */
    XQueryPointer(pgx->display, parent, &p_root_win, &p_child,
		    &p_root_x, &p_root_y, &p_win_x, &p_win_y, &p_mask);
    if(pgx->bad_device)
      return 0;
    if(p_child==pgx->window) {
/*
 * Determine the current position of the pointer within the PGPLOT window.
 */
      XQueryPointer(pgx->display, pgx->window, &p_root_win, &p_child,
		    &p_root_x, &p_root_y, &p_win_x, &p_win_y, &p_mask);
      if(pgx->bad_device)
	return 0;
/*
 * Record the pointer coordinates.
 */
      pointer.x = p_win_x;
      pointer.y = p_win_y;
/*
 * Warp the cursor?
 */
      if(cursor->warp) {
	XWindowAttributes attr; /* Current window attributes */
	XPoint warp_coord;      /* The window coordinates to warp to */
/*
 * Query the current state of the window.
 */
	XGetWindowAttributes(pgx->display, pgx->window, &attr);
	if(pgx->bad_device)
	  return 0;
/*
 * Convert the warp pixmap coordinates to window coordinates.
 */
	pgx_pixmap_to_window(pgx, &cursor->vbeg, &warp_coord);
/*
 * Disable subsequent warping.
 */
	cursor->warp = 0;
/*
 * Don't warp the cursor unless the warp target location is visible.
 */
	if(warp_coord.x < 0 || warp_coord.x >= attr.width ||
	   warp_coord.y < 0 || warp_coord.y >= attr.height) {
	  pgx_window_to_pixmap(pgx, &pointer, &cursor->vend);
	} else {
	  XWarpPointer(pgx->display, None, pgx->window, 0, 0, 0, 0,
		       warp_coord.x, warp_coord.y);

	  if(pgx->bad_device)
	    return 0;
/*
 * Record the new coordinates.
 */
	  pgx_window_to_pixmap(pgx, &warp_coord, &cursor->vend);
	};
/*
 * Return the current position of the cursor without warping.
 */
      } else {
	pgx_window_to_pixmap(pgx, &pointer, &cursor->vend);
      };
      return 1;  /* The pointer is in the window */
    };
  };
  return 0;    /* The pointer is not in the window */
}

/*.......................................................................
 * Install a new event mask. This function can be used to add to remove
 * from or replace the existing event mask. When adding to or replacing
 * the existing event mask, care is taken to check for the addition of
 * events that X only allows one client to select. If these can not
 * be selected, they will silently be removed from the event mask.
 * The pertinent masks are: ButtonPressMask, SubstructureRedirectMask and
 * ResizeRedirectMask.
 *
 * Input:
 *  pgx           PgxWin *  The PGPLOT window context.
 *  oper             int    The operation to be performed, from:
 *                           PGX_ADD_EVENTS - Form a union of the existing
 *                                            event mask and 'events'.
 *                           PGX_REM_EVENTS - Remove the events in 'events'
 *                                            from the existing event mask.
 *                           PGX_SET_EVENTS - Replace the existing event
 *                                            mask with 'events'.
 *  events unsigned long    The event mask to use as specified by 'oper'.
 * Output:
 *  return unsigned long    The old event mask.
 */
#ifdef __STDC__
unsigned long pgx_select_events(PgxWin *pgx, int oper, long events)
#else
unsigned long pgx_select_events(pgx, oper, events)
     PgxWin *pgx; int oper; long events;
#endif
{
  unsigned long incr_mask;    /* The events to be added to the old mask */
  unsigned long decr_mask;    /* The events to be removed from the old mask */
  unsigned long new_mask = 0; /* The new trial event mask */
  unsigned long old_mask = 0; /* The old event mask to be returned */
/*
 * We need a window to select events from.
 */
  if(pgx_ready(pgx, PGX_NEED_WINDOW)) {
    XWindowAttributes attr; /* Current window attributes */
/*
 * Get the current window attributes.
 */
    XGetWindowAttributes(pgx->display, pgx->window, &attr);
    if(pgx->bad_device)
      return old_mask;
/*
 * Record the existing event mask.
 */
    old_mask = attr.your_event_mask;
/*
 * Decompose the mask-update into a mask of events that need to be
 * added and a mask of events that need to be removed.
 */
    switch(oper) {
    case PGX_SET_EVENTS:
    default:
      incr_mask = ~old_mask & events;
      decr_mask = ~events & old_mask;
      break;
    case PGX_ADD_EVENTS:
      incr_mask = ~old_mask & (old_mask | events);
      decr_mask = 0;
      break;
    case PGX_REM_EVENTS:
      incr_mask = 0;
      decr_mask = events;
      break;
    };
/*
 * Form a new mask from the old mask by removing the events in decr_mask
 * but adding only events from the incremental mask that do not have the
 * potential to cause protocol errors.
 */
    new_mask = (old_mask & ~decr_mask) |
      (incr_mask & ~(unsigned long)(ButtonPressMask |
		     SubstructureRedirectMask | ResizeRedirectMask));
/*
 * If the incremental event mask contains events that can only be selected
 * by one client at a time, we must try each one, one at a time and
 * only add the event to the final mask if it can be accomodated.
 */
    if(incr_mask & ButtonPressMask) {
      pgx_start_error_watch(pgx);
      XSelectInput(pgx->display, pgx->window,
		   (long) (new_mask | ButtonPressMask));
      if(!pgx_end_error_watch(pgx))
	new_mask |= ButtonPressMask;
    };
    if(incr_mask & SubstructureRedirectMask) {
      pgx_start_error_watch(pgx);
      XSelectInput(pgx->display, pgx->window,
		   (long) (new_mask | SubstructureRedirectMask));
      if(!pgx_end_error_watch(pgx))
	new_mask |= SubstructureRedirectMask;
    };
    if(incr_mask & ResizeRedirectMask) {
      pgx_start_error_watch(pgx);
      XSelectInput(pgx->display, pgx->window,
		   (long)(new_mask | ResizeRedirectMask));
      if(!pgx_end_error_watch(pgx))
	new_mask |= ResizeRedirectMask;
    };
/*
 * Try to select the new mask.
 */
    pgx_start_error_watch(pgx);
    XSelectInput(pgx->display, pgx->window, (long) new_mask);
    if(pgx_end_error_watch(pgx)) {
      fprintf(stderr, "pgx_select_events: Error selecting events.\n");
      return old_mask;
    };
  };
  return old_mask;
}

/*.......................................................................
 * Perform a blocking cursor read to implement the read-cursor PGPLOT
 * driver opcode. The function will not return until the user presses a
 * pointer button or keyboard key within the window.
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 * Input/Output:
 *  rbuf    float *  The array of float arguments sent by the PGPLOT
 *                   GREXEC() subroutine.
 *                   On input:
 *                    rbuf[0] = Initial X position of cursor.
 *                    rbuf[1] = Initial Y position of cursor.
 *                    rbuf[2] = Reference X position for rubber-banding.
 *                    rbuf[3] = Reference Y position for rubber-banding.
 *                    rbuf[4] = Cursor banding mode as enumerated in
 *                              pgxwin.h as PGX_*_CURSOR.
 *                    rbuf[5] = Pre-position cursor if > 0.
 *                   On output:
 *                    rbuf[0] = X position of cursor.
 *                    rbuf[1] = Y position of cursor.
 *  chr      char *  On output *chr will be assigned the character
 *                   associated with the key or button that the user
 *                   typed.
 *  nbuf      int *  On output this will be set to 2 to tell PGPLOT that
 *                   two elements of rbuf are being returned.
 *  lchr      int *  On output this will be set to 1 to tell PGPLOT that
 *                   chr[] contains a single character.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Error.
 */
#ifdef __STDC__
int pgx_read_cursor(PgxWin *pgx, float *rbuf, char *chr, int *nbuf, int *lchr)
#else
int pgx_read_cursor(pgx, rbuf, chr, nbuf, lchr)
     PgxWin *pgx; float *rbuf; char *chr; int *nbuf; int *lchr;
#endif
{
  int waserr = 0;
/*
 * Preset predetermined return values.
 */
  if(nbuf)
    *nbuf = 2;
  if(lchr)
    *lchr = 1;
  if(chr)
    *chr = '\0';
/*
 * Read the cursor if possible.
 */
  if(pgx_ready(pgx, PGX_NEED_PGOPEN | PGX_NEED_PIXMAP)) {
    unsigned long old_mask;
/*
 * Raise the cursor.
 */
    if(pgx_set_cursor(pgx, -1, (int)(rbuf[4]+0.5), (int)(rbuf[5]+0.5)>0,
		      &rbuf[2], &rbuf[0]))
      return 1;
/*
 * Draw the cursor if it is in the window.
 */
    pgx_locate_cursor(pgx);
/*
 * Augment the event mask with the events that we need.
 */
    old_mask = pgx_select_events(pgx, PGX_ADD_EVENTS, (long)
		   (ExposureMask | KeyPressMask | ButtonPressMask |
		    EnterWindowMask | LeaveWindowMask |
		    PointerMotionMask));
/*
 * Handle the above events until the user selects a position by pressing
 * a mouse button or key.
 */
    waserr = waserr || pgx_handle_cursor(pgx, rbuf, chr);
/*
 * Remove any cursor augmentation.
 */
    waserr = waserr || pgx_erase_cursor(pgx) ||
      pgx_set_cursor(pgx, 0, PGX_NORM_CURSOR, 0, NULL, NULL);
/*
 * Reinstall the original event mask.
 */
    pgx_select_events(pgx, PGX_SET_EVENTS, (long) old_mask);
  };
  return waserr != 0;
}

/*.......................................................................
 * This is the private function of pgx_read_cursor() which maintains
 * the display of the cursor and returns when a valid keyboard or
 * button-press event has been received.
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 * Input/Output:
 *  rbuf    float *  The return position array.
 *  key      char *  The key that caused the cursor to be selected.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Error.
 */
#ifdef __STDC__
static int pgx_handle_cursor(PgxWin *pgx, float *rbuf, char *key)
#else
static int pgx_handle_cursor(pgx, rbuf, key)
     PgxWin *pgx; float *rbuf; char *key;
#endif
{
  XEvent event;
/*
 * Discard un-handled ButtonPress, KeyPress and MotionNotify events
 * without blocking.
 */
  while(XCheckWindowEvent(pgx->display, pgx->window, (long)
	      (ButtonPressMask | KeyPressMask | PointerMotionMask), &event))
    ;
  if(pgx->bad_device)
    return 1;
/*
 * Wait for further events.
 */
  while(!pgx->bad_device) {
    XWindowEvent(pgx->display, pgx->window, (long)
		 (ExposureMask | KeyPressMask | ButtonPressMask |
		  EnterWindowMask | LeaveWindowMask | PointerMotionMask),
		 &event);
    if(pgx->bad_device)
      return 1;
    if(pgx_cursor_event(pgx, &event, rbuf, key) && *key!='\0')
      return 0;
  };
  return 1;
}

/*.......................................................................
 * Limit pixmap coordinates to lie within the pixmap area.
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 * Input/Output:
 *  coord  XPoint *  The coordinates to be modified.
 */
#ifdef __STDC__
static void pgx_limit_pcoords(PgxWin *pgx, XPoint *coord)
#else
static void pgx_limit_pcoords(pgx, coord)
     PgxWin *pgx; XPoint *coord;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN)) {
    PgxState *state = pgx->state;
    if(coord->x < 0)
      coord->x = 0;
    if(coord->y < 0)
      coord->y = 0;
    if(coord->x >= state->geom.width)
      coord->x = state->geom.width - 1;
    if(coord->y >= state->geom.height)
      coord->y = state->geom.height - 1;
  };
  return;
}

/*.......................................................................
 * Limit window coordinates to lie within the drawable window area.
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 * Input/Output:
 *  coord  XPoint *  The coordinates to be modified.
 */
#ifdef __STDC__
static void pgx_limit_wcoords(PgxWin *pgx, XPoint *coord)
#else
static void pgx_limit_wcoords(pgx, coord)
     PgxWin *pgx; XPoint *coord;
#endif
{
  if(pgx_ready(pgx, 0) && pgx->clip.doclip) {
    if(coord->x >= pgx->clip.xmax)
      coord->x = pgx->clip.xmax;
    if(coord->y >= pgx->clip.ymax)
      coord->y = pgx->clip.ymax;
    if(coord->x < pgx->clip.xmin)
      coord->x = pgx->clip.xmin;
    if(coord->y < pgx->clip.ymin)
      coord->y = pgx->clip.ymin;
  };
  return;
}

/*.......................................................................
 * Convert from scrolled window coordinates to pixmap coordinates.
 * The pixmap coordinates will be limited to the bounds of the pixmap.
 *
 * Input:
 *  pgx     PgxWin *  The PGPLOT window context.
 *  w_coord XPoint *  The window coordinate to be converted.
 * Input/Output:
 *  p_coord XPoint *  The resulting pixmap coordinates.
 */
#ifdef __STDC__
static void pgx_window_to_pixmap(PgxWin *pgx, XPoint *w_coord, XPoint *p_coord)
#else
static void pgx_window_to_pixmap(pgx, w_coord, p_coord)
     PgxWin *pgx; XPoint *w_coord; XPoint *p_coord;
#endif
{
  if(pgx) {
    p_coord->x = w_coord->x + pgx->scroll.x;
    p_coord->y = w_coord->y + pgx->scroll.y;
  } else {
    p_coord->x = w_coord->x;
    p_coord->y = w_coord->y;
  };
  pgx_limit_pcoords(pgx, p_coord);
  return;
}

/*.......................................................................
 * Convert from pixmap coordinates to scrolled window coordinates.
 * The coordinates will be bounded to the size of the pixmap
 * before the conversion and bounded to the drawable area of the
 * pixmap after the conversion.
 *
 * Input:
 *  pgx     PgxWin *  The PGPLOT window context.
 *  p_coord XPoint *  The pixmap coordinate to be converted.
 * Input/Output:
 *  w_coord XPoint *  The resulting window coordinates.
 */
#ifdef __STDC__
static void pgx_pixmap_to_window(PgxWin *pgx, XPoint *p_coord, XPoint *w_coord)
#else
static void pgx_pixmap_to_window(pgx, p_coord, w_coord)
     PgxWin *pgx; XPoint *p_coord; XPoint *w_coord;
#endif
{
  if(pgx) {
    pgx_limit_pcoords(pgx, p_coord);
    w_coord->x = p_coord->x - pgx->scroll.x;
    w_coord->y = p_coord->y - pgx->scroll.y;
    pgx_limit_wcoords(pgx, w_coord);
  } else {
    w_coord->x = p_coord->x;
    w_coord->y = p_coord->y;
  };
  return;
}

/*.......................................................................
 * Create and initialize an empty PGPLOT window context descriptor
 * at least up to the point at which it can safely be passed to
 * del_PgxWin().
 *
 * Input:
 *  display  Display *   The display to associate with the window.
 *  screen       int     The screen to associate with the window.
 *  name        char *   A name to refer to the window by.
 *  resize_fn PgxResizeWindowFn The function to call when the window
 *                       needs resizing, or 0 if window resizing is to be
 *                       disallowed.
 *  pixmap_fn PgxNewPixmapFn The function to call to allocate a new
 *                       pixmap. If no special action is required,
 *                       send 0 and pgx_new_pixmap will be substituted.
 * Output:
 *  return    PgxWin *   The new container ready to be filled, or NULL
 *                       on error.
 */
#ifdef __STDC__
PgxWin *new_PgxWin(Display *display, int screen, void *context, char *name,
		   PgxResizeWindowFn resize_fn, PgxNewPixmapFn pixmap_fn)
#else
PgxWin *new_PgxWin(display, screen, context, name, resize_fn, pixmap_fn)
     Display *display; int screen; void *context; char *name;
     PgxResizeWindowFn resize_fn; PgxNewPixmapFn pixmap_fn;
#endif		   
{
  PgxWin *pgx;  /* The new descriptor */
/*
 * Check arguments.
 */
  if(!display) {
    fprintf(stderr, "new_PgxWin: NULL Display intercepted.\n");
    return NULL;
  };
/*
 * Allocate the container.
 */
  pgx = (PgxWin *) malloc(sizeof(PgxWin));
  if(!pgx) {
    fprintf(stderr, "new_PgxWin: Insufficient memory for new PGPLOT window.\n");
    return NULL;
  };
/*
 * Before attemping anything that might fail, initialize the container
 * at least up to the point at which it can safely be passed to
 * del_PgxWin().
 */
  pgx->context = context;
  pgx->display = display;
  pgx->screen = screen;
  pgx->window = None;
  pgx->pixmap = None;
  pgx->expose_gc = NULL;
  pgx->bad_device = 0;
  pgx->name = NULL;
  pgx->xmargin = 0;
  pgx->ymargin = 0;
  pgx->color = NULL;
  pgx->clip.doclip = 0;
  pgx->clip.xmin = pgx->clip.xmax = 0;
  pgx->clip.ymin = pgx->clip.ymax = 0;
  pgx->resize_fn = resize_fn;
  pgx->new_pixmap_fn = pixmap_fn ? pixmap_fn : pgx_new_pixmap;
  pgx->old_handler = 0;
  pgx->state = NULL;
/*
 * Allocate a copy of the specified name.
 */
  if(!name)
    name = "pgxwin";
  pgx->name = (char *) malloc(strlen(name) + 1);
  if(!pgx->name) {
    fprintf(stderr, "new_PgxWin: Insufficient memory to name window.\n");
    return del_PgxWin(pgx);
  };
  strcpy(pgx->name, name);
/*
 * The rest of the initialization is driver-specific.
 */
  return pgx;
}

/*.......................................................................
 * Delete a PgxWin PGPLOT window context container and its contents.
 * Note that it is the responsibility of the caller to keep a record of
 * and delete the window pgx->window and close the display pgx->display
 * when appropriate.
 *
 * Input:
 *  pgx    PgxWin *  The container to be deleted.
 * Output:
 *  return PgxWin *  The deleted container (Always NULL).
 */
#ifdef __STDC__
PgxWin *del_PgxWin(PgxWin *pgx)
#else
PgxWin *del_PgxWin(pgx)
     PgxWin *pgx;
#endif
{
  if(pgx) {
/*
 * Make sure that the window is closed to PGPLOT.
 * This deletes pgx->state and its contents.
 */
    pgx_close(pgx);
/*
 * Destroy the pixmap.
 */
    if(pgx->display && pgx->pixmap != None)
      XFreePixmap(pgx->display, pgx->pixmap);
/*
 * Discard the graphical context.
 */
    if(pgx->display && pgx->expose_gc)
      XFreeGC(pgx->display, pgx->expose_gc);
/*
 * Delete the colormap/visual context.
 */
    pgx->color = del_PgxColor(pgx, pgx->color);
/*
 * Release the memory taken by the window-name string.
 */
    if(pgx->name)
      free(pgx->name);
/*
 * Just in case an application continues to use this descriptor after
 * it has been free()'d mark it as bad. This isn't foolproof since the
 * next malloc() will probably reuse this memory, but anything that
 * might help track down such a lethal problem is worth doing.
 */
    pgx->bad_device = 1;
/*
 * Finally, free the container.
 */
    free(pgx);
  };
  return NULL;
}

/*.......................................................................
 * When copying the off-screen pixmap to the window this function
 * should be used in place of XCopyArea() [which it calls]. This function
 * clips the copied area to the dimensions of the window clipping
 * area in pgx->clip.
 *
 * Input:
 *  px,py     int    The top-left corner of the area to be copied
 *                   from the pixmap.
 *  w,h  unsigned    The width and height of the area to be copied.
 *  wx,wy     int    The top-left corner of the destination area
 *                   in the window.
 */
#ifdef __STDC__
static int pgx_copy_area(PgxWin *pgx, int px, int py, unsigned w, unsigned h,
			 int wx, int wy)
#else
static int pgx_copy_area(pgx, px, py, w, h, wx, wy)
     PgxWin *pgx; int px; int py; unsigned w; unsigned h;
     int wx; int wy;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_WINDOW | PGX_NEED_PIXMAP)) {
/*
 * Limit the copied area to the drawable area of the window as listed
 * in pgx->clip.
 *
 * First clip the destination X-axis extent to between pgx->clip.xmin and
 * pgx->clip.xmax and adjust the source coordinates to suit.
 */
    if(pgx->clip.doclip) {
      long height = h;   /* Signed version of h */
      long width = w;    /* Signed version of w */
      if(wx < pgx->clip.xmin) {
	int xdiff = pgx->clip.xmin - wx;
	wx += xdiff;
	px += xdiff;
	width -= xdiff;
      };
      if(wx + width - 1 > pgx->clip.xmax)
	width = pgx->clip.xmax - wx + 1;
/*
 * Now clip the destination Y-axis extent to between pgx->clip.ymin and
 * pgx->clip.ymax and adjust the source coordinates to suit.
 */
      if(wy < pgx->clip.ymin) {
	int ydiff = pgx->clip.ymin - wy;
	wy += ydiff;
	py += ydiff;
	height -= ydiff;
      };
      if(wy + height - 1 > pgx->clip.ymax)
	height = pgx->clip.ymax - wy + 1;
/*
 * Nothing visible?
 */
      if(width <= 0 || height <= 0)
	return 0;
      w = width;
      h = height;
    };
/*
 * Perform the requested copy.
 */
    XCopyArea(pgx->display, pgx->pixmap, pgx->window, pgx->expose_gc,
	      px, py, w, h, wx, wy);
  };
  return 0;
}

/*.......................................................................
 * When clearing a region of the window this function should be used in
 * place of XClearArea() [which it calls]. This function clips the copied
 * area to the dimensions of the window clipping area in pgx->clip.
 *
 * Input:
 *  x,y       int    The top-left corner of the area to be cleared
 *                   in the window (window pixel coordinates).
 *  w,h  unsigned    The width and height of the area to be cleared.
 */
#ifdef __STDC__
static int pgx_clear_area(PgxWin *pgx, int x, int y, unsigned w, unsigned h)
#else
static int pgx_clear_area(pgx, x, y, w, h)
     PgxWin *pgx; int x; int y; unsigned w; unsigned h;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_WINDOW)) {
/*
 * Limit the cleared area to the drawable area of the window as listed
 * in pgx->clip.
 *
 * First clip the destination X-axis extent to between pgx->clip.xmin and
 * pgx->clip.xmax.
 */
    if(pgx->clip.doclip) {
      long height = h;   /* Signed version of h */
      long width = w;    /* Signed version of w */
      if(x < pgx->clip.xmin) {
	int xdiff = pgx->clip.xmin - x;
	x += xdiff;
	width -= xdiff;
      };
      if(x + width - 1 > pgx->clip.xmax)
	width = pgx->clip.xmax - x + 1;
/*
 * Now clip the destination Y-axis extent to between pgx->clip.ymin and
 * pgx->clip.ymax and adjust the source coordinates to suit.
 */
      if(y < pgx->clip.ymin) {
	int ydiff = pgx->clip.ymin - y;
	y += ydiff;
	height -= ydiff;
      };
      if(y + height - 1 > pgx->clip.ymax)
	height = pgx->clip.ymax - y + 1;
/*
 * Nothing visible?
 */
      if(width <= 0 || height <= 0)
	return 0;
      w = width;
      h = height;
    };
/*
 * Perform the requested clear.
 */
    XClearArea(pgx->display, pgx->window, x, y, w, h, False);
  };
  return 0;
}

/*.......................................................................
 * Change the background color of a window.
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 *  xc     XColor *  The new background color.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Error.
 */
#ifdef __STDC__
int pgx_set_background(PgxWin *pgx, XColor *xc)
#else
int pgx_set_background(pgx, xc)
     PgxWin *pgx; XColor *xc;
#endif
{
  if(!xc) {
    fprintf(stderr, "%s: pgx_set_background: NULL xc argument.", PGX_IDENT);
    return 1;
  };
  if(pgx_ready(pgx, PGX_NEED_COLOR)) {
    float r = (float) xc->red / PGX_COLORMULT;
    float g = (float) xc->green / PGX_COLORMULT;
    float b = (float) xc->blue / PGX_COLORMULT;
/*
 * Register the desired color in pgx->color->xcolor[0].
 */
    if(pgx_set_rgb(pgx, 0, r, g, b))
      return 1;
/*
 * Flush the changed color to the X server.
 * If the allocated color cells are readonly, defer the update until
 * the next page.
 */
    if((!pgx->color->readonly && pgx->state) ?
       pgx_update_colors(pgx) : pgx_flush_colors(pgx, 0, 1))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Change the foreground color of a window.
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 *  xc     XColor *  The new foreground color.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Error.
 */
#ifdef __STDC__
int pgx_set_foreground(PgxWin *pgx, XColor *xc)
#else
int pgx_set_foreground(pgx, xc)
     PgxWin *pgx; XColor *xc;
#endif
{
  if(!xc) {
    fprintf(stderr, "%s: pgx_set_foreground: NULL xc argument.", PGX_IDENT);
    return 1;
  };
  if(pgx_ready(pgx, PGX_NEED_COLOR)) {
    float r = (float) xc->red / PGX_COLORMULT;
    float g = (float) xc->green / PGX_COLORMULT;
    float b = (float) xc->blue / PGX_COLORMULT;
/*
 * Register the desired color in pgx->color->xcolor[0].
 */
    if(pgx_set_rgb(pgx, 1, r, g, b))
      return 1;
/*
 * Flush the changed color to the X server.
 * If the allocated color cells are readonly, defer the update until
 * the next page.
 */
    if((!pgx->color->readonly && pgx->state) ?
       pgx_update_colors(pgx) : pgx_flush_colors(pgx, 1, 1))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Return the width of the current pixmap.
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 * Output:
 *  unsigned  int    The width of the pixmap, or 0 if no pixmap currently
 *                   exists.
 */
#ifdef __STDC__
unsigned pgx_pixmap_width(PgxWin *pgx)
#else
unsigned pgx_pixmap_width(pgx)
     PgxWin *pgx;
#endif
{
  if(!pgx_ready(pgx, PGX_NEED_PGOPEN | PGX_NEED_PIXMAP))
    return 0;
  return pgx->state->geom.width;
}

/*.......................................................................
 * Return the height of the current pixmap.
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 * Output:
 *  unsigned  int    The height of the pixmap, or 0 if no pixmap currently
 *                   exists.
 */
#ifdef __STDC__
unsigned pgx_pixmap_height(PgxWin *pgx)
#else
unsigned pgx_pixmap_height(pgx)
     PgxWin *pgx;
#endif
{
  if(!pgx_ready(pgx, PGX_NEED_PGOPEN | PGX_NEED_PIXMAP))
    return 0;
  return pgx->state->geom.height;
}

/*.......................................................................
 * Convert from window pixel coordinates to PGPLOT device coordinates.
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 *  x,y       int    The X window pixel coordinates.
 * Input/Output:
 *  rbuf    float *  The return array for the X and Y device coordinates.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Error.
 */
#ifdef __STDC__
int pgx_win2dev(PgxWin *pgx, int x, int y, float *rbuf)
#else
int pgx_win2dev(pgx, x, y, rbuf)
     PgxWin *pgx; int x; int y; float *rbuf;
#endif
{
  if(!rbuf) {
    fprintf(stderr, "pgx_win2dev: NULL rbuf[].\n");
    return 1;
  };
  if(pgx_ready(pgx, PGX_NEED_PGOPEN)) {
    XPoint coord;
/*
 * Convert from window coordinates to pixmap coordinates.
 */
    coord.x = x + pgx->scroll.x;
    coord.y = y + pgx->scroll.y;
/*
 * Convert to PGPLOT device coordinates.
 */
    pgx_XPoint_to_xy(pgx, &coord, rbuf);
  } else {
    rbuf[0] = rbuf[1] = 0.0;
  };
  return 0;
}

/*.......................................................................
 * Convert from PGPLOT device coordinates to X window coordinates.
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 *  rbuf    float *  The PGPLOT X and Y device coordinates.
 *                    rbuf[0] = X device coordinate.
 *                    rbuf[1] = Y device coordinate.
 * Input/Output:
 *  x,y       int *  The corresponding X window coordinates.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Error.
 */
#ifdef __STDC__
int pgx_dev2win(PgxWin *pgx, float *rbuf, int *x, int *y)
#else
int pgx_dev2win(pgx, rbuf, x, y)
     PgxWin *pgx; float *rbuf; int *x; int *y;
#endif
{
  if(!rbuf || !x || !y) {
    fprintf(stderr, "pgx_dev2win: NULL %s.\n",
	    !rbuf ? "rbuf[]" : (!x ? "x":"y"));
    return 1;
  };
  if(pgx_ready(pgx, PGX_NEED_PGOPEN)) {
    XPoint coord;
/*
 * Convert from PGPLOT device coordinates to pixmap coordinates.
 */
    pgx_xy_to_XPoint(pgx, rbuf, &coord);
/*
 * Convert from pixmap coordinates to window coordinates.
 */
    *x = coord.x - pgx->scroll.x;
    *y = coord.y - pgx->scroll.y;
  } else {
    *x = *y = 0.0;
  };
  return 0;
}

/*.......................................................................
 * Convert from PGPLOT device coordinates to PGPLOT world coordinates.
 *
 * Note that use of this function requires that opcode 27 be enabled and
 * set up to update the world-coordinate scaling via pgx_set_world().
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 * Input/Output:
 *  rbuf    float *  On input this should contain the PGPLOT device
 *                   x and y coordinates. On output it will contain the
 *                   corresponding world coordinates.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Error.
 */
#ifdef __STDC__
int pgx_dev2world(PgxWin *pgx, float *rbuf)
#else
int pgx_dev2world(pgx, rbuf)
     PgxWin *pgx; float *rbuf;
#endif
{
  if(!rbuf) {
    fprintf(stderr, "pgx_dev2world: NULL rbuf[].\n");
    return 1;
  };
  if(pgx_ready(pgx, PGX_NEED_PGOPEN)) {
    XWworld *world = &pgx->state->world;
/*
 * Convert device coordinates to world coordinates.
 */
    rbuf[0] = (rbuf[0] - world->xoff) / world->xdiv;
    rbuf[1] = (rbuf[1] - world->yoff) / world->ydiv;
  } else {
    rbuf[0] = rbuf[1] = 0.0;
  };
  return 0;
}

/*.......................................................................
 * Convert from PGPLOT world coordinates to PGPLOT device coordinates.
 *
 * Note that use of this function requires that opcode 27 be enabled and
 * set up to update the world-coordinate scaling via pgx_set_world().
 *
 * Input:
 *  pgx    PgxWin *  The PGPLOT window context.
 * Input/Output:
 *  rbuf    float *  On input this should contain the PGPLOT world
 *                   x and y coordinates. On output it will contain the
 *                   corresponding PGPLOT device coordinates.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Error.
 */
#ifdef __STDC__
int pgx_world2dev(PgxWin *pgx, float *rbuf)
#else
int pgx_world2dev(pgx, rbuf)
     PgxWin *pgx; float *rbuf;
#endif
{
  if(!rbuf) {
    fprintf(stderr, "pgx_world2dev: NULL rbuf[].\n");
    return 1;
  };
  if(pgx_ready(pgx, PGX_NEED_PGOPEN)) {
    XWworld *world = &pgx->state->world;
/*
 * Convert world coordinates to device coordinates.
 */
    rbuf[0] = rbuf[0] * world->xdiv + world->xoff;
    rbuf[1] = rbuf[1] * world->ydiv + world->yoff;
  } else {
    rbuf[0] = rbuf[1] = 0.0;
  };
  return 0;
}

/*.......................................................................
 * Scroll a rectangular area vertically and/or horizontally.
 *
 * Input:
 *  pgx     PgxWin *  The PGPLOT window context.
 *  rbuf     float *  The array of float arguments sent by the PGPLOT
 *                    GREXEC() subroutine.
 */
#ifdef __STDC__
void pgx_scroll_rect(PgxWin *pgx, float *rbuf)
#else
void pgx_scroll_rect(pgx, rbuf)
     PgxWin *pgx; float *rbuf;
#endif
{
  if(pgx_ready(pgx, PGX_NEED_PGOPEN | PGX_NEED_PIXMAP | PGX_NEED_COLOR)) {
    XPoint blc, trc;     /* The bottom left and top right rectangle corners */
    XPoint blc_orig, trc_orig; /* The vertices of the rectangle to be copied */
    XPoint blc_dest, trc_dest; /* The vertices of the destination of the copy */
    int dx, dy;                /* The amounts to scroll right and down */
/*
 * Convert the rectangle vertices from PGPLOT coordinates to X coordinates.
 */
    pgx_xy_to_XPoint(pgx, &rbuf[0], &blc);
    pgx_xy_to_XPoint(pgx, &rbuf[2], &trc);
/*
 * Get the scroll offsets in X coordinates.
 */
    dx = pgx_nint(rbuf[4]);
    dy = pgx_nint(-rbuf[5]);
/*
 * Selected parts of the pixmap will need to be erased by drawing an
 * opaque rectangle over them in the background color. Establish
 * the background color in the exposure graphical context to avoid
 * changing the current foreground color.
 */
    XSetForeground(pgx->display, pgx->expose_gc, pgx->color->pixel[0]);
/*
 * If either scroll extent exceeds the length of the associated
 * axis, then fill the area with the background color.
 */
    if(abs(dx) > trc.x - blc.x || abs(dy) > blc.y - trc.y) {
/*
 * Fill the rectangle in the pixmap.
 */
      XFillRectangle(pgx->display, pgx->pixmap, pgx->expose_gc, blc.x, trc.y,
		     (unsigned)(trc.x-blc.x+1), (unsigned)(blc.y-trc.y+1));
    } else {
/*
 * Calculate the vertices of the source and destination rectangles to
 * be copied.
 */
      blc_orig = blc_dest = blc;
      trc_orig = trc_dest = trc;
      if(dx > 0) {
	trc_orig.x = trc.x - dx;
	blc_dest.x = blc.x + dx;
      } else if(dx < 0) {
	blc_orig.x = blc.x - dx;
	trc_dest.x = trc.x + dx;
      };
      if(dy > 0) {
	blc_orig.y = blc.y - dy;
	trc_dest.y = trc.y + dy;
      } else if(dy < 0) {
	trc_orig.y = trc.y - dy;
	blc_dest.y = blc.y + dy;
      };
/*
 * Constrain the coordinates to lie within the pixmap.
 */
      pgx_limit_pcoords(pgx, &blc_orig);
      pgx_limit_pcoords(pgx, &blc_dest);
      pgx_limit_pcoords(pgx, &trc_orig);
      pgx_limit_pcoords(pgx, &trc_dest);
/*
 * Scroll the rectangle to its shifted location.
 */
      XCopyArea(pgx->display, pgx->pixmap, pgx->pixmap, pgx->expose_gc,
		blc_orig.x, trc_orig.y,
		trc_orig.x - blc_orig.x + 1,
		blc_orig.y - trc_orig.y + 1,
		blc_dest.x, trc_dest.y);
/*
 * Clear the vacated area to the left or right of the copied area.
 */
      if(dx > 0) {
	XFillRectangle(pgx->display, pgx->pixmap, pgx->expose_gc,
		       blc.x, trc.y,
		       (unsigned) dx,
		       (unsigned) (blc.y - trc.y + 1));
      } else if(dx < 0) {
	XFillRectangle(pgx->display, pgx->pixmap, pgx->expose_gc,
		       trc_dest.x, trc.y,
		       (unsigned) (-dx),
		       (unsigned) (blc.y - trc.y + 1));
      };
/*
 * Clear the vacated area above or below the copied area.
 */
      if(dy > 0) {
	XFillRectangle(pgx->display, pgx->pixmap, pgx->expose_gc,
		       blc.x, trc.y,
		       (unsigned) (trc.x - blc.x + 1),
		       (unsigned) dy);
      } else if(dy < 0) {
	XFillRectangle(pgx->display, pgx->pixmap, pgx->expose_gc,
		       blc.x, blc_dest.y,
		       (unsigned) (trc.x - blc.x + 1),
		       (unsigned) (-dy));
      };
    };
/*
 * Record the extent of the modified part of the pixmap.
 */
    pgx_mark_modified(pgx, blc.x, blc.y, 1);
    pgx_mark_modified(pgx, trc.x, trc.y, 1);
  };
  return;
}

/*.......................................................................
 * Return the nearest integer to a given floating point number.
 *
 * Input:
 *  f    float   The floating point number to be rounded.
 * Output:
 *  return int   The nearest integer to f.
 */
#ifdef __STDC__
static int pgx_nint(float f)
#else
static int pgx_nint(f)
     float f;
#endif
{
  return (int) (f >= 0.0 ? (f + 0.5) : (f - 0.5));
}

/*.......................................................................
 * Find the parent window of a PGPLOT window. Note that we can't cache
 * this because reparenting window managers change the parent of
 * top-level windows.
 *
 * Input:
 *  pgx     PgxWin *  The PGPLOT window context.
 * Output:
 *  return  Window    The parent window, or None on error.
 */
#ifdef __STDC__
Window pgx_parent_window(PgxWin *pgx)
#else
Window pgx_parent_window(pgx)
     PgxWin *pgx;
#endif
{
  Window root;
  Window parent;
  Window *children;
  unsigned int nchildren;
  if(!XQueryTree(pgx->display, pgx->window, &root, &parent,
		 &children, &nchildren)) {
    fprintf(stderr, "pgx_parent_window: XQueryTree failed.\n");
    pgx->bad_device = 1;
    return None;
  };
/*
 * Discard the unwanted list of children.
 */
  XFree(children);
  return parent;
}

/*.......................................................................
 * Replace an existing set of readonly colors with new color representations.
 *
 * Input:
 *  pgx           PgxWin *  The PGPLOT window context.
 *  ncol             int    The number of colors to redefine.
 * Input/Output:
 *  colors        XColor *  On input pass the array of ncol color
 *                          representations to allocate. On output this
 *                          will contain the newly allocated colors.
 *  pixels unsigned long *  The array of pixels corresponding to colors.
 *                          On calls when pgx->color->initialized is true
 *                          this should contain the pixels that are to be
 *                          replaced. On output it will contain the
 *                          allocated pixels.
 * Output: 
 *  return           int    0 - OK.
 *                          1 - Error.
 */
#ifdef __STDC__
static int pgx_readonly_colors(PgxWin *pgx, int ncol, XColor *colors,
			     unsigned long *pixels)
#else
static int pgx_readonly_colors(pgx, ncol, colors, pixels)
     PgxWin *pgx; int ncol; XColor *colors; unsigned long *pixels;
#endif
{
  int ngot=0;    /* The number of colors acquired */
  int i;
/*
 * Get aliases to objects in pgx.
 */
  Colormap cmap = pgx->color->cmap;
  Display *display = pgx->display;
/*
 * No colors required?
 */
  if(ncol < 1)
    return 0;
/*
 * First discard all but the first of the colors that we are replacing.
 * The first will be used as a fallback if the first pgx->color->nwork
 * entries of the colormap contain no shared colors.
 */
  if(pgx->color->initialized)
    XFreeColors(display, cmap, pixels+1, ncol-1, (long)0);
/*
 * First ask for precise versions of the requested colors. If the
 * colormap is readonly, the X server will actually return the
 * nearest approximation. If the colormap is read/write XAllocColor
 * is documented to fail up if it can't find exactly the color that
 * one asks for (within the color resolution of the hardware)!
 * Mark those that can't be allocated by setting their flags to 0.
 */
  for(i=0; i<ncol; i++) {
    XColor *c = colors + i;
    if(XAllocColor(display, cmap, c))
      ngot++;
    else
      c->flags = 0;
  };
/*
 * If we failed to allocate precise versions of any of the colors,
 * try to allocate approximate versions of the colors. Note that
 * the X server is supposed to do this for us if the colormap is
 * readonly, but not if it is read/write.
 */
  if(ngot < ncol) {
    XColor *last=NULL; /* The last color looked at while allocating colors */
    int napprox=0;     /* The number of approximate colors to choose from */
/*
 * Get the work array and its size.
 */
    XColor *work = pgx->color->work;
    int nwork = pgx->color->nwork;
/*
 * Ask the server for the first nwork colors in its colormap.
 */
    for(i=0; i<nwork; i++)
      work[i].pixel = i;
    XQueryColors(display, cmap, work, nwork);
/*
 * Sort the colors into ascending order of red,green,blue intensities.
 */
    qsort(work, nwork, sizeof(work[0]), pgx_cmp_xcolor);
/*
 * Attempt to allocate as many of the reported colors as possible,
 * being careful to ignore duplicate entries. Copy the napprox
 * allocated ones to the start of the work array.
 */
    for(i=0; i<nwork; i++) {
      XColor *c = work + i;
      if((!last || c->red != last->red || c->green != last->green ||
	  c->blue != last->blue)) {
	if(XAllocColor(display, cmap, c)) {
	  if(i != napprox)
	    work[napprox] = *c;
	  napprox++;
	};
	last = c;
      };
    };
/*
 * Fill in the outstanding PGPLOT colors from those remaining in the work
 * array. Note that we must also re-allocate each of the chosen colors
 * so that it can be free'd as many times as it appears in our color table.
 */
    for(i=0; i < ncol; i++) {
      XColor *c = colors + i;
      if(!c->flags) {
	if(pgx_nearest_color(work, napprox, c) ||
	   !XAllocColor(display, cmap, c))
	  break;
	else
	  ngot++;
      };
    };
/*
 * Release the work array of colors (note that the colors that were
 * chosen above were duplicately allocated, and X uses a reference
 * counting scheme for colormap entries, so this won't invalidate them).
 */
    for(i=0; i<napprox; i++) {
      XColor *c = work + i;
      XFreeColors(display, cmap, &c->pixel, 1, 0);
    };
  };
/*
 * Did we fail to get all of the required colors?
 */
  if(ngot < ncol) {
/*
 * If we were (re-)allocating the whole colortable, discard all colors
 * allocated above the first that we failed to get and reduce ncol to
 * account for this.
 */
    if(pixels==pgx->color->pixel && ncol >= pgx->color->npixel) {
      for(ngot=0; ngot<ncol && colors[ngot].flags; ngot++)
	;
      for(i=ngot; i<ncol; i++)
	XFreeColors(display, cmap, &colors[i].pixel, 1, 0);
/*
 * If there are no colors at all, then there isn't anything that
 * we can do. Note that if we are allocating from the default
 * colormap of the screen, BlackPixel() and WhitePixel() are
 * guaranteed to be available and shareable, so it is only
 * private colormaps that should suffer this problem. In private
 * colormaps there is no equivalent of BlackPixel() or WhitePixel(),
 * so we have nothing to fall back on.
 */
      if(ngot < 2) {
	for(i=0; i<ngot; i++)
	  XFreeColors(display, cmap, &colors[i].pixel, 1, 0);
	fprintf(stderr, "%s: There aren't any colors available.\n", PGX_IDENT);
	return 1;
      };
/*
 * Record the dimensions of the resized color table.
 */
      pgx->color->npixel = ngot;
      pgx->color->ncol = ngot;
/*
 * If we were replacing entries, then the size of the color table has already
 * been fixed, so we need to fill in the missing colors. Since we may not have
 * managed to allocate any colors, find out the color representation of the
 * first of the colors that we were replacing and reallocate for each of
 * the missing slots. Note that we were careful not to free this color
 * above, so it is supposedly guaranteed to still be allocatable as a
 * shared color.
 */
    } else {
/*
 * Get the color representation of the fallback color.
 */
      XColor fallback;   /* The color being used as a fallback */
      fallback.pixel = pixels[0];
      XQueryColor(display, cmap, &fallback);
/*
 * Re-allocate the fallback entry for each missing color.
 */
      for(i=0; i<ncol; i++) {
	XColor *c = colors + i;
	if(!c->flags) {
	  (void) XAllocColor(display, cmap, &fallback);
	  *c = fallback;
	};
      };
/*
 * We now have the requested number of colors, albeit with
 * unexpected colors.
 */
      ngot = ncol;
    };
  };
/*
 * Release the unfree'd first entry of the replaced colors.
 */
  if(pgx->color->initialized)
    XFreeColors(display, cmap, pixels, 1, 0);
/*
 * Record the new pixel indexes.
 */
  for(i=0; i<ngot; i++)
    pixels[i] = colors[i].pixel;
  return 0;
}

/*.......................................................................
 * This is a qsort comparison function used to sort an array XColor
 * entries into ascending order. The colors are sorted into ascending
 * order using red as the primary sort key, green as the second
 * sort key and blue as the third sort key.
 */
#ifdef __STDC__
static int pgx_cmp_xcolor(const void *va, const void *vb)
#else
static int pgx_cmp_xcolor(va, vb)
     char *va; char *vb;
#endif
{
  XColor *ca = (XColor *) va;
  XColor *cb = (XColor *) vb;
/*
 * Compare the red components.
 */
  if(ca->red < cb->red)
    return -1;
  else if(ca->red > cb->red)
    return 1;
/*
 * The red components are the same, so compare the green components.
 */
  if(ca->green < cb->green)
    return -1;
  else if(ca->green > cb->green)
    return 1;
/*
 * Both the red components and the green components are the same,
 * so compare the blue components.
 */
  if(ca->blue < cb->blue)
    return -1;
  else if(ca->blue > cb->blue)
    return 1;
/*
 * The colors are identical.
 */
  return 0;
}

/*.......................................................................
 * Find the nearest color in colors[] and assign it to *c.
 *
 * Input:
 *  colors  XColor *  An array of ncol colors from which to choose.
 *                    Those whose flags are 0 are used.
 *  ncol       int    The number of entries in colors[].
 * Input/Output:
 *  c       XColor *  On input this should contain the desired color.
 *                    On output it will contain the nearest unused color
 *                    found in colors[].
 * Output:
 *  return     int    0 - OK.
 *                    1 - There are no colors left.
 */
#ifdef __STDC__
static int pgx_nearest_color(XColor *colors, int ncol, XColor *c)
#else
static int pgx_nearest_color(colors, ncol, c)
     XColor *colors; int ncol; XColor *c;
#endif
{
  XColor *best=NULL;  /* The nearest color found so far */
  float residual=0;   /* The square color residual of best compared to c */
  int first = 1;      /* True until residual has been initialized */
  int i;
/*
 * Find the unused entry in colors[] that is nearest to the requested color.
 */
  for(i=0; i<ncol; i++) {
    XColor *xc = colors + i;
/*
 * Compute the square residual between the two colors.
 */
    float dr = c->red - xc->red;
    float dg = c->green - xc->green;
    float db = c->blue - xc->blue;
    float r = dr * dr + dg * dg + db * db;
    if(r < residual || first) {
      first = 0;
      residual = r;
      best = xc;
    };
  };
/*
 * Not found?
 */
  if(!best)
    return 1;
/*
 * Copy the color to the return container.
 */
  *c = *best;
  return 0;
}
