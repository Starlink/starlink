#ifndef pgxwin_h
#define pgxwin_h

/*
 * The following macro must enclose all function prototype arguments.
 * This allows pre-ANSI compilers to compile this code, by discarding
 * the prototype arguments if __STDC__ is not set.
 */
#ifdef __STDC__
#define ARGS(args) args
#else
#define ARGS(args) ()
#endif

typedef struct PgxWin PgxWin;

/*
 * Declare a type for the function that the pgxwin library calls
 * whenever it wants to resize the PGPLOT window.
 * A function of this type must be assigned to the resize_window_fn
 * member of the PgxWin structure as follows:
 * If resizing is unrestricted, assign pgx_resize_window.
 * If resizing is to be disallowed, assign NULL.
 * Otherwise supply your own resize function.
 */
typedef void (*PgxResizeWindowFn) ARGS((PgxWin *pgx, unsigned width, \
					unsigned height));

/*
 * Declare a type for the function that the pgxwin library calls
 * whenever it wants to create a new backing pixmap. The new pixmap
 * must be assigned to pgx->pixmap, and pgx->geom must be updated
 * to reflect its new dimensions.
 * A default function of this type pgx_new_pixmap() is substituted if
 * PgxWin::new_pixmap_fn==NULL. This should be satisfactory for
 * most cases.
 */
typedef void (*PgxNewPixmapFn) ARGS((PgxWin *pgx, unsigned width, \
				     unsigned height));
/*
 * Declare a visual/colormap context descriptor.
 */
typedef struct {
  XVisualInfo *vi;      /* The visual info descriptor for the colormap */
  Colormap cmap;        /* Colormap ID */
  int private;          /* True if the colormap had to be newly allocated */
  int ncol;             /* The number of colors available. ci = [0...ncol-1] */
  int monochrome;       /* True we have to use a monochrome screen */
  unsigned long *pixel; /* 'ncol' colormap pixel indexes. */
  int npixel;           /* The number of colorcells actually allocated */
  XColor *xcolor;       /* 'ncol' colormap color representations */
  int initialized;      /* True after pgx_init_colors() */
  int default_class;    /* The class of the default visual of the screen */
  int readonly;         /* True if the allocated color-cells are readonly */
  int nwork;            /* The number of entries in work[] */
  XColor *work;         /* Work array for shared color allocations */
} PgxColor;

/*
 * Declare a scroll/pan context descriptor.
 * The x and y coordinates are the position of the top left corner
 * of the window within the pixmap.
 */
typedef struct {
  unsigned x;
  unsigned y;
} PgxScroll;

/*
 * Declare a container type to record the rectangular extent of the
 * window area that is available for drawing. Clipping is optional
 * and by default it is turned off, but if it is enabled (via
 * a call to pgx_update_clip()), then it should be kept up to date
 * by calling pgx_update_clip() whenever ConfigureNotify events
 * are received.
 */
typedef struct {
  int doclip;           /* True if clipping is enabled */
  int xmin, xmax;       /* Min/max X-axis pixels excluding border */
  int ymin, ymax;       /* Min/max Y-axis pixels excluding border */
} PgxClip;

typedef struct PgxState PgxState;

/*
 * All X devices must fill a structure of the following type.
 */
struct PgxWin {
  void *context;            /* The context descriptor of the window type */
  Display *display;         /* The display of the window */
  int screen;               /* The screen of the display */
  Window window;            /* The window id */
  Pixmap pixmap;            /* The backing pixmap (or None if not available) */
  GC expose_gc;             /* Expose-event handler graphical context */
  int bad_device;           /* Set to 1 by pgx_bad_device() after fatal error.*/
  char *name;               /* The name of the window */
  int xmargin;              /* The number of pixels to leave blank on */
                            /*  either side of the plot area (default=0). */
  int ymargin;              /* The number of pixels to leave blank above */
                            /*  and below the plot area (default=0). */
  PgxColor *color;          /* The visual/colormap context descriptor */
  PgxScroll scroll;         /* The pixmap scroll context descriptor */
  PgxClip clip;             /* The window clipping area */
  PgxResizeWindowFn resize_fn;  /* Function to call to resize window. */
  PgxNewPixmapFn new_pixmap_fn; /* Function to call to allocate a new pixmap */
  XErrorHandler old_handler;/* Used to preserve previous X error handler */
  PgxState *state;          /* This is NULL when not in use. It is created */
                            /* on pgbeg() via new_PgxState() and destroyed on */
                            /* pgend() via del_PgxState() */
};

int pgx_pre_opcode ARGS((PgxWin *pgx, int opcode));
int pgx_scroll ARGS((PgxWin *pgx, unsigned x, unsigned y));
int pgx_update_clip ARGS((PgxWin *pgx, int doclip, unsigned width,
			  unsigned height, unsigned border));
int pgx_set_margin ARGS((PgxWin *pgx, int xmargin, int ymargin));
int pgx_expose ARGS((PgxWin *pgx, XEvent *event));
void pgx_start_error_watch ARGS((PgxWin *pgx));
int pgx_end_error_watch ARGS((PgxWin *pgx));

PgxWin *new_PgxWin ARGS((Display *display, int screen, void *context,char *name,
		    PgxResizeWindowFn resize_fn, PgxNewPixmapFn pixmap_fn));
PgxWin *del_PgxWin ARGS((PgxWin *pgx));

PgxState *pgx_open ARGS((PgxWin *pgx));
PgxState *pgx_close ARGS((PgxWin *pgx));
int pgx_bad_device ARGS((PgxWin *pgx));
void pgx_draw_line ARGS((PgxWin *pgx, float *rbuf));
void pgx_draw_dot ARGS((PgxWin *pgx, float *rbuf));
int pgx_flush ARGS((PgxWin *pgx));
int pgx_set_ci ARGS((PgxWin *pgx, int ci));
void pgx_poly_fill ARGS((PgxWin *pgx, float *rbuf));
void pgx_rect_fill ARGS((PgxWin *pgx, float *rbuf));
void pgx_set_lw ARGS((PgxWin *pgx, float lw));
int pgx_pix_line ARGS((PgxWin *pgx, float *rbuf, int *nbuf));
void pgx_set_world ARGS((PgxWin *pgx, float *rbuf));
int pgx_set_rgb ARGS((PgxWin *pgx, int ci, float red, float green, float blue));
void pgx_get_rgb ARGS((PgxWin *pgx, float *rbuf, int *nbuf));
void pgx_scroll_rect ARGS((PgxWin *pgx, float *rbuf));
int pgx_clear_window ARGS((PgxWin *pgx));
void pgx_get_resolution ARGS((PgxWin *pgx, float *xpix_per_inch, \
			      float *ypix_per_inch));
void pgx_def_size ARGS((PgxWin *pgx, unsigned d_width, unsigned d_height, \
		 float *rbuf, int *nbuf));
void pgx_new_pixmap ARGS((PgxWin *pgx, unsigned width, unsigned height));
void pgx_begin_picture ARGS((PgxWin *pgx, float *rbuf));

int pgx_set_background ARGS((PgxWin *pgx, XColor *xc));
int pgx_set_foreground ARGS((PgxWin *pgx, XColor *xc));

unsigned pgx_pixmap_width ARGS((PgxWin *pgx));
unsigned pgx_pixmap_height ARGS((PgxWin *pgx));

  /* Convert from X window coordinates to PGPLOT device coordinates */

int pgx_win2dev ARGS((PgxWin *pgx, int x, int y, float *rbuf));

  /* Convert from PGPLOT device coordinates to PGPLOT world coordinates */

int pgx_dev2world ARGS((PgxWin *pgx, float *rbuf));

  /* Convert from PGPLOT world coordinates to PGPLOT device coordinates */

int pgx_world2dev ARGS((PgxWin *pgx, float *rbuf));

  /* Convert from PGPLOT device coordinates to X window coordinates */

int pgx_dev2win ARGS((PgxWin *pgx, float *rbuf, int *x, int *y));

  /* Return the current parent of a given PGPLOT window */

Window pgx_parent_window ARGS((PgxWin *pgx));

/*
 * Alternate visual/colormap acquisition functions.
 */

/* Search for a suitable visual as directed by the 'class_name' string */

PgxColor *pgx_new_visual ARGS((PgxWin *pgx, char *class_name, int min_col, \
		   int max_col, int share));

/* Don't allocate any colors. Instead just use the black and white pixels
 * of the screen to implement a monochrome PGPLOT window.
 */

PgxColor *pgx_bw_visual ARGS((PgxWin *pgx));

/* Allocate colors from the default (root window) colormap */

PgxColor *pgx_default_visual ARGS((PgxWin *pgx, int min_col, int max_col, \
				   int share));

/* Allocate colors from a specified existing visual/colormap */


PgxColor *pgx_adopt_visual ARGS((PgxWin *pgx, VisualID vid, Colormap cmap, \
			   int min_col, int max_col, int share));

/* Allocate colors from the visual/colormap of a specified other window */


PgxColor *pgx_window_visual ARGS((PgxWin *pgx, Window w, int min_col, \
				  int max_col, int share));
/* Delete pgx->color */

PgxColor *pgx_del_visual ARGS((PgxWin *pgx));

/* Enumerate the types of cursor augmentation supported by pgx_set_cursor() */
 
#define PGX_NORM_CURSOR  0 /* Un-augmented X cursor */
#define PGX_LINE_CURSOR  1 /* Line cursor between beg and end */
#define PGX_RECT_CURSOR  2 /* Rectangular cursor between beg and end */
#define PGX_YRNG_CURSOR  3 /* Two horizontal lines, at beg.x and end.x */
#define PGX_XRNG_CURSOR  4 /* Two vertical lines, at beg.y and end.y */
#define PGX_VLINE_CURSOR 5 /* Vertical line cursor at x=beg.x */
#define PGX_HLINE_CURSOR 6 /* Horizontal line cursor at y=beg.y */
#define PGX_CROSS_CURSOR 7 /* Cross-hair cursor */

int pgx_set_cursor ARGS((PgxWin *pgx, int ci, int type, int warp, \
			 float *rbeg, float *rend));
int pgx_locate_cursor ARGS((PgxWin *pgx));
int pgx_refresh_cursor ARGS((PgxWin *pgx));
int pgx_draw_cursor ARGS((PgxWin *pgx));
int pgx_erase_cursor ARGS((PgxWin *pgx));
int pgx_cursor_event ARGS((PgxWin *pgx, XEvent *event, float *rbuf, char *key));

/* Enumerate the operations performed by pgx_select_events() */

#define PGX_SET_EVENTS 0 /* Replace the existing event mask */
#define PGX_ADD_EVENTS 1 /* Add events to the existing event mask */
#define PGX_REM_EVENTS 2 /* Remove events from the existing event mask */

unsigned long pgx_select_events ARGS((PgxWin *pgx, int oper, long events));
int pgx_read_cursor ARGS((PgxWin *pgx, float *rbuf, char *chr, int *nbuf, int *lchr));
int pgx_same_string ARGS((char *s1, char *s2));
#endif
