/*.......................................................................
 * PGPLOT driver for workstations running X Windows.
 * Version 1.0 - 1989 Nov 06 - A. L. Fey
 * Version 3.0 - 1994 Nov 06 - M. C. Shepherd (mcs@astro.caltech.edu).
 *                             Re-write. Visible changes include:
 *                             1. Corrected expose-event handling.
 *                             2. The driver now runs a window server,
 *                                so that if requested, windows
 *                                persist for re-use by later programs.
 *                             3. Support for gray-scale and truecolor visuals.
 *                             4. Support for private color maps.
 *                             5. Support for window-resizing by the user.
 *                             6. New X-resources: pgxwin.geometry,
 *                                pgxwin.iconize, pgxwin.minColors,
 *                                pgxwin.maxColors, pgxwin.visual,
 *                                pgxwin.acceptQuit, pgxwin.crosshair.
 *                             7. "rubber-band" cursor options for use with
 *                                pgband().
 *                             8. Corrected selective event handling.
 *                             9. Fixed input focus code so that iconizing
 *                                doesn't kill the program.
 *                            10. Arranged for the window manager to ignore
 *                                the delete-window option unless the
 *                                pgxwin.acceptQuit resource is assigned a
 *                                truth value.
 *                            11. Added XErrorEvent handling to prevent program
 *                                crashes.
 *                            12. Cursor warps are defered until the cursor
 *                                enters the /xw window, and can be turned
 *                                off entirely with the appropriate argument
 *                                to PGBAND().
 *                            13. Colormap updates are now buffered.
 *                            14. Support for multiple open devices.
 *                            15. The cursor can now be moved with the
 *                                keyboard arrow keys.
 *
 *  Scope: This driver should work with all unix workstations running
 *         X Windows (Version 11). It also works on VMS and OpenVMS
 *         workstations running X.
 *  Color: Visual color maps of types, PsuedoColor, StaticColor, GrayScale
 *         and StaticGray are supported. Where insufficient colors are
 *         available in the default colormap, a private colormap is
 *         requested. If this fails, the device is treated as
 *         monochrome.
 * Cursor: The cursor is controlled by a mouse or equivalent. Buttons
 *         1 2 3 are mapped to characters A D X. The cursor can also
 *         be moved horizontally and vertically with the arrow keys.
 *         Each press of an arrow key moves the cursor one pixel. This
 *         can be increased to 10 pixels by pressing the shift key.
 * Size:   The initial size and position of the window number #,
 *         is determined with the following heirachy of specifications,
 *         missing details at each level are supplied by those below.
 *
 *          1. X-resource: pgxwin.win#.geometry:   WIDTHxHEIGHT+X+Y
 *          2. X-resource: pgxwin.Win.geometry:    WIDTHxHEIGHT+X+Y
 *          3. Environment variable: PGPLOT_XW_WIDTH  [fractional display width]
 *          4. #define's: XW_DEF_WIDTH, XW_DEF_HEIGHT, XW_DEF_ASPECT
 *
 * There are too many other configuration options to document here, but
 * complete documentation of the driver is available over the WEB at URL:
 *
 *          http://astro.caltech.edu/~tjp/xwdriv.html
 *
 */

/*
 * Certain symbols in fcntl.h may not get defined
 * unless the _POSIX_SOURCE feature-test macro is set.
 */
#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif

/*
 * Allow xwdriv to be calleable by FORTRAN using the two commonest
 * calling conventions. Both conventions append length arguments for
 * each FORTRAN string at the end of the argument list, and convert the
 * name to lower-case, but one post-pends an underscore to the function
 * name (PG_PPU) while the other doesn't. Note the VMS is handled
 * separately below. For other calling conventions you must write a
 * C wrapper routine to call xwdriv() or xwdriv_().
 */
#ifdef PG_PPU
#define XWDRIV xwdriv_
#else
#define XWDRIV xwdriv
#endif

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#ifndef convex
#include <string.h>
#endif

/*
 * VAX VMS includes etc..
 */
#ifdef VMS
#include <signal.h>  /* sleep() is prototyped here */
#include <unixio.h>  /* access() is prototyped here */
#include <descrip.h>
#include <ssdef.h>
#include <clidef.h>
#include <libclidef.h>
#include <lib$routines.h>

typedef struct dsc$descriptor_s VMS_string;

#define VMS_STRING(dsc, string) \
  dsc.dsc$w_length = strlen(string); \
  dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
  dsc.dsc$b_class = DSC$K_CLASS_S; \
  dsc.dsc$a_pointer = string;

static int vms_define_command(char *file, char *command);
static int vms_spawn_nowait(char *command);
#endif

#ifndef VMS
#include <fcntl.h>
#include <unistd.h>
#endif

/* X-Window include files */

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>

/*
 * Record the client/server protocol revision implemented herein.
 */
#define PGXWIN_REVISION 0

/*
 * Allow the pgplot /xw server name to be changed by compile
 * time pre-definition of PGXWIN_SERVER. If pgxwin_server is modified in
 * such a way as to become incompatible with an earlier version of xwdriv.c,
 * its name should be changed by postfixing a small increasing integer
 * to the name of the executable. New and old pgplot programs can then
 * coexist as long as both versions of the server are
 * retained. In order not to clutter up system directories, don't change
 * this name unless absolutely necessary. This is also the name given to
 * the server selection atom, so be sure that it remains valid for this
 * purpose.
 */
#ifndef PGXWIN_SERVER
#define PGXWIN_SERVER "pgxwin_server"
#endif

#define NCOLORS 16            /* Number of pre-defined PGPLOT colors */
#define XW_IMAGE_LEN 1280     /* Length of the line-of-pixels buffer */
#define COLORMULT 65535       /* Normalized color intensity multiplier */

#define XW_IDENT "PGPLOT /xw"      /* Name to prefix messages to user */
#define XW_DEF_ASPECT (8.5/11.0)   /* Default aspect (height/width) of window */
#define XW_DEF_WIDTH 867           /* Default width (pixels) */
#define XW_DEF_HEIGHT ((int) XW_DEF_WIDTH * XW_DEF_ASPECT)
                                   /* Default height (pixels) */
#define XW_SERVER_TIMEOUT 10       /* Max time to allow for server startup */

/*
 * Define equivalence values for the XParseGeometry bitmask bits, using
 * values agreed upon by xwdriv.c and pgxwin_server.c, for use in
 * communicating geometries between client and server.
 */
#define XW_WidthValue 1
#define XW_HeightValue 2
#define XW_XValue 4
#define XW_YValue 8
#define XW_XNegative 16
#define XW_YNegative 32

/*
 * Enumerate the supported window close-down modes.
 */
#define XW_DELETE 1
#define XW_PERSIST 2
#define XW_ICONIZE 3

/*
 * Enumerate property-data formats, named after the internal types that
 * are used to communicate them with XChangeProperty() and
 * XGetWindowProperty().
 */
#define XW_CHAR_PROP 8
#define XW_SHORT_PROP 16
#define XW_LONG_PROP 32

/*
 * Set the degree to which holding down the shift-key speeds up cursor
 * motion when an arrow key is held down.
 */
#define ARROW_KEY_VELOCITY 10

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

/* A container used to record the geometry of the X-window */

typedef struct {
  Atom geom_atom;      /* Client/server geometry transaction atom */
  int x,y;             /* Locus of top left corner of window (pixels) */
  unsigned int width;  /* Width of window (pixels) */
  unsigned int height; /* Height of window (pixels) */
  int xpix_per_inch;   /* Number of pixels per inch along X */
  int ypix_per_inch;   /* Number of pixels per inch along Y */
  int xmargin;         /* X-axis 1/4" margin in pixels */
  int ymargin;         /* Y-axis 1/4" margin in pixels */
  int xmin,xmax;       /* Min/max X-axis pixels excluding 1/4" margins */
  int ymin,ymax;       /* Min/max X-axis pixels excluding 1/4" margins */
} XWgeom;

/*
 * Declare a colormap descriptor.
 */
typedef struct {
  XVisualInfo *vi;   /* The visual info descriptor for the colormap */
  Colormap cmap;     /* Colormap ID */
  int ncol;          /* The number of colors available. ci = [0...ncol-1] */
  int monochrome;    /* True we have to use a monochrome screen */
  unsigned long *pixel; /* 'ncol' colormap pixel indexes. */
  XColor *xcolor;    /* 'ncol' colormap color representations */
  int initialized;   /* True after the first call to xw_init_colors() */
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
 * pixmap area that has been modified since the last xw_flush().
 */
typedef struct {
  int modified;    /* True if 'pixmap' has been modified since last update */
  int xmin,xmax;   /* X-axis extent of modified region (pixels) */
  int ymin,ymax;   /* Y-axis extent of modified region (pixels) */
} XWupdate;

/*
 * Declare a container to encapsulate the buffers needed to
 * draw a line of pixels.
 */
typedef struct {
  XImage *xi;          /* Line of pixels Xlib image object */
} XWimage;

/*
 * Declare a container used to hold event state information.
 */
typedef struct {
  long mask;       /* Current event mask */
  int no_buttons;  /* True after failure to acquire ButtonPressMask */
} XWevent;

/*
 * Declare a function type, instances of which are to be called to flush
 * buffered opcodes, and return 0 if OK, or 1 on error.
 */
struct XWdev;
typedef int (*Flush_Opcode_fn) ARGS((struct XWdev *xw));

/*
 * The following container is used to retain state information for /xw
 * connections.
 */
typedef struct XWdev {
  Display *display;  /* Display descriptor */
  Window parent;     /* The ID of the parent window */
  Window window;     /* Window ID */
  Window client;     /* Client communication window */
  Window server;     /* Server communication window */
  Atom server_atom;  /* Server selection atom */
  Atom client_data;  /* Client data property atom */
  int protocol;      /* Client/server communication protocol to use */
  int number;        /* PGPLOT window number */
  int screen;        /* The screen in which the window is displayed */
  int disposition;   /* Close-down mode: XW_PERSIST, XW_ICONIZE, XW_DELETE */
  int bad_device;    /* Set to 1 by xw_bad_device() after fatal errors. */
  int last_error;    /* The last error code trapped by xw_error() */
  Pixmap pixmap;     /* Pixmap ID */
  Cursor norm_cursor;/* ID of normal cursor */
  Cursor live_cursor;/* ID of active cursor */
  int crosshair;     /* Show intersecting line cursor if true */
  XWpoly poly;       /* Polygon-fill accumulation descriptor */
  XWcolor color;     /* Colormap descriptor */
  XWgeom geom;       /* The size and position of the window */
  XWupdate update;   /* Descriptor of un-drawn area of pixmap */
  XWevent event;     /* Event state container */
  XWimage image;     /* Line of pixels container */
  XGCValues gcv;     /* Publicly visible contents of 'gc' */
  GC gc;             /* Graphical context descriptor */
  int last_opcode;   /* Index of last opcode */
  Flush_Opcode_fn flush_opcode_fn; /* Function to flush a buffered opcode */
  struct XWdev *next;/* Pointer to next /xw device in list */
} XWdev;

/* Create an alias for the standard X error handling function type */

typedef int (*Xerrorfn) ARGS((Display *, XErrorEvent *));

/* Private method functions that operate on XWdev descriptors */

static XWdev *new_XWdev ARGS((char *display, int mode));
static XWdev *del_XWdev ARGS((XWdev *xw, int partial));
static int xw_bad_device ARGS((XWdev *xw));

static int xw_ok ARGS((XWdev *xw));
static int xw_set_rgb ARGS((XWdev *xw, int ci, float red, float green, float blue));
static int xw_get_visual ARGS((XWdev *xw));
static int xw_init_colors ARGS((XWdev *xw));
static Window xw_get_window ARGS((XWdev *xw));
static Pixmap xw_get_pixmap ARGS((XWdev *xw));
static int xw_get_cursors ARGS((XWdev *xw));
static int xw_get_image ARGS((XWdev *xw, int npix));
static Window xw_get_server ARGS((XWdev *xw));
static int xw_query_server ARGS((XWdev *xw, XEvent *event));
static int xw_set_cursor ARGS((XWdev *xw, int norm));
static int xw_clear ARGS((XWdev *xw));
static int xw_set_ci ARGS((XWdev *xw, int ci));
static void xw_mark_modified ARGS((XWdev *xw, int x, int y, int diameter));
static int xw_flush ARGS((XWdev *xw));
static void xw_XPoint_to_xy ARGS((XWdev *xw, XPoint *xp, float *xy));
static void xw_xy_to_XPoint ARGS((XWdev *xw, float *xy, XPoint *xp));
static float xw_xcolor_to_rgb ARGS((unsigned short urgb));
static int xw_rgb_to_xcolor ARGS((float rgb));

static int xw_next_page ARGS((XWdev *xw, unsigned int width, unsigned int height));
static int xw_image_line ARGS((XWdev *xw, XPoint *start, float *cells, int ncell));
static int xw_read_cursor ARGS((XWdev *xw, int mode, int posn, XPoint *ref,
				XPoint *pos, char *key));
static int xw_shift_cursor ARGS((XWdev *xw, KeySym keysym, \
				 unsigned int modifiers));
static int xw_expose ARGS((XWdev *xw, XEvent *event));
static int xw_new_geom ARGS((XWdev *xw, int x, int y, unsigned int width,
		       unsigned int height,int mask));
static int xw_error ARGS((Display *display, XErrorEvent *event));
static int xw_locate_cursor ARGS((XWdev *xw, XPoint *pos, int warp, XPoint *loc));
static int xw_next_event ARGS((XWdev *xw, XEvent *event));
static int xw_check_window_event ARGS((XWdev *xw, Window window,
				       long event_mask, XEvent *event));
static unsigned long xw_get_data ARGS((XWdev *xw, char *data, int form, unsigned long n));
static XVisualInfo *xw_visual_info ARGS((Display *display, int screen,
					 Visual *visual));
static void xw_limit_pcoords ARGS((XWdev *xw, XPoint *coord));
static void xw_scroll_rect ARGS((XWdev *xw, float *rbuf));

/* Container for rubber-band cursor resources and status */

typedef struct {
  int line_width;  /* Rubber-band line width */
  int mode;        /* Cursor mode 1=line, 2=rectangle */
  XPoint ref;      /* Reference vertex of cursor */
  XPoint end;      /* End point of cursor */
} Band;

static Band *xw_new_Band ARGS((XWdev *xw, int mode, XPoint *ref));
static int xw_draw_cursor ARGS((XWdev *xw, Band *bc, XPoint *end));
static int xw_erase_cursor ARGS((XWdev *xw, Band *bc));
static int xw_end_cursor ARGS((XWdev *xw, Band *bc, int status));
static Band *xw_del_Band ARGS((XWdev *xw, Band *bc));
static int xw_bound_cursor ARGS((XWdev *xw, XPoint *xp));
static int xw_cursor_line ARGS((XWdev *xw, int xa, int ya, int xb, int yb));
static int xw_add_events ARGS((XWdev *xw, long events));
static int xw_rem_events ARGS((XWdev *xw, long events));

/* Functions used to flush buffered opcodes */

static int xw_update_colors ARGS((XWdev *xw));

/*
 * Declare the head of the list of open XW device descriptors.
 * This has to have file scope to allow the X error handler to get at it.
 */

static XWdev *device_list = NULL;

static XWdev *xw_insert_device ARGS((XWdev *xw));
static XWdev *xw_select_device ARGS((int number));
static XWdev *xw_remove_device ARGS((XWdev *xw));
static char *find_exe ARGS((char *path, char *program));
static int xw_nint ARGS((float f));

/*.......................................................................
 * This is the only external entry point to the /xw device driver.
 * It is called by PGPLOT to open, perform operations on, return
 * information about and close /xw windows.
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
 *  mode    int *  The value of *mode specifies the disposition of
 *                 the device:
 *                  1 - /XWINDOW  => non-persistent window.
 *                  2 - /XSERVE   => persistent window.
 *  len     int    Added to the call line by the FORTRAN compiler.
 *                 This contains the declared size of chr[].  
 */
#ifdef VMS
void xwdriv(ifunc, rbuf, nbuf, chrdsc, lchr, mode)
     int *ifunc;
     float rbuf[];
     int *nbuf;
     struct dsc$descriptor_s *chrdsc; /* VMS FORTRAN string descriptor */
     int *lchr;
     int *mode;
{
  int len = chrdsc->dsc$w_length;
  char *chr = chrdsc->dsc$a_pointer;
#else
void XWDRIV(ifunc, rbuf, nbuf, chr, lchr, mode, len)
 int   *ifunc, *nbuf, *lchr, *mode;
 int   len;
 float rbuf[];
 char  *chr;
{
#endif
  static XWdev *xw = NULL; /* The descriptor of the currently selected device */
  int i;
/*
 * If there is a buffered opcode and the latest opcode is not the same
 * as the last opcode, call the given flush function for the
 * buffered opcode.
 */
  if(xw && !xw->bad_device) {
    if(xw->last_opcode != *ifunc) {
      if(xw->flush_opcode_fn != (Flush_Opcode_fn) 0) {
	(*xw->flush_opcode_fn)(xw);
	xw->flush_opcode_fn = (Flush_Opcode_fn) 0;
      };
/*
 * Record the current opcode for next time.
 */
      xw->last_opcode = *ifunc;
    };
  };

/* Branch on opcode. */

  switch(*ifunc) {

/*--- IFUNC=1, Return device name ---------------------------------------*/

  case 1:
    {
      char *dev_name;
      switch(*mode) {  /* Locate the name used to select the given mode */
      case 1: default:
	dev_name = "XWINDOW (X window window@node:display.screen/xw)";
	break;
      case 2:
	dev_name = "XSERVE  (A /XWINDOW window that persists for re-use)";
	break;
      };
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
    rbuf[5] = (xw && !xw->bad_device) ? xw->color.ncol-1 : 1;
    *nbuf = 6;
    break;

/*--- IFUNC=3, Return device resolution ---------------------------------*/

  case 3:
    if(xw_ok(xw)) {
      rbuf[0] = xw->geom.xpix_per_inch;
      rbuf[1] = xw->geom.ypix_per_inch;
    } else {
      rbuf[0] = 1.0;
      rbuf[1] = 1.0;
    };
    rbuf[2] = 1.0;		/* Device coordinates per pixel */
    *nbuf = 3;
    break;

/*--- IFUNC=4, Return misc device info ----------------------------------*/

  case 4:
    chr[0] = 'I'; /* Interactive device */
    chr[1] = 'C'; /* Cursor is available */
    chr[2] = 'N'; /* No dashed lines */
    chr[3] = 'A'; /* Area fill available */
    chr[4] = 'T'; /* Thick lines */
    chr[5] = 'R'; /* Rectangle fill available */
    chr[6] = 'P'; /* Line of pixels available */
/*
 * Tell PGPLOT to prompt on PGEND only if the window goes away.
 */
    chr[7] = xw && xw->disposition==XW_PERSIST ? 'N':'V';
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
    if(xw && !xw->bad_device) {  /* Return the size of the current window */
      XWindowAttributes attr;
      XGetWindowAttributes(xw->display, xw->window, &attr);
      if(!xw->bad_device) {
	rbuf[0] = 0.0;
	rbuf[1] = (float) (attr.width - 2 * xw->geom.xmargin);
	rbuf[2] = 0.0;
	rbuf[3] = (float) (attr.height - 2 * xw->geom.ymargin);
      } else {
	rbuf[0] = 0.0;
	rbuf[1] = (float) xw->geom.width;
	rbuf[2] = 0.0;
	rbuf[3] = (float) xw->geom.height;
      };
    } else {
      rbuf[0] = 0.0;
      rbuf[1] = XW_DEF_WIDTH;
      rbuf[2] = 0.0;
      rbuf[3] = XW_DEF_HEIGHT;
    };
    *nbuf = 4;
    break;

/*--- IFUNC=7, Return misc defaults -------------------------------------*/

  case 7:
    rbuf[0] = 1.0;
    *nbuf = 1;
    break;

/*--- IFUNC=8, Select plot ----------------------------------------------*/

  case 8:
    xw = xw_select_device((int)(rbuf[1]+0.5));
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
      fprintf(stderr, "%s: Display name too long.\n", XW_IDENT);
      return;
    } else {
      chr[*lchr] = '\0';
    };
/*
 * Connect to the server and create the window.
 */
    xw = new_XWdev(chr, *mode);
    if(xw==NULL)
      return;
/*
 * Insert the device in the list of open devices.
 */
    xw_insert_device(xw);
    rbuf[0] = xw->number; /* Number used to select this device */
    rbuf[1] = 1.0;
    *nbuf = 2;
    break;

/*--- IFUNC=10, Close workstation ---------------------------------------*/

  case 10:
/*
 * Remove the device from the list of open devices and delete it.
 */
    xw_remove_device(xw);
    xw = del_XWdev(xw,0);
    break;

/*--- IFUNC=11, Begin picture -------------------------------------------*/

  case 11:
    if(xw_ok(xw)) {
/*
 * Convert the passed max X and Y coordinates into the total width of the
 * new window. Add 1/4" margins to the requested area.
 */
      unsigned int width  = (int) (rbuf[0] + 0.5) + 2 * xw->geom.xmargin;
      unsigned int height = (int) (rbuf[1] + 0.5) + 2 * xw->geom.ymargin;
/*
 * Re-size the window if required.
 */
      xw_next_page(xw, width, height);
    };
    break;

/*--- IFUNC=12, Draw line -----------------------------------------------*/

  case 12:
    if(xw_ok(xw) && xw->pixmap!=None) {
      XPoint start;
      XPoint end;
      xw_xy_to_XPoint(xw, &rbuf[0], &start);
      xw_xy_to_XPoint(xw, &rbuf[2], &end);
      XDrawLine(xw->display, xw->pixmap, xw->gc, start.x,start.y, end.x,end.y);
      xw_mark_modified(xw, start.x, start.y, xw->gcv.line_width);
      xw_mark_modified(xw, end.x, end.y, xw->gcv.line_width);
    };
    break;

/*--- IFUNC=13, Draw dot ------------------------------------------------*/

  case 13:
    if(xw_ok(xw) && xw->pixmap!=None) {
      XPoint xp;
      int radius = xw->gcv.line_width/2;
      xw_xy_to_XPoint(xw, rbuf, &xp);
      if(radius < 1) {
	XDrawPoint(xw->display, xw->pixmap, xw->gc, xp.x, xp.y);
      } else {
	unsigned int diameter = radius*2;
	int x = xp.x - radius;
	int y = xp.y - radius;
	XFillArc(xw->display, xw->pixmap, xw->gc, x, y, diameter, diameter,
		 0, 23040);
      };
      xw_mark_modified(xw, xp.x, xp.y, xw->gcv.line_width);
    };
    break;

/*--- IFUNC=14, End picture ---------------------------------------------*/

  case 14:
    break;

/*--- IFUNC=15, Select color index --------------------------------------*/

  case 15:
    if(xw_ok(xw))
      xw_set_ci(xw, (int) (rbuf[0] + 0.5));
    break;

/*--- IFUNC=16, Flush buffer. -------------------------------------------*/

  case 16:
    if(xw_ok(xw))
      xw_flush(xw);
    break;

/*--- IFUNC=17, Read cursor. --------------------------------------------*/

  case 17:
    if(xw_ok(xw)) {
      XPoint ref;                     /* Reference cursor coordinates */
      XPoint pos;                     /* Input/Output cursor coordinates */
      int mode = 0;                   /* Cursor band mode */
      int posn = 1;                   /* True to position the cursor */
      xw_xy_to_XPoint(xw, rbuf, &pos);
      xw_xy_to_XPoint(xw, &rbuf[2], &ref);
      mode = (int)(rbuf[4]+0.5);
      posn = (int)(rbuf[5]+0.5) > 0;
      if(xw_read_cursor(xw, mode, posn, &ref, &pos, chr)==0)
	xw_XPoint_to_xy(xw, &pos, rbuf);
      else
	*chr = '\0';
    } else {
      *chr = '\0';
    };
    *lchr = 1;
    *nbuf = 2;
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
    if(xw_ok(xw) && xw->pixmap != None) {
/*
 * The first call specifies just the number of vertixes in the polygon.
 */
      if(xw->poly.npoint == 0) {
	xw->poly.npoint = (int) (rbuf[0] + 0.5);
	xw->poly.points = (XPoint *) malloc(sizeof(XPoint) * xw->poly.npoint);
	if(xw->poly.points == NULL)
	  fprintf(stderr, "%s: Insufficient memory for polygon points.\n",
		  XW_IDENT);
	xw->poly.ndone = 0;
/*
 * The next xw->poly.npoint calls specify the vertexes of the polygon.
 */
      } else {
/*
 * Ignore the points if the above malloc() failed.
 */
	if(xw->poly.points) {
	  XPoint *xp = &xw->poly.points[xw->poly.ndone];
	  xw_xy_to_XPoint(xw, rbuf, xp);
	  xw_mark_modified(xw, xp->x, xp->y, 1);
	};
/*
 * Maintain the count of the number of points, even if no memory for the
 * points is available. Thus we can just ignore all calls until
 * xw->poly.ndone == xw->poly.npoint.
 */
	xw->poly.ndone++;
/*
 * On the last call display the filled polygon and release the memory used
 * to store its vertexes.
 */
	if(xw->poly.ndone >= xw->poly.npoint) {
	  if(xw->poly.points) {
	    XFillPolygon(xw->display, xw->pixmap, xw->gc, xw->poly.points,
			 xw->poly.npoint, Complex, CoordModeOrigin); 
	    free((char *)xw->poly.points);
	    xw->poly.points = NULL;
	  };
	  xw->poly.npoint = 0;
	};
      };
    };
    break;

/*--- IFUNC=21, Set color representation. -------------------------------*/

  case 21:
    if(xw_ok(xw)) {
      if(!xw->color.initialized)
	xw_init_colors(xw);
      xw_set_rgb(xw, (int)(rbuf[0]+0.5), rbuf[1],rbuf[2],rbuf[3]);
    };
    break;

/*--- IFUNC=22, Set line width. -----------------------------------------*/

  case 22:
/*
 * The line width is provided in multiples of 0.005 inches.
 */
    if(xw_ok(xw)) {
      xw->gcv.line_width = (int)(rbuf[0]*0.005 * xw->geom.xpix_per_inch);
      XChangeGC(xw->display, xw->gc, (unsigned long) GCLineWidth, &xw->gcv);
    };
    break;

/*--- IFUNC=23, Escape --------------------------------------------------*/
    /* (Not implemented: ignored) */
  case 23:
    break;

/*--- IFUNC=24, Rectangle Fill. -----------------------------------------*/

  case 24:
    if(xw_ok(xw) && xw->pixmap != None) {
      XPoint blc;
      XPoint trc;
      xw_xy_to_XPoint(xw, &rbuf[0], &blc);
      xw_xy_to_XPoint(xw, &rbuf[2], &trc);
      XFillRectangle(xw->display, xw->pixmap, xw->gc, blc.x, trc.y,
		     (unsigned)(trc.x-blc.x+1), (unsigned)(blc.y-trc.y+1));
      xw_mark_modified(xw, blc.x, blc.y, 1);
      xw_mark_modified(xw, trc.x, trc.y, 1);
    };
    break;

/*--- IFUNC=25, ---------------------------------------------------------*/
  /* (Not implemented: ignored) */
  case 25:
    break;

/*--- IFUNC=26, Line of pixels ------------------------------------------*/

  case 26:
    if(xw_ok(xw)) {
      XPoint start;
      xw_xy_to_XPoint(xw, rbuf, &start);
      xw_image_line(xw, &start, &rbuf[2], *nbuf - 2);
    };
    break;

/*--- IFUNC=29, Query color representation ------------------------------*/
  case 29:
    if(xw_ok(xw)) {
      int ci = (int) (rbuf[0] + 0.5);
      if(!xw->color.initialized)
	xw_init_colors(xw);
      rbuf[1] = xw_xcolor_to_rgb(xw->color.xcolor[ci].red);
      rbuf[2] = xw_xcolor_to_rgb(xw->color.xcolor[ci].green);
      rbuf[3] = xw_xcolor_to_rgb(xw->color.xcolor[ci].blue);
    } else {
      rbuf[1] = rbuf[2] = rbuf[3] = 0;
    };
    *nbuf = 4;
    break;

/*--- IFUNC=30, Scroll rectangle ----------------------------------------*/
  case 30:
    xw_scroll_rect(xw, rbuf);
    break;

/*--- IFUNC=?, ----------------------------------------------------------*/

  default:
    fprintf(stderr, "%s: Ignoring unimplemented opcode=%d.\n",XW_IDENT, *ifunc);
    *nbuf = -1;
    break;
  };
/*
 * After a server error, close the connection to the display and set all
 * server resources to 'None'. This both prevents calls on bad resources
 * and by deleting the client communication window, tells the server to
 * close the connection if the server hasn't already died.
 */
  if(xw && xw->bad_device && xw->display)
    del_XWdev(xw, 1);
  return;
}

/*.......................................................................
 * Assign a given RGB color representation to a given color index.
 *
 * Input:
 *  xw    XWdev *  The /xw device descriptor.
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
static int xw_set_rgb(XWdev *xw, int ci, float red, float green, float blue)
#else
static int xw_set_rgb(xw, ci, red, green, blue)
     XWdev *xw; int ci; float red; float green; float blue;
#endif
{
  float gray;   /* Gray-scale intensity */
  XColor *xc;   /* The descriptor of the new color */
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
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
  if(!xw->color.monochrome && ci >= 0 && ci < xw->color.ncol) {
/*
 * Get the color representation descriptor.
 */
    xc = &xw->color.xcolor[ci];
/*
 * Get the pixel to be assigned the new color representation.
 */
    xc->pixel = xw->color.pixel[ci];
    xc->flags = DoRed | DoGreen | DoBlue;
    xc->pad   = 0;
/*
 * Determine the appropriate RGB values for the type of colormap.
 */
    switch(xw->color.vi->class) {
    case PseudoColor:
    case StaticColor:
    case DirectColor:
    case TrueColor:
      xc->red   = xw_rgb_to_xcolor(red);
      xc->green = xw_rgb_to_xcolor(green);
      xc->blue  = xw_rgb_to_xcolor(blue);
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
      xc->red = xc->green = xc->blue = xw_rgb_to_xcolor(gray);
      break;
    };
/*
 * Update the recorded range of color indexes whose color representations
 * have been changed since the last call to xw_update_colors().
 */
    if(xw->color.nbuff<=0) {
      xw->color.sbuff = ci;
      xw->color.nbuff = 1;
    } else if(ci < xw->color.sbuff) {
      xw->color.nbuff += xw->color.sbuff - ci;
      xw->color.sbuff = ci;
    } else if(ci > xw->color.sbuff + xw->color.nbuff-1) {
      xw->color.nbuff = ci - xw->color.sbuff + 1;
    };
/*
 * Register xw_update_colors() to be called to flush the colors to the
 * window.
 */
    xw->flush_opcode_fn = (Flush_Opcode_fn) xw_update_colors;
  };
  return 0;
}

/*.......................................................................
 * Map floating point color intenisties between 0.0 and 1.0 to XColor
 * intensities between 0 to 65535. Numbers outside of this range are
 * limited to the nearest of the two limits.
 *
 * Input:
 *  rgb              float   The PGPLOT normalized intensity to be converted.
 * Output:
 *  return  unsigned short   The equivalent XColor RGB intensity.
 */
#ifdef __STDC__
static int xw_rgb_to_xcolor(float rgb)
#else
static int xw_rgb_to_xcolor(rgb)
     float rgb;
#endif
{
  long lrgb;      /* Use to check output before casting to unsigned short */
/*
 * Check for limiting input values.
 */
  if(rgb < 0.0)
    return 0;
  if(rgb > 1.0)
    return COLORMULT;
/*
 * Form the xcolor intensity in a long int so that its range can be checked
 * before casting to unsigned short.
 */
  lrgb = rgb * COLORMULT + 0.5;
  return lrgb > COLORMULT ? COLORMULT : lrgb;
}

/*.......................................................................
 * Map XColor intensities between 0 and 65535 to floating point color
 * intenisties between 0.0 and 1.0. Numbers outside of this range are
 * limited to the nearest of the two limits.
 *
 * Input:
 *  unsigned short   The equivalent XColor RGB intensity.
 * Output:
 *  return   float   The PGPLOT normalized intensity to be converted.
 */
#ifdef __STDC__
static float xw_xcolor_to_rgb(unsigned short urgb)
#else
static float xw_xcolor_to_rgb(urgb)
     unsigned short urgb;
#endif
{
  float rgb;   /* The output value */
  rgb = (float) urgb / (float) COLORMULT;
/*
 * Check for limiting input values.
 */
  if(rgb < 0.0)
    return 0.0;
  if(rgb > 1.0)
    return 1.0;
  return rgb;
}

/*.......................................................................
 * Flush color-representation changes made by xw_set_rgb() to the /xw
 * window. This updates the window colormap. If color index 0 is changed
 * then the background color is also updated.
 * 
 * Input:
 *  xw      XWdev *    The PGPLOT /xw device descriptor.
 *  If xw->color.nbuff > 0 {
 *    For(ci=xw->color.sbuff; ci<xw->color.sbuff + xw->color.nbuff; ci++) {
 *      xw->color.pixel[ci] = Color pixel to be changed.
 *      xw->color.xcolor[ci]= Requested color representation.
 *    };
 *  };
 * Output:
 *  If xw->color.nbuff > 0 {
 *    For(ci=xw->color.sbuff; ci<xw->color.sbuff + xw->color.nbuff; ci++) {
 *      xw->color.pixel[ci] = New color pixel if the colormap is readonly.
 *      xw->color.xcolor[ci]= Actual color representation installed.
 *    };
 *  };
 *  return    int      0 - OK.
 *                     1 - Error.
 */
#ifdef __STDC__
static int xw_update_colors(XWdev *xw)
#else
static int xw_update_colors(xw)
     XWdev *xw;
#endif
{
  int bad_colors = 0;  /* The number of failed color assignments */
  int i;
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * Are there any colors to be updated?
 */
  if(!xw->color.monochrome && xw->color.nbuff > 0) {
    XColor *xc = &xw->color.xcolor[xw->color.sbuff];
    unsigned long *pixel = &xw->color.pixel[xw->color.sbuff];
    int nbuff = xw->color.nbuff;
/*
 * Install the colors in the color map.
 */
    switch(xw->color.vi->class) {
    case PseudoColor:
    case GrayScale:
    case DirectColor:
      XStoreColors(xw->display, xw->color.cmap, xc, nbuff);
      break;
    case StaticColor:
    case StaticGray:
    case TrueColor:
      for(i=0; i<nbuff && !xw->bad_device; i++) {
	if(XAllocColor(xw->display, xw->color.cmap, &xc[i])) {
	  if(xw->color.initialized)
	    XFreeColors(xw->display, xw->color.cmap, &pixel[i], 1, (long)0);
	  pixel[i] = xc[i].pixel;
	} else {
	  bad_colors++;
	};
      };
      break;
    };
/*
 * Device error?
 */
    if(xw->bad_device)
      return 1;
/*
 * Update the background color?
 */
    if(xw->color.sbuff == 0)
      XSetWindowBackground(xw->display, xw->window, pixel[0]);
/*
 * Did any of the color assignments fail?
 */
    if(bad_colors > 0) {
      fprintf(stderr,
	      "%s: Error setting the color representations of %d colors.\n",
	      XW_IDENT, bad_colors);
    };
  };
/*
 * Reset buffer pointers.
 */
  xw->color.nbuff = 0;
  xw->color.sbuff = 0;
  return xw->bad_device!=0;
}

/*.......................................................................
 * Set up the visual and colormap for the /xw window.
 *
 * Input:
 *  xw      XWdev *    The PGPLOT /xw device descriptor.
 * Output:
 *  xw->color.vi             The info descriptor of the visual to be used.
 *  xw->color.cmap           The ID of the colormap to use.
 *  xw->color.ncol           The number of colors available.
 *  xw->color.pixel[0..ncol] The color cell pixel indexes.
 *  xw->color.xcolor[0..ncol]The color pixel definitions.
 *  xw->color.monochrome     If true, use black and white instead of the above
 *                           values.
 *  xw->color.nbuff          The number of buffered color representations.
 *  xw->color.sbuff          The index of the first buffered color rep.
 *
 *  return   int     0 - OK.
 *                   1 - Error.
 */
#ifdef __STDC__
static int xw_get_visual(XWdev *xw)
#else
static int xw_get_visual(xw)
     XWdev *xw;
#endif
{
  XEvent event;   /* Descriptor of XClientMessage communication descriptor */
  XWindowAttributes attr;
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * Assume that we have a monochrome display until proven otherwise.
 */
  xw->color.monochrome = 1;
  xw->color.ncol = 2;
  xw->color.nbuff = 0;  /* No color representations buffered yet */
  xw->color.sbuff = 0;
/*
 * Inquire the current visual details of the window.
 */
  if(!XGetWindowAttributes(xw->display, xw->window, &attr)) {
    fprintf(stderr,
	    "%s: (xw_get_visual) Error getting attributes for window 0x%lx.\n",
	    XW_IDENT, (unsigned long) xw->window);
    return 1;
  };
  xw->color.vi = xw_visual_info(xw->display, xw->screen, attr.visual);
  xw->color.cmap = attr.colormap;
  if(xw->color.vi == NULL || xw->color.cmap == None)
    return 1;
/*
 * Ask the server for other colormap details.
 */
  event.xclient.message_type = XA_COLORMAP;
  if(xw_query_server(xw, &event))
    return 1;
  xw->color.monochrome = event.xclient.data.l[0] == None;
  xw->color.ncol = event.xclient.data.l[1];
/*
 * Allocate memory for the array of color pixels and color pixel
 * representations.
 */
  if(xw->color.ncol > 0) {
    xw->color.pixel = (unsigned long *) malloc(sizeof(unsigned long) *
					       xw->color.ncol);
    xw->color.xcolor = (XColor *) malloc(sizeof(XColor) * xw->color.ncol);
    if(xw->color.pixel==NULL || xw->color.xcolor==NULL)
      xw->color.ncol = 0;
  };
/*
 * If we got a colormap, wait for the array of 'ncol' color-cell pixel
 * indexes to be placed in the PGXWIN_CLIENT_DATA property on the
 * client communication window, then read it and delete the property.
 */
  if(!xw->color.monochrome) {
    xw->color.ncol = xw_get_data(xw, (char *) &xw->color.pixel[0],
				 XW_LONG_PROP, (unsigned long) xw->color.ncol);
    if(xw->color.ncol==0)
      xw->color.monochrome = 1;
  };
  return 0;
}

/*.......................................................................
 * Initialize the color representations in the color table.
 * xw_get_visual() must have been called prior to calling this function,
 * so that we have a visual and colormap to define the colors in.
 *
 * Input:
 *  xw      XWdev *    The PGPLOT /xw device descriptor.
 * Output:
 *  xw->color.xcolor[0..ncol] The color pixel definitions.
 *  return    int      0 - OK.
 *                     1 - Error.
 */
#ifdef __STDC__
static int xw_init_colors(XWdev *xw)
#else
static int xw_init_colors(xw)
     XWdev *xw;
#endif
{
/*
 * Define the standard PGPLOT line colors (RGB).
 */
  static float ctable[NCOLORS][3] = {
    {0.0,0.0,0.0}, {1.0,1.0,1.0}, {1.0,0.0,0.0}, {0.0,1.0,0.0},
    {0.0,0.0,1.0}, {0.0,1.0,1.0}, {1.0,0.0,1.0}, {1.0,1.0,0.0},
    {1.0,0.5,0.0}, {0.5,1.0,0.0}, {0.0,1.0,0.5}, {0.0,0.5,1.0},
    {0.5,0.0,1.0}, {1.0,0.0,0.5}, {0.333,0.333,0.333}, 
    {0.667,0.667,0.667}
  };
  int i;
/*
 * Initialize the color-table with the standard PGPLOT line colors.
 */
  if(!xw->color.monochrome) {
    int ncol = (NCOLORS < xw->color.ncol) ? NCOLORS : xw->color.ncol;
    for(i=0; i<ncol; i++) {
      if(xw_set_rgb(xw, i, ctable[i][0], ctable[i][1], ctable[i][2]))
	return 1;
    };
/*
 * Initialize the rest of the colors with a grey-scale ramp.
 */
    for(i=ncol; i<xw->color.ncol; i++) {
      float grey = (float)(i-NCOLORS) / (float)(xw->color.ncol-1-NCOLORS);
      if(xw_set_rgb(xw, i, grey, grey, grey))
	return 1;
    };
  };
/*
 * Flush the new color definitions to the display.
 */
  if(xw_update_colors(xw))
    return 1;
/*
 * Record the new colormap state.
 */
  xw->color.initialized = 1;
/*
 * Start with the foreground color set to white.
 */
  if(xw_set_ci(xw, 1))
    return 1;
  return 0;
}

/*.......................................................................
 * Get a new PGPLOT window from the server.
 *
 * Input:
 *  xw       XWdev *  The PGPLOT /xw device descriptor.
 * Output:
 *  xw->window        The new window ID.
 *  return  Window    The window ID, or 'None' on error.
 */
#ifdef __STDC__
static Window xw_get_window(XWdev *xw)
#else
static Window xw_get_window(xw)
     XWdev *xw;
#endif
{
  XEvent event;   /* Descriptor of XClientMessage communication descriptor */
  int number;     /* The requested window number */
/*
 * Device error?
 */
  if(xw->bad_device)
    return None;
/*
 * Keep a record of the window number that was requested.
 */
  number = xw->number;
/*
 * Ask the server for other colormap details.
 */
  event.xclient.message_type = XA_WINDOW;
  event.xclient.data.l[0] = PGXWIN_REVISION;
  event.xclient.data.l[1] = xw->number;
  event.xclient.data.l[2] = xw->screen;
  event.xclient.data.l[3] = xw->disposition;
  if(xw_query_server(xw, &event))
    return None;
  xw->protocol = event.xclient.data.l[0];
  xw->number = event.xclient.data.l[1];
  xw->window = event.xclient.data.l[2];
  xw->disposition = event.xclient.data.l[3];
/*
 * Did the server refuse to give us the requested window?
 */
  if(xw->window == None) {
    if(number != 0)
      fprintf(stderr, "%s: Window %d is unavailable.\n", XW_IDENT, number);
    else
      fprintf(stderr, "%s: Failed to acquire a PGPLOT window.\n", XW_IDENT);
  };
  return xw->window;
}

/*.......................................................................
 * Get a new pixmap from the server. This should be called whenever a
 * new page is started.
 *
 * Input:
 *  xw       XWdev *  The PGPLOT /xw device descriptor.
 * Output:
 *  xw->pixmap        The new pixmap ID.
 *  return  Pixmap    The pixmap ID, or 'None' on error.
 */
#ifdef __STDC__
static Pixmap xw_get_pixmap(XWdev *xw)
#else
static Pixmap xw_get_pixmap(xw)
     XWdev *xw;
#endif
{
  XEvent event;   /* Descriptor of XClientMessage communication descriptor */
/*
 * Device error?
 */
  if(xw->bad_device)
    return None;
/*
 * Ask the server for other colormap details.
 */
  event.xclient.message_type = XA_PIXMAP;
  event.xclient.data.l[0] = xw->color.monochrome ? BlackPixel(xw->display, xw->screen) : xw->color.pixel[0];
  if(xw_query_server(xw, &event))
    return 1;
  xw->pixmap = event.xclient.data.l[0];
/*
 * Did the server fail to give us the requested pixmap?
 */
  if(xw->pixmap == None)
    fprintf(stderr, "%s: Failed to allocate pixmap.\n", XW_IDENT);
  return xw->pixmap;
}

/*.......................................................................
 * Get the IDs of the normal and active cursors.
 *
 * Input:
 *  xw       XWdev *  The PGPLOT /xw device descriptor.
 * Output:
 *  xw->norm_cursor   The ID of the idle cursor.
 *  xw->live_cursor   The ID of the live cursor.
 *  return     int    0 - OK.
 *                    1 - Error.
 */
#ifdef __STDC__
static int xw_get_cursors(XWdev *xw)
#else
static int xw_get_cursors(xw)
     XWdev *xw;
#endif
{
  XEvent event;   /* Descriptor of XClientMessage communication descriptor */
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * Ask the server for other colormap details.
 */
  event.xclient.message_type = XA_CURSOR;
  if(xw_query_server(xw, &event))
    return 1;
  xw->norm_cursor = event.xclient.data.l[0];
  xw->live_cursor = event.xclient.data.l[1];
  xw->crosshair   = event.xclient.data.l[2];
  return xw->bad_device!=0;
}

/*.......................................................................
 * Agree upon a window geometry with the PGPLOT /xw server.
 *
 * Input:
 *  xw         XWdev *  The PGPLOT /xw device descriptor. Only the display
 *                      and screen members are required.
 *  mask         int    A bit mask to specify which values have been provided
 *                      and how they should be interpretted. The mask is the
 *                      union of the following:
 *                        WidthValue  -  Use the given width value.
 *                       HeightValue  -  Use the given height value.
 *                            XValue  -  Use the given value of 'x'.
 *                            YValue  -  Use the given value of 'y'.
 *                         XNegative  -  x is wrt the right of the display.
 *                         YNegative  -  y is wrt the left of the display.
 *  x            int    The left edge of the window.
 *  y            int    The top edge of the window.
 *  width   unsigned    The width of the window.
 *  height  unsigned    The height of the window.
 * Output:
 *  xw->geom  XWgeom    The new window geometry.
 *  return       int    0 - OK.
 *                      1 - Error.
 */
#ifdef __STDC__
static int xw_new_geom(XWdev *xw, int x, int y, unsigned int width,
		       unsigned int height, int mask)
#else
static int xw_new_geom(xw, x, y, width, height, mask)
    XWdev *xw; int x; int y; unsigned int width; unsigned int height; int mask;
#endif
{
  XEvent event;   /* Descriptor of XClientMessage communication descriptor */
  unsigned int d_pix_width;   /* Display width in pixels */
  unsigned int d_pix_height;  /* Display height in pixels */
  unsigned int d_mm_width;    /* Display width in mm */
  unsigned int d_mm_height;   /* DIsplay height in mm */
  int xw_mask=0;              /* PGXWIN communication version of 'mask' */
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * Get the PGXWIN_GEOMETRY transaction atom.
 */
  if(xw->geom.geom_atom == None)
    xw->geom.geom_atom = XInternAtom(xw->display, "PGXWIN_GEOMETRY", False);
/*
 * Translate the local bitmask values to a PGXWIN defined bitmask for
 * communication, since different Xlibs may define the XParseGeometry
 * bitmask values differently. They will be translated back in the server.
 */
  if(mask & WidthValue)
    xw_mask |= XW_WidthValue;
  if(mask & HeightValue)
    xw_mask |= XW_HeightValue;
  if(mask & XValue)
    xw_mask |= XW_XValue;
  if(mask & YValue)
    xw_mask |= XW_YValue;
  if(mask & XNegative)
    xw_mask |= XW_XNegative;
  if(mask & YNegative)
    xw_mask |= XW_YNegative;    
/*
 * Send geometry prefences to the server and receive the resulting
 * geometry.
 */
  event.xclient.message_type = xw->geom.geom_atom;
  event.xclient.data.l[0] = x;
  event.xclient.data.l[1] = y;
  event.xclient.data.l[2] = width;
  event.xclient.data.l[3] = height;
  event.xclient.data.l[4] = xw_mask;
  if(xw_query_server(xw, &event))
    return 1;
/*
 * Record the geometry that the server sent.
 */
  xw->geom.x = event.xclient.data.l[0];
  xw->geom.y = event.xclient.data.l[1];
  xw->geom.width = event.xclient.data.l[2];
  xw->geom.height = event.xclient.data.l[3];
/*
 * Determine the current display width and height in mm and pixels.
 */
  d_pix_width = DisplayWidth(xw->display, xw->screen);
  d_mm_width = DisplayWidthMM(xw->display, xw->screen);
  d_pix_height = DisplayHeight(xw->display, xw->screen);
  d_mm_height = DisplayHeightMM(xw->display, xw->screen);
/*
 * Determine the device resolution in pixels per inch.
 */
  xw->geom.xpix_per_inch = 25.4 * ((double)d_pix_width / (double)d_mm_width);
  xw->geom.ypix_per_inch = 25.4 * ((double)d_pix_height / (double)d_mm_height);
/*
 * Determine the number of pixels needed to form a 1/4" margin around the
 * the plot area.
 */
  xw->geom.xmargin = (int) (0.25 * xw->geom.xpix_per_inch + 0.5);
  xw->geom.ymargin = (int) (0.25 * xw->geom.ypix_per_inch + 0.5);
/*
 * Determine the pixel indexes that enclose an area bounded by 1/4" margins.
 */
  xw->geom.xmin = xw->geom.xmargin;
  xw->geom.xmax = xw->geom.width - xw->geom.xmargin;
  xw->geom.ymin = xw->geom.ymargin;
  xw->geom.ymax = xw->geom.height - xw->geom.ymargin;
  return 0;
}

/*.......................................................................
 * Instate the given cursor type.
 *
 * Input:
 *  xw      XWdev *   The PGPLOT /xw device descriptor.
 *  norm      int     If norm!=0 instate the normal idle cursor.
 *                    If norm==0 instate the active cursor.
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
#ifdef __STDC__
static int xw_set_cursor(XWdev *xw, int norm)
#else
static int xw_set_cursor(xw, norm)
     XWdev *xw; int norm;
#endif
{
  Cursor cursor;  /* The ID of the cursor to be instated */
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * Get the cursor ID and color to use.
 */
  cursor = norm ? xw->norm_cursor : xw->live_cursor;
/*
 * Register the cursor to the window.
 */
  XDefineCursor(xw->display, xw->window, cursor);
  if(xw->bad_device)
    return 1;
  XFlush(xw->display);
  return xw->bad_device!=0;
}

/*.......................................................................
 * Clear the window and pixmap to start a new page.
 *
 * Input:
 *  xw    XWdev *  The PGPLOT /xw device descriptor.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
#ifdef __STDC__
static int xw_clear(XWdev *xw)
#else
static int xw_clear(xw)
     XWdev *xw;
#endif
{
  unsigned long fg;     /* Saved foreground color */
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * We are about to change the current foreground color, so save the
 * current value to be re-instated shortly.
 */
  fg = xw->gcv.foreground;
/*
 * Clear the pixmap by drawing an opaque rectangle over it in the background
 * color.
 */
  xw_set_ci(xw, 0);
  if(xw->pixmap != None) {
    XFillRectangle(xw->display, xw->pixmap, xw->gc, 0, 0,
		   xw->geom.width, xw->geom.height);
    if(xw->bad_device)
      return 1;
  };
/*
 * Re-instate the foreground color.
 */
  xw->gcv.foreground = fg;
  XSetForeground(xw->display, xw->gc, xw->gcv.foreground);
  if(xw->bad_device)
    return 1;
/*
 * Mark the pixmap as unmodified.
 */
  xw->update.modified = 0;
/*
 * Clear the window itself.
 */
  XClearWindow(xw->display, xw->window);
  if(xw->bad_device)
    return 1;
  XFlush(xw->display);
  if(xw->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * Set the foreground color.
 *
 * Input:
 *  xw    XWdev *  The PGPLOT /xw device descriptor.
 *  ci      int    The PGPLOT color index to instate as the foreground
 *                 color.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
#ifdef __STDC__
static int xw_set_ci(XWdev *xw, int ci)
#else
static int xw_set_ci(xw, ci)
     XWdev *xw; int ci;
#endif
{
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * Assign white to out-of range color indexes.
 */
  if(ci < 0 || ci >= xw->color.ncol)
    ci = 1;
/*
 * Determine the color pixel associated with the given color index.
 */
  if(xw->color.monochrome) {
    xw->gcv.foreground = ci==1 ? WhitePixel(xw->display, xw->screen) :
                                 BlackPixel(xw->display, xw->screen);
  } else {
    xw->gcv.foreground = xw->color.pixel[ci];
  };
/*
 * Instate the new foreground color.
 */
  XSetForeground(xw->display, xw->gc, xw->gcv.foreground);
  if(xw->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * Update the vertices of the rectangular area that has been modified
 * since the last time the window was updated from the pixmap.
 *
 * Input:
 *  xw      XWdev * The PGPLOT /xw device descriptor.
 *  x         int   The x-axis pixel index that the rectangular update area
 *                  must be extended to include.
 *  y         int   The y-axis pixel index that the rectangular update area
 *                  must be extended to include.
 *  diameter  int   The diameter of the locus in pixels. For line or
 *                  point drawing operations this is usually the line width.
 */
#ifdef __STDC__
static void xw_mark_modified(XWdev *xw, int x, int y, int diameter)
#else
static void xw_mark_modified(xw, x, y, diameter)
     XWdev *xw; int x; int y; int diameter;
#endif
{
  int radius = diameter/2;
/*
 * Expand the current rectangle to include point (x,y).
 */
  if(xw->update.modified) {
    if(x - radius < xw->update.xmin)
      xw->update.xmin = x - radius;
    if(x + radius > xw->update.xmax)
      xw->update.xmax = x + radius;
    if(y - radius < xw->update.ymin)
      xw->update.ymin = y - radius;
    if(y + radius > xw->update.ymax)
      xw->update.ymax = y + radius;
  } else {
    xw->update.xmin = x - radius;
    xw->update.xmax = x + radius;
    xw->update.ymin = y - radius;
    xw->update.ymax = y + radius;
    xw->update.modified = 1;
  };
  return;
}

/*.......................................................................
 * Flush changes to the pixmap to the window.
 *
 * Input:
 *  xw    XWdev *  The PGPLOT /xw device descriptor.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
#ifdef __STDC__
static int xw_flush(XWdev *xw)
#else
static int xw_flush(xw)
     XWdev *xw;
#endif
{
  if(xw->bad_device)
    return 1;
/*
 * Flush buffered opcodes if necessary.
 */
  if(xw->flush_opcode_fn != (Flush_Opcode_fn) 0) {
    (*xw->flush_opcode_fn)(xw);
    xw->flush_opcode_fn = (Flush_Opcode_fn) 0;
    if(xw->bad_device)
      return 1;
  };
/*
 * Copy the modified rectangular area of the pixmap to the /xw window.
 */
  if(xw->update.modified) {
/*
 * Enforce bounds on the area to be updated.
 */
    if(xw->update.xmin < 0)
      xw->update.xmin = 0;
    if(xw->update.ymin < 0)
      xw->update.ymin = 0;
    if(xw->update.xmax > xw->geom.width - 1)
      xw->update.xmax = xw->geom.width - 1;
    if(xw->update.ymax > xw->geom.height - 1)
      xw->update.ymax = xw->geom.height - 1;
/*
 * Copy the area to be updated from the pixmap to the window.
 */
    if(xw->pixmap != None && !xw->bad_device) {
      XCopyArea(xw->display, xw->pixmap, xw->window, xw->gc,
		xw->update.xmin, xw->update.ymin,
		(unsigned) (xw->update.xmax - xw->update.xmin + 1),
		(unsigned) (xw->update.ymax - xw->update.ymin + 1),
		xw->update.xmin, xw->update.ymin);
      if(xw->bad_device)
	return 1;
    };
    xw->update.modified = 0;
  };
  XFlush(xw->display);
  if(xw->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * Open a /xw window and return an initialized /xw PGPLOT device descriptor.
 *
 * Input:
 *  display    char *   A '\0' terminated string containing the name of
 *                      the display.
 *  mode        int     The type of window to open.
 *                       1 - None-persistent window.
 *                       2 - Persistent window.
 * Output:
 *  return    XWdev *   THe PGPLOT /xw device descriptor, or NULL on error.
 */
#ifdef __STDC__
static XWdev *new_XWdev(char *display, int mode)
#else
static XWdev *new_XWdev(display, mode)
     char *display; int mode;
#endif
{
  XWdev *xw;     /* The descriptor to be returned */
/*
 * Allocate the descriptor.
 */
  xw = (XWdev *) malloc(sizeof(XWdev));
  if(xw==NULL)
    return del_XWdev(xw,0);
/*
 * Initialize all members of the descriptor at least to the point at which
 * the descriptor can safely be sent to del_XWdev(xw,0). All pointers must
 * be assigned NULL and XIDs assigned None, so that del_XWdev() knows what
 * hasn't been allocated yet.
 */
  xw->display = NULL;
  xw->parent = None;
  xw->window = None;
  xw->client = None;
  xw->server = None;
  xw->number = 0;
  xw->screen = 0;
  xw->disposition = mode==2 ? XW_PERSIST : XW_DELETE;
  xw->bad_device = 0;
  xw->last_error = 0;
  xw->pixmap = None;
  xw->color.cmap = None;
  xw->norm_cursor = None;
  xw->live_cursor = None;
  xw->crosshair = 0;
  xw->poly.points = NULL;
  xw->poly.ndone = xw->poly.npoint = 0;
  xw->gc = NULL;
  xw->color.vi = NULL;
  xw->color.cmap = None;
  xw->color.ncol = 0;
  xw->color.monochrome = 1;
  xw->color.pixel = NULL;
  xw->color.xcolor = NULL;
  xw->color.initialized = 0;
  xw->color.nbuff = 0;
  xw->color.sbuff = 0;
  xw->geom.geom_atom = None;
  xw->update.modified = 0;
  xw->event.mask = NoEventMask;
  xw->event.no_buttons = 0;
  xw->image.xi = NULL;
  xw->last_opcode = 0;
  xw->flush_opcode_fn = (Flush_Opcode_fn) 0;
/*
 * See if the device name is prefixed with a window number.
 * The device name is encoded as [window@][display] | [window].
 * Leave the trailing display name in display.
 */
  {
    char *endp;
    long number = strtol(display, &endp, 10);
    switch(*endp) {
    case '@':
      display = endp+1;
      xw->number = number;
      break;
    case '\0':
      display = endp;
      xw->number = number;
      break;
    };
  };
/*
 * Treat -ve window numbers as equivalent to 0.
 */
  if(xw->number < 0)
    xw->number = 0;
/*
 * Open a connection to the X display server.
 */
  xw->display = XOpenDisplay(display);
  if(xw->display==NULL) {
    fprintf(stderr, "%s: cannot connect to X server [%s]\n", XW_IDENT,
	    XDisplayName(display));
    return del_XWdev(xw,0);
  };
/*
 * Install an error handler for non-fatal errors. If we don't do this then
 * Xlib will do its own error handling, which includes killing the program.
 */
  XSetErrorHandler(xw_error);
/*
 * Get the index of the screen cited in the display string.
 */
  xw->screen = DefaultScreen(xw->display);
/*
 * Also record the parent window ID.
 */
  xw->parent = RootWindow(xw->display, xw->screen);
/*
 * Create a simple window for communication with the server.
 */
  xw->client = XCreateSimpleWindow(xw->display, xw->parent,
				   0, 0, (unsigned)1, (unsigned)1, (unsigned)1,
				   BlackPixel(xw->display, xw->screen),
				   BlackPixel(xw->display, xw->screen));
  if(xw->client == None || xw->bad_device) {
    fprintf(stderr, "%s: Unable to create window.\n", XW_IDENT);
    return del_XWdev(xw,0);
  };
/*
 * We want notice of changes of the PGXWIN_CLIENT_DATA property.
 */
  XSelectInput(xw->display, xw->client, (long) PropertyChangeMask);
  if(xw->bad_device)
    return del_XWdev(xw,0);
/*
 * Get the server selection atom and the client data transfer atom.
 */
  xw->server_atom = XInternAtom(xw->display, PGXWIN_SERVER, False);
  if(xw->bad_device)
    return del_XWdev(xw,0);
  xw->client_data = XInternAtom(xw->display, "PGXWIN_CLIENT_DATA", False);
  if(xw->bad_device)
    return del_XWdev(xw,0);
/*
 * Get the server window ID.
 */
  if(xw_get_server(xw) == None)
    return del_XWdev(xw,0);
/*
 * Get a new PGPLOT window.
 */
  if(xw_get_window(xw) == None)
    return del_XWdev(xw,0);
/*
 * We want to know if the PGPLOT window gets destroyed.
 */
  if(xw_add_events(xw, (long) StructureNotifyMask))
    return del_XWdev(xw,0);
/*
 * Get the visual and colormap of the window.
 */
  if(xw_get_visual(xw))
    return del_XWdev(xw,0);
/*
 * Set/get the current geometry for the window.
 */
  if(xw_new_geom(xw, 0,0, 0,0, 0))
    return del_XWdev(xw,0);
/*
 * Get the IDs of the normal and active cursors.
 */
  if(xw_get_cursors(xw))
    return del_XWdev(xw,0);
/*
 * Instate the normal cursor for the window.
 */
  if(xw_set_cursor(xw, 1))
    return del_XWdev(xw,0);
/*
 * Create and initialize a graphical context descriptor. This is where
 * Line widths, line styles, fill styles, plot color etc.. are
 * recorded.
 */
  xw->gcv.line_width = 1;
  xw->gcv.cap_style = CapRound;
  xw->gcv.join_style = JoinRound;
  xw->gcv.fill_rule = EvenOddRule;
  xw->gcv.graphics_exposures = False;
  xw->gcv.foreground = WhitePixel(xw->display, xw->screen);
  xw->gc = XCreateGC(xw->display, xw->window, (unsigned long) (GCLineWidth |
      GCCapStyle | GCJoinStyle | GCFillRule | GCGraphicsExposures |
      GCForeground), &xw->gcv);
  if(xw->gc==NULL || xw->bad_device) {
    fprintf(stderr, "%s: Failed to allocate graphical context.\n", XW_IDENT);
    return del_XWdev(xw,0);
  };
/*
 * Allocate the buffers that will be used to compose a line
 * of pixels.
 */
  if(xw_get_image(xw, XW_IMAGE_LEN))
    return del_XWdev(xw,0);
/*
 * Return the initialized descriptor for use.
 */
  return xw;
}

/*.......................................................................
 * Delete a PGPLOT /xw device and its descriptor.
 *
 * Input:
 *  xw      XWdev *  The descriptor of the device to be deleted.
 *  partial   int    0 - Normal deletion - delete everything.
 *                   1 - Close the display connection and mark all
 *                       resources as deleted but don't delete the
 *                       container - also set xw->bad_device==1.
 * Output:
 *  return  XWdev *  Allways NULL. Use like xw = del_XWdev(xw,0);
 */
#ifdef __STDC__
static XWdev *del_XWdev(XWdev *xw, int partial)
#else
static XWdev *del_XWdev(xw, partial)
     XWdev *xw; int partial;
#endif
{
  if(xw) {
/*
 * Mark the device as unusable as the first operation so that if
 * any X errors are generated during cleanup, they are not reported.
 */
    xw->bad_device = 1;
/*
 * Delete the graphical context descriptor.
 */
    if(xw->gc)
      XFreeGC(xw->display, xw->gc);
    xw->gc = NULL;
/*
 * Delete the image buffers.
 */
    if(xw->image.xi)
      XDestroyImage(xw->image.xi);
    xw->image.xi = NULL;
/*
 * Check for un-freed polygon points.
 */
    if(xw->poly.points)
      free((char *)xw->poly.points);
    xw->poly.points = NULL;
/*
 * Zap the arrays of color pixels and color pixel definitions.
 */
    if(xw->color.pixel)
      free((char *)xw->color.pixel);
    if(xw->color.xcolor)
      free((char *)xw->color.xcolor);
/*
 * Discard the visual info descriptor.
 */
    if(xw->color.vi)
      XFree((char *)xw->color.vi);
/*
 * Close the connection to the display server - this will also delete
 * all X-resources.
 */
    if(xw->display != NULL) {
/*
 * Explicitly clear the local event mask for the PGPLOT /xw window in case
 * XCloseDisplay fails to do this.
 */
      if(xw->window != None)
	XSelectInput(xw->display, xw->window, (long) NoEventMask);
      XCloseDisplay(xw->display);
      xw->display = NULL;
    };
/*
 * Mark effected resources as deleted.
 */
    xw->client = xw->server = xw->window = xw->parent = None;
    xw->server_atom = xw->client_data = None;
    xw->pixmap = xw->norm_cursor = xw->live_cursor = None;
    xw->flush_opcode_fn = (Flush_Opcode_fn) 0;
    xw->update.modified = 0;
/*
 * Delete the descriptor if required.
 */
    if(!partial) {
      free((char *)xw);
      xw = NULL;
    };
  };
  return xw;
}

/*.......................................................................
 * Before using a given /xw device descriptor call this function to check
 * that it is usable. If it isn't, 0 will be returned and you should not
 * attempt to use the descriptor. If the descriptor is NULL an error
 * message will be presented.
 *
 * Input:
 *  xw       XWdev *  The device descriptor to be checked.
 * Output:
 *  return     int    1 - Descriptor OK.
 *                    0 - Error - don't use /xw.
 */
#ifdef __STDC__
static int xw_ok(XWdev *xw)
#else
static int xw_ok(xw)
     XWdev *xw;
#endif
{
  if(xw==NULL) {
    fprintf(stderr, "%s: Device not open.\n", XW_IDENT);
    return 0;
  };
/*
 * If the window is marked as unusable, it must have been set that way
 * after an error was detected. Assume that the error must already
 * have been reported.
 */
  if(xw->bad_device)
    return 0;
  return 1;
}

/*.......................................................................
 * Present the active cursor, wait for the user to press a button or
 * keyboard key, then retrack the active cursor and return the cursor
 * position and key pressed.
 *
 * Input:
 *  xw    XWdev *   The PGPLOT /xw device descriptor.
 *  mode    int     0 - No rubber banding.
 *                  1 - Maintain a rectangle outline with opposing
 *                      vertexes at (xin,yin) and the cursor.
 *                  2 - Maintain a line between (xin,yin) and the cursor.
 *  posn    int     0 - Don't attempt to position the cursor.
 *                  1 - Do try to pre-position the cursor.
 *  ref  XPoint *   The reference position of the cursor (can be the same
 *                  as 'pos').
 * Input/Output:
 *  pos  XPoint *   The start position of the cursor. On output this is the
 *                  selected position of the cursor.
 * Output:
 *  key    char *   If key!=NULL, the selection key will be assigned to
 *                  the caller's variable pointed to by key.
 *  return  int     0 - OK.
 *                  1 - Error.
 */
#ifdef __STDC__
static int xw_read_cursor(XWdev *xw, int mode, int posn, XPoint *ref,
			  XPoint *pos, char *key)
#else
static int xw_read_cursor(xw, mode, posn, ref, pos, key)
     XWdev *xw; int mode; int posn; XPoint *ref; XPoint *pos; char *key;
#endif
{
  int finished = 0;       /* True when cursor succesfully read */
  XEvent event;           /* The latest event */
  XPoint last;            /* Last recorded position of cursor */
  Band *bc=NULL;          /* Band-cursor descriptor */
  int warped=0;           /* Zero until the cursor has been positioned */
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * Ensure that the input positions are within the pixmap and window bounds.
 */
  if(xw_bound_cursor(xw, ref) || xw_bound_cursor(xw, pos))
    return 1;
/*
 * Present the active cursor.
 */
  if(xw_set_cursor(xw, 0))
    return xw_end_cursor(xw, bc, 1);
/*
 * Make sure that the window is up to date.
 */
  if(xw_flush(xw))
    return xw_end_cursor(xw, bc, 1);
/*
 * De-iconify and bring the window to the foreground.
 */
  XMapRaised(xw->display, xw->window);
  if(xw->bad_device)
    return xw_end_cursor(xw, bc, 1);
  XSync(xw->display, False);
  if(xw->bad_device)
    return xw_end_cursor(xw, bc, 1);
/*
 * Set up for modes that maintain elastic lines following the cursor.
 */
  if((bc=xw_new_Band(xw, mode, ref))==NULL)
    return xw_end_cursor(xw, bc, 1);
/*
 * If the cursor is in the window, locate its position,
 * after warping if requested.
 */
  if(xw_locate_cursor(xw, pos, posn, &last)) {
    warped = 1;
/*
 * Draw the cursor.
 */
    if(xw->bad_device || xw_bound_cursor(xw, &last) ||
       xw_draw_cursor(xw, bc, &last))
      return xw_end_cursor(xw, bc, 1);
  };
/*
 * Discard un-handled ButtonPress, KeyPress and MotionNotify events.
 */
  while(xw_check_window_event(xw, xw->window, (long)
	      (ButtonPressMask | KeyPressMask | PointerMotionMask), &event));
  if(xw->bad_device)
    return xw_end_cursor(xw, bc, 1);
/*
 * Loop for cursor events.
 */
  while(!finished) {
/*
 * Handle the next selected event.
 */
    if(xw_next_event(xw, &event))
      return xw_end_cursor(xw, bc, 1);
    switch(event.type) {
    case Expose:
      if(xw_expose(xw, &event))
	return xw_end_cursor(xw, bc, 1);
      break;
    case ButtonPress:
/*
 * Return the position at which the cursor was selected.
 */
      pos->x = event.xbutton.x;
      pos->y = event.xbutton.y;
/*
 * Return the key alias of the button that selected the cursor.
 */
      if(key) {
	switch(event.xbutton.button) {
	case Button1:
	  *key = 'A';
	  break;
	case Button2:
	  *key = 'D';
	  break;
	default:
	  *key = 'X';
	  break;
	};
      };
      finished = 1;
      break;
    case KeyPress:
      {
	char buffer[10];        /* Buffer to read key definition into */
	KeySym keysym;          /* Key code of pressed keyboard key */
	int nret;               /* The number of characters in buffer[] */
/*
 * Get the ASCII encoding associated with the key.
 */
	nret = XLookupString((XKeyEvent *)&event, buffer,
			   (int) (sizeof(buffer)/sizeof(char)), &keysym, NULL);
	if(xw->bad_device)
	  return xw_end_cursor(xw, bc, 1);
/*
 * Ignore modifier keys and all but single character keys.
 */
	if(nret==1 && (keysym < XK_Shift_L || keysym > XK_Hyper_R)) {
	  pos->x = event.xkey.x;
	  pos->y = event.xkey.y;
	  if(key)
	    *key = buffer[0];
	  finished = 1;
	};
/*
 * Check for arrow keys.
 */
	switch(keysym) {
#ifdef XK_KP_Left
	case XK_KP_Left:
#endif
	case XK_Left:
#ifdef XK_KP_Right
	case XK_KP_Right:
#endif
	case XK_Right:
#ifdef XK_KP_Up
	case XK_KP_Up:
#endif
	case XK_Up:
#ifdef XK_KP_Down
	case XK_KP_Down:
#endif
	case XK_Down:
	  if(xw_shift_cursor(xw, keysym, event.xkey.state))
	    return xw_end_cursor(xw, bc, 1);
	  break;
	};
      };
      break;
    case EnterNotify:
/*
 * The cursor may still be drawn if a button was pressed when the
 * cursor was last moved out of the window. The resulting
 * passive grab will have continued to deliver motion events to
 * the PGPLOT window.
 */
      if(xw_erase_cursor(xw, bc))
	return xw_end_cursor(xw, bc, 1);
/*
 * If the cursor is in the window, locate its position. If this is
 * the first time that the cursor has been in the window and warping
 * has been requested, this also inolves pre-positioning the cursor
 * and setting input focus.
 */
      if(xw_locate_cursor(xw, pos, posn && !warped, &last)) {
	warped = 1;
/*
 * Draw the cursor.
 */
	if(xw->bad_device || xw_bound_cursor(xw, &last) ||
	   xw_draw_cursor(xw, bc, &last))
	  return xw_end_cursor(xw, bc, 1);
      };
      break;
    case LeaveNotify:
      if(xw_erase_cursor(xw, bc))
	return xw_end_cursor(xw, bc, 1);
      break;
    case MotionNotify:
/*
 * Discard all but the last MotionNotify event.
 */
      while(xw_check_window_event(xw, xw->window, (long)(PointerMotionMask),
				  &event));
      if(xw->bad_device || xw_erase_cursor(xw, bc))
	return xw_end_cursor(xw, bc, 1);
      last.x = event.xmotion.x;
      last.y = event.xmotion.y;
      if(xw_bound_cursor(xw, &last) || xw_draw_cursor(xw, bc, &last))
	return xw_end_cursor(xw, bc, 1);
      break;
    default:
      break;
    };
  };
/*
 * Clean up.
 */
  return xw_end_cursor(xw, bc, xw->bad_device!=0);
}

/*.......................................................................
 * This is a private function of xw_read_cursor(). If the user has just
 * pressed one of the keyboard or keypad arrow keys, it moves the cursor
 * by one pixel in the corresponding direction. If one of the shift keys
 * is also held down, then the cursor is moved by ARROW_KEY_VELOCITY
 * pixels instead of one. If the resulting shift would move the cursor
 * out of the bounds of the pgplot window pixmap, then the motion is
 * aborted.
 *
 * Input:
 *  xw           XWdev *  The PGPLOT /xw device descriptor.
 *  keysym      KeySym    The key symbol returned by XLookupString() wrt
 *                        the arrow-button key-press.
 *  modifiers unsigned    The Event::xkey.state key-modifier mask
 *                        associated with the key-press.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
#ifdef __STDC__
static int xw_shift_cursor(XWdev *xw, KeySym keysym, unsigned int modifiers)
#else
static int xw_shift_cursor(xw, keysym, modifiers)
     XWdev *xw; KeySym keysym; unsigned int modifiers;
#endif
{
  Window p_child;         /* The child window that contains the pointer */
  int p_win_x, p_win_y;   /* The pointer coordinates wrt xw->window */
  int p_root_x, p_root_y; /* The pointer coordinates wrt the root window */
  Window p_root_win;      /* The root window that contains the cursor */
  unsigned int p_mask;    /* The bit-mask of button states etc.. */
  int dx=0;               /* The amount to move the cursor in X */
  int dy=0;               /* The amount to move the cursor in Y */
/*
 * Determine the current position of the cursor.
 */
  XQueryPointer(xw->display, xw->window, &p_root_win, &p_child,
		&p_root_x, &p_root_y, &p_win_x, &p_win_y, &p_mask);
  if(xw->bad_device)
    return 1;
/*
 * Work out the position increments in x and y.
 */
  switch(keysym) {
#ifdef XK_KP_Left
  case XK_KP_Left:
#endif
  case XK_Left:
    dx = -1;
    break;
#ifdef XK_KP_Right
  case XK_KP_Right:
#endif
  case XK_Right:
    dx = 1;
    break;
#ifdef XK_KP_Up
  case XK_KP_Up:
#endif
  case XK_Up:
    dy = -1;
    break;
#ifdef XK_KP_Down
  case XK_KP_Down:
#endif
  case XK_Down:
    dy = 1;
    break;
  default:
    return 0;
    break;
  };
/*
 * If one of the shift keys is held down, increase the size of the
 * move to ARROW_KEY_VELOCITY pixels in the specified direction.
 */
  if(modifiers & ShiftMask) {
    dx *= ARROW_KEY_VELOCITY;
    dy *= ARROW_KEY_VELOCITY;
  };
/*
 * Determine the final position of the pointer wrt the top left corner
 * of the window.
 */
  p_win_x += dx;
  p_win_y += dy;
/*
 * Abort the shift operation if the final position lies outside of the
 * bounds of the pgplot window pixmap. Note that this simple test doesn't
 * take account of the fact that another window may lie over the pgplot
 * window, or that the window may have been resized to a smaller size
 * than the pixmap. To do this properly one would have to perform the
 * move, then check for LeaveNotify events and put the cursor back if
 * one was detected. This would be hard to code without breaking
 * xw_read_cursor() which also wants LeaveNotify events, would be
 * slower to operate and would be unavoidably subject to race conditions.
 */
  if(p_win_x < 0 || p_win_x >= xw->geom.width ||
     p_win_y < 0 || p_win_y >= xw->geom.height)
    return 0;
/*
 * Move the cursor to the new location.
 */
  XWarpPointer(xw->display, None, xw->window, 0, 0, 0, 0, p_win_x, p_win_y);
  if(xw->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * Private return function of xw_read_cursor().
 *
 * Input:
 *  xw    XWdev *   The PGPLOT /xw device descriptor.
 *  bc     Band *   The cursor banding descriptor to be deleted.
 *  status  int     Required xw_read_cursor() return status.
 * Output:
 *  return  int     The value of 'status'.
 */
#ifdef __STDC__
static int xw_end_cursor(XWdev *xw, Band *bc, int status)
#else
static int xw_end_cursor(xw, bc, status)
     XWdev *xw; Band *bc; int status;
#endif
{
  if(bc) {
    if(xw_erase_cursor(xw, bc))
      status=1;
    if(xw_flush(xw))
      status=1;
    bc = xw_del_Band(xw, bc);
  };
  if(xw_set_cursor(xw,1))
    status=1;
  return status;
}

/*.......................................................................
 * Convert from the coordinates sent by PGPLOT in rbuf[...] to an
 * X-windows point in the coordinate system of the window. 
 *
 * Input:
 *  xw      XWdev *   The PGPLOT /xw device descriptor.
 *  xy      float [2] Array of two floats containing PGPLOT coordinates
 *                    arranged as x followed by y.
 * Output:
 *  xp     XPoint *   The converted coordinates will be assigned to xp->x
 *                    and xp->y.
 */
#ifdef __STDC__
static void xw_xy_to_XPoint(XWdev *xw, float *xy, XPoint *xp)
#else
static void xw_xy_to_XPoint(xw, xy, xp)
     XWdev *xw; float *xy; XPoint *xp;
#endif
{
  xp->x = xw->geom.xmin + (int)(xy[0] + 0.5);
  xp->y = xw->geom.ymax - (int)(xy[1] + 0.5);
}

/*.......................................................................
 * Convert from window pixel coordinates to PGPLOT coordinates, in a
 * form that can be returned to PGPLOT via rbuf[...].
 *
 * Input:
 *  xw      XWdev *   The PGPLOT /xw device descriptor.
 *  xp     XPoint *   The window pixel-coordinates to be converted.
 * Output:
 *  xy      float [2] Output array of two floats in which to place the
 *                    PGPLOT coordinates, arranged as x followed by y.
 */
#ifdef __STDC__
static void xw_XPoint_to_xy(XWdev *xw, XPoint *xp, float *xy)
#else
static void xw_XPoint_to_xy(xw, xp, xy)
     XWdev *xw; XPoint *xp; float *xy;
#endif
{
  xy[0] = (float) (xp->x - xw->geom.xmin);
  xy[1] = (float) (xw->geom.ymax - xp->y);
}

/*.......................................................................
 * Start a new page by clearing and possibly re-sizing the given /xw
 * window and its pixmap. If the current size of the window is equal
 * to the requested new size, then only the window clearing operation
 * will be performed.
 *
 * Input:
 *  xw        XWdev * The PGPLOT /xw device descriptor of the window to be
 *                    resized.
 *  width  unsigned   The new width for the re-sized window (pixels).
 *  height unsigned   The new height for the re-sized window (pixels).
 * Output:
 *  return  int       0 - OK.
 *                    1 - Error.
 */
#ifdef __STDC__
static int xw_next_page(XWdev *xw, unsigned int width, unsigned int height)
#else
static int xw_next_page(xw, width, height)
     XWdev *xw; unsigned int width; unsigned int height;
#endif
{
  int need_resize;  /* True if the current pixmap size needs to be changed */
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * Does the pixmap need to be resized?
 */
  need_resize = width != xw->geom.width || height != xw->geom.height;
/*
 * Establish the new geometry with the server.
 */
  if(need_resize) {
    if(xw_new_geom(xw, 0,0, width,height, (WidthValue|HeightValue)))
      return 1;
  };
/*
 * Reset the colormap color representations if necessary.
 */
  if(!xw->color.initialized && xw_init_colors(xw))
    return 1;
/*
 * Allocate a new pixmap?
 */
  if(xw->pixmap==None || need_resize) {
    xw_get_pixmap(xw);
    if(xw->bad_device)
      return 1;
/*
 * If a new pixmap is not required, simply clear the window and pixmap.
 */
  } else {
    if(xw_clear(xw))
      return 1;
/*
 * Also ensure that the window has the same size as the pixmap.
 */
    XResizeWindow(xw->display, xw->window, xw->geom.width, xw->geom.height);
  };
  return 0;
}

/*.......................................................................
 * Draw a horizontal line of pixels at a given location, from a float
 * array of PGPLOT color indexes.
 *
 * Input:
 *  xw      XWdev *   The PGPLOT /xw device descriptor.
 *  start  XPoint *   The position to start the line at.
 *  cells   float *   An array of ncell pixel PGPLOT color indexes.
 *  ncell     int     The number of cells in cells[].
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
#ifdef __STDC__
static int xw_image_line(XWdev *xw, XPoint *start, float *cells, int ncell)
#else
static int xw_image_line(xw, start, cells, ncell)
     XWdev *xw; XPoint *start; float *cells; int ncell;
#endif
{
  int ndone;  /* The number of pixels drawn so far */
  int i;
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * Quietly ignore the call if we don't have a pixmap.
 */
  if(xw->pixmap != None) {
/*
 * Draw up to xw->image.npix pixels at a time. This is the size of the
 * xw->image.buff[] buffer.
 */
    for(ndone=0; !xw->bad_device && ndone<ncell; ndone += XW_IMAGE_LEN) {
      int ntodo = ncell-ndone;
      int nimage = ntodo < XW_IMAGE_LEN ? ntodo : XW_IMAGE_LEN;
/*
 * Load the image buffer with the color cell indexes assigned to the
 * given PGPLOT color indexes.
 */
      if(xw->color.vi->depth == 8) {
	for(i=0; i<nimage; i++)
	  xw->image.xi->data[i] = xw->color.pixel[(int)(cells[ndone+i] + 0.5)];
      } else {
	for(i=0; i<nimage; i++) {
	  XPutPixel(xw->image.xi, i, 0,
		    xw->color.pixel[(int) (cells[ndone+i] + 0.5)]);
	};
      };
/*
 * Display the image.
 */
      XPutImage(xw->display, xw->pixmap, xw->gc, xw->image.xi, 0, 0,
		start->x+ndone, start->y, (unsigned) nimage, (unsigned) 1);
    };
/*
 * Extend the region to be updated on the next flush.
 */
    xw_mark_modified(xw, start->x, start->y, 1);
    xw_mark_modified(xw, start->x + ncell - 1, start->y, 1);
  };
  if(xw->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * Call this function when an Expose event is received. It will then
 * re-draw the exposed region from the xw->pixmap.
 *
 * Input:
 *  xw       XWdev *  The PGPLOT /xw device descriptor.
 *  event   XEvent *  The expose event.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
#ifdef __STDC__
static int xw_expose(XWdev *xw, XEvent *event)
#else
static int xw_expose(xw, event)
     XWdev *xw; XEvent *event;
#endif
{
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
  if(event->type == Expose && xw->pixmap != None) {
    XCopyArea(xw->display, xw->pixmap, xw->window, xw->gc,
	      event->xexpose.x, event->xexpose.y,
	      (unsigned) event->xexpose.width, (unsigned) event->xexpose.height,
	      event->xexpose.x, event->xexpose.y);
    if(xw->bad_device)
      return 1;
    XFlush(xw->display);
    if(xw->bad_device)
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Set up for a band cursor and return its descriptor.
 *
 * Input:
 *  xw    XWdev *   The PGPLOT /xw device descriptor.
 *  mode    int     0 - No band cursor.
 *                  1 - Line between reference position and cursor.
 *                  2 - Rectangle drawn with opposite corners at reference
 *                      and cursor position.
 *  ref  XPoint *   The reference position.
 * Output:
 *  return Band *   A pointer to a static internal container of cursor
 *                  resources. Call xw_del_Band() to release these resources
 *                  and return the event mask to normal.
 */
#ifdef __STDC__
static Band *xw_new_Band(XWdev *xw, int mode, XPoint *ref)
#else
static Band *xw_new_Band(xw, mode, ref)
     XWdev *xw; int mode; XPoint *ref;
#endif
{
  static Band band;        /* Return container */
  long event_mask=0;       /* Bit map of events to be caught */
/*
 * Device error?
 */
  if(xw->bad_device)
    return NULL;
/*
 * Initialize values at least to the point at which xw_del_Band() can
 * safely be called.
 */
  band.line_width = xw->gcv.line_width;
  band.mode = mode;
  band.ref = *ref;
  band.end = *ref;
/*
 * All cursor types require us to catch the following events.
 */
  event_mask = ExposureMask | KeyPressMask | ButtonPressMask |
               EnterWindowMask | LeaveWindowMask;
/*
 * Set up for a band cursor?
 */
  if(band.mode != 0 || xw->crosshair) {
/*
 * Arrange for the band cursor to be drawn with a line width of 0.
 */
    if(band.line_width != 0) {
      XGCValues attr;
      band.line_width = attr.line_width = 0;
      XChangeGC(xw->display, xw->gc, (unsigned long) GCLineWidth, &attr);
      if(xw->bad_device)
	return NULL;
    };
/*
 * Select for cursor motion events along with the normal events.
 */
      event_mask |= PointerMotionMask;
  };
/*
 * Register the additional event types that are now to be caught.
 */
  if(xw_add_events(xw, event_mask))
    return NULL;
  return &band;
}

/*.......................................................................
 * Release band cursor resources and return the window event mask to
 * its normal state.
 *
 * Input:
 *  xw    XWdev *   The PGPLOT /xw device descriptor.
 *  bc     Band *   The band-cursor descriptor.
 * Output:
 *  return Band *   Always NULL.
 */
#ifdef __STDC__
static Band *xw_del_Band(XWdev *xw, Band *bc)
#else
static Band *xw_del_Band(xw, bc)
     XWdev *xw; Band *bc;
#endif
{
/*
 * Prevent the event buffer from overflowing by removing superflous events
 * from the set of those to be caught.
 */
  xw_rem_events(xw, (long) (ExposureMask | KeyPressMask | ButtonPressMask |
			    EnterWindowMask | LeaveWindowMask |
			    PointerMotionMask));
/*
 * If the line width was changed for rubber banding, re-instate the
 * original line width.
 */
  if(bc->line_width != xw->gcv.line_width)
    XChangeGC(xw->display, xw->gc, (unsigned long) GCLineWidth, &xw->gcv);
  return NULL;
}

/*.......................................................................
 * Trim a coordinate to lie within the current pixmap and window area.
 * This prevents the cursor being displayed or returned for a position
 * outside the currently visible window and pixmap areas.
 *
 * Input:
 *  xp    XPoint *   The point to be limited.
 * Output:
 *  return   int     0 - OK.
 *                   1 - Error.
 */
#ifdef __STDC__
static int xw_bound_cursor(XWdev *xw, XPoint *xp)
#else
static int xw_bound_cursor(xw, xp)
     XWdev *xw; XPoint *xp;
#endif
{
  XWindowAttributes attr;  /* Current window attributes */
  int xmax, ymax;          /* Max usable X and Y coordinates */
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * Get the current window dimensions.
 */
  XGetWindowAttributes(xw->display, xw->window, &attr);
  if(xw->bad_device)
    return 1;
/*
 * With NorthWest pixmap gravity, coordinates 0,0 are always visible at the
 * top left corner of the plot.
 */
  if(xp->x < 0)
    xp->x = 0;
  if(xp->y < 0)
    xp->y = 0;
/*
 * Determine the max X and Y coordinates that fall both within the pixmap and
 * the window.
 */
  xmax = ((xw->geom.width < attr.width) ? xw->geom.width : attr.width) - 1;
  ymax = ((xw->geom.height < attr.height) ? xw->geom.height : attr.height) - 1;
/*
 * Limit the coordinates to the above range.
 */
  if(xp->x > xmax)
    xp->x = xmax;
  if(xp->y > ymax)
    xp->y = ymax;
  return 0;
}

/*.......................................................................
 * Draw the current cursor.
 *
 * Input:
 *  xw   XWdev *   The PGPLOT /xw device descriptor.
 *  bc    Band *   A cursor descriptor returned by xw_new_band().
 *  end XPoint *   The current cursor position.
 * Output:
 *  return int     0 - OK.
 *                 1 - Error.
 */
#ifdef __STDC__
static int xw_draw_cursor(XWdev *xw, Band *bc, XPoint *end)
#else
static int xw_draw_cursor(xw, bc, end)
     XWdev *xw; Band *bc; XPoint *end;
#endif
{
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * Store the new end point.
 */
  bc->end = *end;
/*
 * Draw the cursor.
 */
  switch(bc->mode) {
  case 0: default:
    if(xw->gc!=NULL && xw->crosshair) {
      XDrawLine(xw->display, xw->window, xw->gc, 0, bc->end.y,
		(int)xw->geom.width-1, bc->end.y);
      if(xw->bad_device)
	return 1;
      XDrawLine(xw->display, xw->window, xw->gc, bc->end.x, 0,
		bc->end.x, (int)xw->geom.height-1);
    };
    break;
  case 1:
    XDrawLine(xw->display, xw->window, xw->gc, bc->ref.x, bc->ref.y,
	      bc->end.x, bc->end.y);
    break;
  case 2:  /* Draw a rectangle */
    {
      int x = bc->ref.x < bc->end.x ? bc->ref.x : bc->end.x;
      int y = bc->ref.y < bc->end.y ? bc->ref.y : bc->end.y;
      unsigned int width = (unsigned int) abs(bc->ref.x - bc->end.x);
      unsigned int height = (unsigned int) abs(bc->ref.y - bc->end.y);
      XDrawRectangle(xw->display, xw->window, xw->gc, x, y, width, height);
    };
    break;
  case 3:  /* Two horizontal lines */
    XDrawLine(xw->display, xw->window, xw->gc, 0, bc->end.y,
	      (int)xw->geom.width-1, bc->end.y);
    if(xw->bad_device)
      return 1;
    XDrawLine(xw->display, xw->window, xw->gc, 0, bc->ref.y,
	      (int)xw->geom.width-1, bc->ref.y);
    break;
  case 4:  /* Two vertical lines */
    XDrawLine(xw->display, xw->window, xw->gc, bc->end.x, 0,
	      bc->end.x, (int)xw->geom.height-1);
    if(xw->bad_device)
      return 1;
    XDrawLine(xw->display, xw->window, xw->gc, bc->ref.x, 0,
	      bc->ref.x, (int)xw->geom.height-1);
    break;
  case 5: /* One horizontal line through the cursor */
    XDrawLine(xw->display, xw->window, xw->gc, 0, bc->end.y,
	      (int)xw->geom.width-1, bc->end.y);
    break;
  case 6: /* One vertical line through the cursor */
    XDrawLine(xw->display, xw->window, xw->gc, bc->end.x, 0,
	      bc->end.x, (int)xw->geom.height-1);
    break;
  case 7: /* Cross hair */
    XDrawLine(xw->display, xw->window, xw->gc, 0, bc->end.y,
	      (int)xw->geom.width-1, bc->end.y);
    if(xw->bad_device)
      return 1;
    XDrawLine(xw->display, xw->window, xw->gc, bc->end.x, 0,
	      bc->end.x, (int)xw->geom.height-1);
    break;
  };
  if(xw->bad_device)
    return 1;
  XFlush(xw->display);
  if(xw->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * Erase a previously drawn cursor.
 *
 * Input:
 *  xw   XWdev *   The PGPLOT /xw device descriptor.
 *  bc    Band *   A cursor descriptor returned by xw_new_band().
 * Output:
 *  return int     0 - OK.
 *                 1 - Error.
 */
#ifdef __STDC__
static int xw_erase_cursor(XWdev *xw, Band *bc)
#else
static int xw_erase_cursor(xw, bc)
     XWdev *xw; Band *bc;
#endif
{
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * Erase the cursor.
 */
  switch(bc->mode) {
  case 0: default:
    if(xw->crosshair) {
      if(xw_cursor_line(xw, 0, bc->end.y, (int)xw->geom.width-1,bc->end.y) ||
	 xw_cursor_line(xw, bc->end.x, 0, bc->end.x, (int)xw->geom.height-1))
	return 1;
    };
    break;
  case 1:   /* Line cursor */
    if(xw_cursor_line(xw, bc->ref.x, bc->ref.y, bc->end.x, bc->end.y))
      return 1;
    break;
  case 2:   /* Rectangle cursor */
    if(xw_cursor_line(xw, bc->ref.x, bc->ref.y, bc->ref.x, bc->end.y) ||
       xw_cursor_line(xw, bc->ref.x, bc->end.y, bc->end.x, bc->end.y) ||
       xw_cursor_line(xw, bc->end.x, bc->end.y, bc->end.x, bc->ref.y) ||
       xw_cursor_line(xw, bc->end.x, bc->ref.y, bc->ref.x, bc->ref.y))
      return 1;
    break;
  case 3:  /* Two horizontal lines */
    if(xw_cursor_line(xw, 0, bc->end.y, (int)xw->geom.width-1,bc->end.y) ||
       xw_cursor_line(xw, 0, bc->ref.y, (int)xw->geom.width-1,bc->ref.y))
      return 1;
    break;
  case 4:  /* Two vertical lines */
    if(xw_cursor_line(xw, bc->end.x, 0, bc->end.x, (int)xw->geom.height-1) ||
       xw_cursor_line(xw, bc->ref.x, 0, bc->ref.x, (int)xw->geom.height-1))
      return 1;
    break;
  case 5: /* One horizontal line through the cursor */
    if(xw_cursor_line(xw, 0, bc->end.y, (int)xw->geom.width-1,bc->end.y))
      return 1;
    break;
  case 6: /* One vertical line through the cursor */
    if(xw_cursor_line(xw, bc->end.x, 0, bc->end.x, (int)xw->geom.height-1))
      return 1;
    break;
  case 7: /* Cross hair */
    if(xw_cursor_line(xw, 0, bc->end.y, (int)xw->geom.width-1,bc->end.y) ||
       xw_cursor_line(xw, bc->end.x, 0, bc->end.x, (int)xw->geom.height-1))
      return 1;
    break;
  };
  return 0;
}

/*.......................................................................
 * Restore the pixels under a given line.
 *
 * Input:
 *  xw    XWdev *  The PGPLOT /xw device descriptor.
 *  xa, ya  int    The start pixel of the line.
 *  xb, yb  int    The end pixel of the line.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
#ifdef __STDC__
static int xw_cursor_line(XWdev *xw, int xa, int ya, int xb, int yb)
#else
static int xw_cursor_line(xw, xa, ya, xb, yb)
     XWdev *xw; int xa; int ya; int xb; int yb;
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
  if(xw->bad_device)
    return 1;
/*
 * Silently ignore the call if a pixmap is not available.
 */
  if(xw->pixmap != None) {
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
      XCopyArea(xw->display, xw->pixmap, xw->window, xw->gc, xmin, ymin,
		(unsigned) 1, (unsigned) (ymax-ymin+1), xmin, ymin);
    }
/*
 * Horizontal line?
 */
    else if(ylen==0) {
      XCopyArea(xw->display, xw->pixmap, xw->window, xw->gc, xmin, ymin,
		(unsigned) (xmax-xmin+1), (unsigned) 1, xmin, ymin);
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
	XCopyArea(xw->display, xw->pixmap, xw->window, xw->gc,
		  x, y1, (unsigned) (PIXINC+1), (unsigned) (y2-y1+1), x, y1);
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
	XCopyArea(xw->display, xw->pixmap, xw->window, xw->gc,
		  x1, y, (unsigned) (x2-x1+1), (unsigned) (PIXINC+1), x1, y);
      };
    };
  };
/*
 * Check for device errors.
 */
  if(xw->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * Add to the set of events to be caught.
 *
 * Input:
 *  xw    XWdev *  The PGPLOT /xw device descriptor.
 *  events long    The bit mask of events to be added to those already
 *                 being selected.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
#ifdef __STDC__
static int xw_add_events(XWdev *xw, long events)
#else
static int xw_add_events(xw, events)
     XWdev *xw; long events;
#endif
{
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * Get the union of the new events with the current event mask.
 */
  xw->event.mask |= events;
/*
 * Register the modified selection with the server.
 */
  XSync(xw->display, False);
  if(xw->bad_device)
    return 1;
  xw->last_error = 0;
  XSelectInput(xw->display, xw->window, xw->event.mask);
  if(xw->bad_device)
    return 1;
/*
 * Force the new selections through to the server and check for access
 * errors that indicate that ButtonPress's are currently selected by
 * another client.
 */
  XSync(xw->display, False);
  if(xw->bad_device)
    return 1;
  if(xw->last_error == BadAccess) {
/*
 * Only one client can select ButtonPress events. If another client
 * already has them selected, then the above XSelectInputs() will have
 * generated an error and the event mask will not have been installed.
 */
    if(xw->event.mask & ButtonPressMask) {
      if(!xw->event.no_buttons) {
	fprintf(stderr,
		"%s: Failed to acquire pointer buttons - use keys A,D,X.\n",
		XW_IDENT);
      };
      xw->event.no_buttons = 1;
    };
/*
 * Retry, but with the events that could have caused the BadAccess removed.
 */
    xw->event.mask &= ~(SubstructureRedirectMask | ResizeRedirectMask |
			ButtonPressMask);
    XSelectInput(xw->display, xw->window, xw->event.mask);
    if(xw->bad_device)
      return 1;
    XSync(xw->display, False);
    if(xw->bad_device)
      return 1;
  };
/*
 * Have we successfully acquired ButtonPress events?
 */
  if(xw->event.mask & ButtonPressMask)
    xw->event.no_buttons = 0;
  return 0;
}

/*.......................................................................
 * Remove selected events from the set of events to be caught.
 *
 * Input:
 *  xw    XWdev *  The PGPLOT /xw device descriptor.
 *  events long    The bit mask of events to be removed from the set
 *                 being selected.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
#ifdef __STDC__
static int xw_rem_events(XWdev *xw, long events)
#else
static int xw_rem_events(xw, events)
     XWdev *xw; long events;
#endif
{
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * Clear the bits in our current event mask that correspond to the events
 * to be removed.
 */
  xw->event.mask &= ~events;
/*
 * Register the modified selection with the server.
 */
  XSelectInput(xw->display, xw->window, xw->event.mask);
  if(xw->bad_device)
    return 1;
  XSync(xw->display, False);
  if(xw->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * This function is called by X whenever a non-fatal error occurs
 * on a given display connection.
 *
 * Input:
 *  display    Display *  The display connection on which the error occured.
 *  event  XErrorEvent *  The descriptor of the error event.
 * Output:
 *  return         int    The return value is not specified by Xlib, so
 *                        we will simply return 0.
 */
#ifdef __STDC__
static int xw_error(Display *display, XErrorEvent *event)
#else
static int xw_error(display, event)
     Display *display; XErrorEvent *event;
#endif
{
  char errtxt[81]; /* Buffer to receive error message in */
  XWdev *xw;
/*
 * Find the device that is the source of the error.
 */
  for(xw=device_list; xw!=NULL && xw->display!=display; xw = xw->next);
/*
 * If a device was located, check if the error implies that server resources
 * have become unusable for that device.
 */
  if(xw && !xw->bad_device) {
    xw->last_error = event->error_code;
    switch(event->error_code) {
    case BadAtom: case BadColor: case BadCursor: case BadDrawable:
    case BadGC: case BadIDChoice: case BadPixmap: case BadWindow:
/*
 * Get a message describing the error.
 */
      XGetErrorText(display, (int)event->error_code,errtxt,(int)sizeof(errtxt));
      fprintf(stderr, "%s: XErrorEvent: %s\n", XW_IDENT, errtxt);
/*
 * Report the operation that caused it. These opcode numbers are listed in
 * <X11/Xproto.h>.
 */
      fprintf(stderr, "%s: Major opcode of failed request: %d\n", XW_IDENT,
	      (int) event->request_code);
/*
 * Report the loss of the window and mark the device as unusable.
 */
      xw_bad_device(xw);
      break;
    };
  };
  return 0;
}

/*.......................................................................
 * If a PGPLOT /xw server has not already been started, start one.
 * Return the ID of the server communication window.
 *
 * Input:
 *  xw      XWdev *  The PGPLOT /xw device descriptor.
 * Output:
 *  return Window    The XID of the server communication window, or None
 *                   on error.
 */
#ifdef __STDC__
static Window xw_get_server(XWdev *xw)
#else
static Window xw_get_server(xw)
     XWdev *xw;
#endif
{
  int i;
/*
 * See if a server has already been started.
 */
  xw->server = XGetSelectionOwner(xw->display, xw->server_atom);
/*
 * Start a new server if necessary.
 */
  if(xw->server == None) {
    char *exe=NULL;          /* The name of the pgxwin_server executable */
    char *command=NULL;      /* The command-line string used to run it */
    char *display_string;    /* The display-name string */
    unsigned long slen;      /* Length of command line excluding '\0' */
    int waserr = 0;          /* Set to true if an error occurs */
/*
 * Describe the format of the command line.
 */
#ifdef VMS
    char *format = "%s -display %s";
#else
    char *format = "%s -display %s </dev/null &";
#endif
/*
 * Determine the name of the display that the user selected.
 */
    display_string = DisplayString(xw->display);
/*
 * Locate the server program.
 */
#ifdef VMS
    if((exe=find_exe("PGPLOT_DIR", PGXWIN_SERVER))==NULL) {
      fprintf(stderr,"%s: Failed to find \"pgplot_dir:%s.exe\".\n",
	      XW_IDENT, PGXWIN_SERVER);
      return None;
    };
#else
    if((exe=find_exe(getenv("PGPLOT_DIR"), PGXWIN_SERVER))==NULL &&
       (exe=find_exe(getenv("PATH"), PGXWIN_SERVER))==NULL) {
      fprintf(stderr,
	  "%s: Couldn't find program \"%s\" in the directory named\n",
	  XW_IDENT, PGXWIN_SERVER);
      fprintf(stderr,
	  "%s: in your PGPLOT_DIR environment variable, or in any directory\n",
	  XW_IDENT);
      fprintf(stderr,
	      "%s: listed in your PATH environment variable.\n",XW_IDENT);
      return None;
    };
#endif
/*
 * Make it possible to determine which server is being started.
 */
    if(getenv("PGPLOT_XW_DEBUG"))
      printf("Starting %s.\n", exe);
/*
 * Determine the length of the comand-line string required, as defined by:
 * sprintf(command, format, PGXWIN_SERVER, display_string)
 */
    slen = strlen(format) + strlen(exe) + strlen(display_string);
    command = (char *) malloc(sizeof(char) * (slen + 1));
    if(command==NULL) {
      fprintf(stderr, "%s: Insufficient memory to run %s.\n", XW_IDENT, exe);
      waserr = 1;
    } else {
/*
 * Compile the command-line string.
 */
      sprintf(command, format, exe, display_string);
/*
 * Spawn the server.
 */
#ifdef VMS
      waserr = vms_spawn_nowait(command);
#else
/*
 * Stipulate that the existing socket connection to the server be closed
 * on the following exec(). This prevents the child from holding the
 * connection open when the parent terminates.
 */
      fcntl(ConnectionNumber(xw->display), F_SETFD, 1);
/*
 * Run the server.
 */
      system(command);
#endif
    };
/*
 * Release the malloc'd command line string.
 */
    if(command)
      free(command);
/*
 * Check once per second for up to XW_SERVER_TIMEOUT seconds for the
 * server to start.
 */
    if(!waserr) {
      for(i=0; xw->server==None && i<XW_SERVER_TIMEOUT; i++) {
	sleep(1);
	if(i==3)
	  printf("%s: Waiting for %s to start (timeout in %d seconds).\n",
		 XW_IDENT, exe, XW_SERVER_TIMEOUT-i);
	xw->server = XGetSelectionOwner(xw->display, xw->server_atom);
      };
/*
 * Contact with server not acheived?
 */
      if(xw->server == None) {
	fprintf(stderr, "%s: Timed out waiting for program %s to start\n",
		XW_IDENT, exe);
      };
    };
/*
 * Discard the string that contained the name of the server executable.
 */
    if(exe)
      free(exe);
  };
  return xw->server;
}

/*.......................................................................
 * Send a ClientMessage query to the server and read its reply.
 *
 * Input:
 *  xw      XWdev *  The PGPLOT /xw device descriptor.
 * Input/Output:
 *  event  XEvent *  The input ClientMessage event.
 *                    On input set:
 *                      event.xclient.message_type = Type of message.
 *                      event.xclient.data.l[0..4] = Message data.
 *                    On output:
 *                      event.xclient.data.l[0..4] = Reply data.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Error.
 */
#ifdef __STDC__
static int xw_query_server(XWdev *xw, XEvent *event)
#else
static int xw_query_server(xw, event)
     XWdev *xw; XEvent *event;
#endif
{
/*
 * Device error?
 */
  if(xw->bad_device)
    return 1;
/*
 * Initialize the generic parts of the event descriptor.
 */
  event->type = ClientMessage;
  event->xclient.window = xw->client;
  event->xclient.format = 32;
  if(!XSendEvent(xw->display, xw->server, False, (long)0, event) ||
     xw->bad_device) {
    fprintf(stderr, "%s: Error talking to PGPLOT /xw server.\n", XW_IDENT);
    return 1;
  };
  XFlush(xw->display);
  if(xw->bad_device)
    return 1;
/*
 * Read the server's reply.
 */
  do {
    if(xw_next_event(xw, event))
      return 1;
  } while(event->type != ClientMessage || event->xclient.window != xw->client);
/*
 * A returned message type of None denotes an error.
 */
  if(event->xclient.message_type == None)
    return 1;
  return 0;
}

/*.......................................................................
 * Front end to XNextEvent() to get the next event from the X server,
 * while checking for DestroyNotify events on the PGPLOT window.
 *
 * Input:
 *  xw      XWdev *  The PGPLOT /xw device descriptor.
 * Input/Output:
 *  event  XEvent *  The event structure for the returned event.
 * Output:
 *  return    int    0 - OK.
 *                   1 - The PGPLOT window has been destroyed.
 */
#ifdef __STDC__
static int xw_next_event(XWdev *xw, XEvent *event)
#else
static int xw_next_event(xw, event)
     XWdev *xw; XEvent *event;
#endif
{
/*
 * Check that we still have a window.
 */
  if(xw->bad_device)
    return 1;
/*
 * Wait for the next event.
 */
  XNextEvent(xw->display, event);
  switch(event->type) {
  case DestroyNotify:
    if(event->xdestroywindow.window == xw->window)
      return xw_bad_device(xw);
  };
  return 0;
}

/*.......................................................................
 * Front end to XCheckWindowEvent() to check and return for the next event
 * that matches the given event_mask, without blocking if no matching event
 * is there and while also checking for DestroyNotify events on said window.
 *
 * Input:
 *  xw        XWdev *  The PGPLOT /xw device descriptor.
 *  window   Window    The window on which to watch for events.
 *  event_mask long    The bit mask of event types wanted.
 * Input/Output:
 *  event    XEvent *  The event structure for the returned event.
 * Output:
 *  return      int    0 - No matching event.
 *                     1 - Got an event that matches the mask.
 */
#ifdef __STDC__
static int xw_check_window_event(XWdev *xw, Window window, long event_mask,
				 XEvent *event)
#else
static int xw_check_window_event(xw, window, event_mask, event)
     XWdev *xw; Window window; long event_mask; XEvent *event;
#endif
{
  int want_structure = 0;  /* True if the caller selects StructureNotifyMask */
/*
 * Check that we still have a window.
 */
  if(xw->bad_device)
    return 1;
/*
 * Did the user also want StructureNotifyMask events?
 */
  want_structure = event_mask & StructureNotifyMask;
/*
 * We also want DestroyNotify events.
 */
  event_mask |= StructureNotifyMask;
/*
 * Wait for the next event.
 */
  while(XCheckWindowEvent(xw->display, window, event_mask, event)==True) {
    switch(event->type) {
    case DestroyNotify:
      if(window == xw->window) {  /* Have we lost the plot window? */
	xw_bad_device(xw);
	return want_structure;
      } else if(want_structure) {
	return 1;
      };
      break;
    case CirculateNotify:
    case ConfigureNotify:
      if(want_structure)  /* Ignore unselected StructureNotifyMask events */
	return 1;
      break;
    default:
      return 1;  /* One of the requested events was found */
    };
  };
  return 0;
}

/*.......................................................................
 * Insert a new PGPLOT /xw device descriptor onto the list of open
 * devices. The descriptor is inserted such that the list is maintained
 * in order of window number xw->number.
 *
 * Input:
 *  xw      XWdev *  The PGPLOT /xw device descriptor to be inserted.
 * Output:
 *  return  XWdev *  The same as the input.
 */
#ifdef __STDC__
static XWdev *xw_insert_device(XWdev *xw)
#else
static XWdev *xw_insert_device(xw)
     XWdev *xw;
#endif
{
  XWdev *prev;   /* Pointer to previous device in list */
  XWdev *next;   /* Pointer to next device in list */
/*
 * Find the correct position for the device in the device list.
 */
  prev = NULL;
  next = device_list;
  while(next && next->number > xw->number) {
    prev = next;
    next = next->next;
  };
/*
 * Insert the device between 'prev' and 'next'.
 */
  xw->next = next;
  if(prev==NULL)
    device_list = xw;
  else
    prev->next = xw;
  return xw;
}

/*.......................................................................
 * Remove a given PGPLOT /xw device descriptor from the list of open
 * devices.
 *
 * Input:
 *  xw      XWdev *  The PGPLOT /xw device descriptor to be removed.
 * Output:
 *  return  XWdev *  The removed descriptor.
 */
#ifdef __STDC__
static XWdev *xw_remove_device(XWdev *xw)
#else
static XWdev *xw_remove_device(xw)
     XWdev *xw;
#endif
{
  XWdev *prev;   /* Pointer to previous device in list */
  XWdev *next;   /* Pointer to next device in list */
/*
 * Find the position of the device in the device list.
 */
  prev = NULL;
  next = device_list;
  while(next && next!=xw) {
    prev = next;
    next = next->next;
  };
/*
 * Relink around the window if it was found.
 */
  if(next) {
    if(prev==NULL)
      device_list = next->next;
    else
      prev->next = next->next;
  };
/*
 * The descriptor is no longer in a list.
 */
  xw->next = NULL;
  return xw;
}

/*.......................................................................
 * Select a given device by its PGPLOT window number: xw->number.
 *
 * Input:
 *  number      int   The device number to search for.
 * Output:
 *  return    XWdev * The descriptor of the located device, or NULL
 *                    on error.
 */
#ifdef __STDC__
static XWdev *xw_select_device(int number)
#else
static XWdev *xw_select_device(number)
     int number;
#endif
{
/*
 * Search for the cited device.
 */
  XWdev *xw = device_list;
  while(xw && xw->number != number)
    xw = xw->next;
  if(xw==NULL || xw->number!=number) {
    fprintf(stderr, "%s: No such device (%d).\n", XW_IDENT, number);
    return NULL;
  };
  return xw;
}

/*.......................................................................
 * Wait for data to become available on the xw->client_data property
 * and read it into the given buffer, after performing any necessary
 * data-type conversions between different sized integers.
 *
 * Input:
 *  xw           XWdev * The PGPLOT /xw device descriptor.
 *  data          char * The buffer to return the data in, cast to (char *).
 *  form           int   The format for the property. Recognised values and
 *                       the data types used to accept them in data[] are:
 *                         XW_CHAR_PROP  -  (char)
 *                         XW_SHORT_PROP -  (short)
 *                         XW_LONG_PROP  -  (long)
 *  n    unsigned long   The number of items to be returned in data[].
 * Output:
 *  return unsigned long The number of items read, or 0 on error.
 */
#ifdef __STDC__
static unsigned long xw_get_data(XWdev *xw, char *data, int form,
				 unsigned long n)
#else
static unsigned long xw_get_data(xw, data, form, n)
     XWdev *xw; char *data; int form; unsigned long n;
#endif
{
  XEvent event;         /* Used to check for property-notify events */
  unsigned long ndone;  /* The number of items read so far */
  unsigned long nread;  /* The number of items read in the latest iteration */
  Atom ret_type;        /* Returned data-type */
  int ret_form;         /* Returned data-format */
  unsigned long nret;   /* Number of elements returned */
  unsigned long nleft;  /* Number of bytes unread */
  unsigned char *prop;  /* Property value */
  unsigned long size;   /* Size of property data element */
/*
 * Device error?
 */
  if(xw->bad_device)
    return 0;
/*
 * The property data returned by XGetWindowProperty is arranged as an array of
 * (char) if form=8, (short) if form=16, and (long) if form=32,
 * irrespective of the sizes of these types. Get the size of one such
 * element in bytes.
 */
  switch(form) {
  case XW_CHAR_PROP:
    size = sizeof(char);
    break;
  case XW_SHORT_PROP:
    size = sizeof(short);
    break;
  case XW_LONG_PROP:
    size = sizeof(long);
    break;
  default:
    fprintf(stderr, "%s: Unkown property format: %d\n", XW_IDENT, form);
    xw_bad_device(xw);
    return 0;
    break;
  };
/*
 * The property data may not appear in one go, so it may take a
 * few iterations to get all the data. The server signals completion
 * by sending a 0-length property.
 */
  ndone = nread = 0;
  do {
/*
 * Wait for the property to be updated.
 */
    do {
      if(xw_next_event(xw, &event))
	return 0;
    } while(!(event.type == PropertyNotify &&
	      event.xproperty.window == xw->client &&
	      event.xproperty.atom   == xw->client_data &&
	      event.xproperty.state  == PropertyNewValue));
/*
 * Determine the format of the data stored in the property but defer
 * reading the data, by asking for 0 items.
 */
    if(XGetWindowProperty(xw->display, xw->client, xw->client_data,
			  (long)0, (long)0, False, AnyPropertyType, &ret_type,
			  &ret_form, &nret, &nleft, &prop) != Success) {
      fprintf(stderr, "%s: Error reading property.\n", XW_IDENT);
      xw_bad_device(xw);
      return 0;
    } else {
/*
 * Delete the copied 0-length (+1 byte Xlib added padding) property value.
 */
      XFree((char *) prop);
/*
 * Make sure that the property has the expected type.
 */
      if(ret_form != form) {
	fprintf(stderr, "%s: Inconsistent property format.\n", XW_IDENT);
	xw_bad_device(xw);
	return 0;
      };
/*
 * Since XGetWindowProperty requires one to specify the amount to be
 * read in multiples of 4 8-bit bytes, round-up 'nleft' to the nearest
 * 4-byte multiple >= nleft.
 */
      nleft = 4 * ((nleft+3)/4);
      if(nleft > 0) {
/*
 * Read the property value.
 */
	if(XGetWindowProperty(xw->display, xw->client, xw->client_data, (long)0,
			      (long)nleft/4, False, ret_type, &ret_type,
			      &ret_form, &nread, &nleft, &prop) != Success) {
	  fprintf(stderr, "%s: Error reading property.\n", XW_IDENT);
	  xw_bad_device(xw);
	  return 0;
	} else {
/*
 * Accumulate up to n items in the output data array.
 */
	  if(ndone < n) {
	    unsigned long ncopy = (ndone+nread<=n) ? nread : (n-ndone);
	    unsigned long icopy;
	    for(icopy=0; icopy < ncopy*size; icopy++)
	      data[ndone*size+icopy] = ((char *)prop)[icopy];
	    ndone += ncopy;
	  };
/*
 * Delete the property data buffer.
 */
	  XFree((char *)prop);
	};
      } else {
	nread = 0;  /* 0-length property detected. */
      };
/*
 * Delete the property, both to release resources and to signal completion
 * to the server.
 */
      XDeleteProperty(xw->display, xw->client, xw->client_data);
    };
/*
 * The server signals that there is no more data to be read by sending
 * a zero-length property. Don't stop reading until this has been
 * detected, even if all expected data have been read.
 */
  } while(nread>0);
/*
 * Nothing read?
 */
  if(n!=0 && ndone==0) {
    fprintf(stderr, "%s: Failed to read property data.\n", XW_IDENT);
    del_XWdev(xw,1);
  };
  return ndone;
}

/*.......................................................................
 * After a fatal error has occured, this function should be called to
 * mark the specified device as unusable. It emits an error message
 * and sets xw->bad_device=1.
 *
 * Input:
 *  xw      XWdev *  The descriptor of the device on which the error
 *                   occurred.
 * Output:
 *  xw->bad_device   This flag is set to 1.
 *  return    int    Allways 1 (intended as a boolean to say that the
 *                   device is unusable). This can be used as the return
 *                   value for functions that use 1 to denote an error
 *                   return. eg.
 *                     if(error_occurred)
 *                       return xw_bad_device(xw);
 */
#ifdef __STDC__
static int xw_bad_device(XWdev *xw)
#else
static int xw_bad_device(xw)
     XWdev *xw;
#endif
{
/*
 * Only report an error if this is the first time that this function
 * has been called on this device.
 */
  if(xw && !xw->bad_device) {
    fprintf(stderr, "%s: Lost PGPLOT window %d.\n", XW_IDENT, xw->number);
    xw->bad_device = 1;
  };
  return 1;
}

/*.......................................................................
 * If the cursor is within the plot window, warp it to a given position.
 *
 * Input:
 *  xw    XWdev * The PGPLOT /xw device descriptor.
 *  pos  XPoint * The location to warp the pointer to if 'warp' is true
 *                and the cursor is in the window.
 *  warp    int   If true, and the cursor is in the window, warp the
 *                cursor to position 'pos'.
 * Output:
 *  loc  XPoint * The located position of the cursor, if it is in
 *                the window.
 *  return  int   0 - Cursor not in window - 'loc' is unchanged.
 *                1 - Cursor is in window - 'loc' records the position.
 */
#ifdef __STDC__
static int xw_locate_cursor(XWdev *xw, XPoint *pos, int warp, XPoint *loc)
#else
static int xw_locate_cursor(xw, pos, warp, loc)
     XWdev *xw; XPoint *pos; int warp; XPoint *loc;
#endif
{
  XWindowAttributes attr; /* Current window attributes */
  Window p_child;         /* The child of /xw (None) containing the pointer */
  int p_win_x, p_win_y;   /* The pointer coordinates in xw->window */
  int p_root_x, p_root_y; /* The pointer coordinates in the root window */
  Window p_root_win;      /* The root window containing the cursor */
  unsigned int p_mask;    /* Bit mask of button states etc.. */
  int inwindow=0;         /* True if the cursor is in the window */
/*
 * Device error?
 */
  if(xw->bad_device)
    return 0;
/*
 * Query the current state of the window.
 */
  XSync(xw->display, False);
  if(xw->bad_device)
    return 0;
  XGetWindowAttributes(xw->display, xw->window, &attr);
  if(xw->bad_device)
    return 0;
/*
 * Determine the current position of the pointer.
 */
  XQueryPointer(xw->display, xw->window, &p_root_win, &p_child,
		&p_root_x, &p_root_y, &p_win_x, &p_win_y, &p_mask);
  if(xw->bad_device)
    return 0;
/*
 * Is the cursor within the bounds of the window?
 */
  inwindow = ((attr.map_state != IsUnmapped) &&
	      (p_win_x >= 0 && p_win_x < attr.width) &&
	      (p_win_y >= 0 && p_win_y < attr.height));
  if(inwindow) {
/*
 * Warp the cursor?
 */
    if(warp) {
      XWarpPointer(xw->display, None, xw->window, 0, 0, 0, 0, pos->x, pos->y);
      if(xw->bad_device)
	return 0;
      loc->x = pos->x;
      loc->y = pos->y;
/*
 * Return the current position of the cursor without warping.
 */
    } else {
      loc->x = p_win_x;
      loc->y = p_win_y;
    };
  };
  return inwindow;
}

#ifdef VMS
/*.......................................................................
 * Define a given executable as a DCL foreign command. This has to be
 * done before the program can be run with command-line arguments.
 *
 * Input:
 *  file    char *   The full file name of the executable.
 *  command char *   The name to give the command that invokes 'file'.
 * Output:
 *  return   int     0 - OK.
 *                   1 - Error.
 */
static int vms_define_command(char *file, char *command)
{
  VMS_string value_dsc;             /* Foreign command string */
  VMS_string symbol_dsc;            /* Symbol name for foreign command */
  long table = LIB$K_CLI_LOCAL_SYM; /* Table to add symbol to */
  char *value = NULL;   /* Dynamically allocated symbol value string */
  int waserr = 0;       /* True after error */
/*
 * Compose a VMS symbol value to use to define 'command' as a foreign
 * command that takes C-style arguments.
 */
  if((value = (char *) malloc(1+strlen(file)+1))==NULL) {
    fprintf(stderr, "%s: Insufficient memory to define command for: %s.\n",
	    XW_IDENT, file);
    waserr = 1;
  } else {
    sprintf(value, "$%s", file);
    VMS_STRING(value_dsc, value)
    VMS_STRING(symbol_dsc, command)
/*
 * Register the symbol value to symbol 'command'.
 */
    lib$set_symbol(&symbol_dsc, &value_dsc, &table);
  };
/*
 * Release resources.
 */
  if(value)
    free((char *)value);
  return waserr != 0;
}

/*.......................................................................
 * Run a PGPLOT program on a VAX VMS machine in the background, with
 * sys$input redirected from NL: so that the child process does not
 * get sent its parent's signals.
 * Unfortunately system() can't be used. For some unknown reason
 *   system("spawn/nowait/sys$input=nl: command_line")
 * doesn't appear to do anything. It doesn't even produce an error
 * message to say why. If the nowait qualifier is removed it works fine,
 * but that is not what is wanted.
 *
 * Input:
 *  command    char *  The command line.
 * Output:
 *  return      int    0 - No error detected.
 *                     1 - Definate error detected.
 */
static int vms_spawn_nowait(char *command)
{
  VMS_string comm_dsc;  /* VMS string descriptor for 'command'. */
  VMS_string sysinp;    /* VMS string descriptor of SYS$INPUT string */
  long flags = CLI$M_NOKEYPAD | CLI$M_NOWAIT;
/*
 * Construct VMS descriptors of C strings.
 */
  VMS_STRING(comm_dsc, command)
  VMS_STRING(sysinp, "NL:")
  if(lib$spawn(&comm_dsc,&sysinp,0,&flags,0,0,0,0,0,0,0) != SS$_NORMAL) {
    fprintf(stderr, "%s: Unable to execute command line: %s\n", XW_IDENT,
	    command);
    return 1;
  };
  return 0;
}
#endif

/*.......................................................................
 * Locate an executable by searching for it in a list of directories.
 * The list of directories is a string containing directory paths
 * separated by ':'s (',' under VMS). If the first or last character
 * in the path is one of these terminators, or if two of these terminators
 * are adjacent in the path, then the current directory is included in
 * the search. Each directory in the path is searched in order and the
 * first match is returned.
 *
 * Note that these semantics are identical to the UNIX Bourne-shell
 * treatment of the PATH environment variable.
 *
 * In order that getenv() can be used directly as an argument to this
 * function without invoking error messages when getenv() returns
 * NULL, either or both of the 'path' and 'program' arguments can be
 * NULL. In this case find_exe() quietly returns NULL as though it had
 * searched for the executable and failed to find it.
 *
 * Input:
 *  program  char *   The name of the program to be located.
 *                    In the case of this being NULL, find_exe() will
 *                    quietly abort and return NULL. This allows one
 *                    to use getenv() without worrying about the NULL
 *                    return case.
 *  path     char *   A colon-separated (comma-separated under VMS),
 *                    '\0' terminated list of directory paths to search
 *                    for the program in.
 * Output:
 *  return   char *   The full name of the executable, or NULL if not
 *                    found. The returned pointer is malloc()d
 *                    memory and should be free()d by the caller when
 *                    no longer required.
 */
#ifdef __STDC__
static char *find_exe(char *path, char *program)
#else
static char *find_exe(path, program)
     char *path; char *program;
#endif
{
  char *dir;       /* Pointer to start of directory in 'path' */
  char *buf=NULL;  /* A buffer used to compile file names in */
  int buflen=0;    /* The size of the dynamic buffer in char's */
  int prog_len;    /* Length of program name */
  int dirlen;      /* Length of directory name pointed to by 'dir' */
  int path_len;    /* Length of latest path name */
#ifdef VMS
  char *exe = ".exe";/* VMS executable extension */
  char *sep = "";    /* VMS directory/file separator (overriden below) */
  int term  = ',';   /* Directory separator in path (in addition to '\0') */
#else
  char *exe = "";    /* UNIX doesn't add extensions */
  char *sep = "/";   /* UNIX directory/file separator */
  int term  = ':';   /* Directory separator in path (in addition to '\0') */
#endif
/*
 * No path or executable?
 */
  if(path==NULL || program==NULL)
    return NULL;
/*
 * Allocate memory for the filename buffer.
 */
  buflen = strlen(program) + 40;
  buf = (char *) malloc(sizeof(char) * (buflen+1));
  if(buf==NULL) {
    fprintf(stderr, "%s: Insufficient memory to locate program: %s\n",
	    XW_IDENT, program);
    return buf;
  };
/*
 * Determine the length of the program name.
 */
  prog_len = strlen(program);
/*
 * Seek the program in each 'term' separated path name.
 */
  do {
/*
 * Maintain a pointer to the start of the directory path.
 */
    dir = path;
/*
 * Find the directory terminator.
 */
    while(*path && *path != term)
      path++;
/*
 * Record the path length.
 */
    dirlen = path - dir;
/*
 * Skip the trailing terminator unless at the end of the path.
 */
    if(*path)
      path++;
/*
 * Under VMS a separator is not required if the directory is given
 * explicitly rather than with logical variables.
 */
#ifdef VMS
    sep = dirlen>0 && dir[dirlen-1]==']' ? "" : ":";
#endif
/*
 * Combine the directory and command file name into a full program
 * name.
 */
    path_len = dirlen + strlen(sep) + prog_len + strlen(exe) ;
    if(path_len > buflen) {
      char *new_buf = realloc(buf, (path_len+1) * sizeof(char));
      if(new_buf==NULL) {
	fprintf(stderr, "%s: Insufficient memory to locate program: %s\n",
		XW_IDENT, program);
	free(buf);
	return buf;
      };
      buf = new_buf;
    };
    sprintf(buf, "%.*s%s%s%s", dirlen, dir, dirlen==0 ? "":sep, program, exe);
/*
 * See if the executable file exists.
 */
#ifndef X_OK
#define X_OK 1
#endif
#ifndef R_OK
#define R_OK 4
#endif
#ifdef VMS
    if(access(buf, X_OK)==0 || access(buf, R_OK)==0) {
      if(vms_define_command(buf, program)) /* Define a foreign VMS command */
	break;
      strcpy(buf, program);
      return buf;
    };
#else
    if(access(buf, X_OK)==0)
      return buf;
#endif
  } while(*path);
/*
 * Executable file not found.
 */
  free(buf);
  return NULL;
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
 *  visual      Visual *   The visual for which information is required.
 * Output:
 *  return XVisualInfo *   The required information descriptor, or NULL
 *                         on error.
 */
#ifdef __STDC__
static XVisualInfo *xw_visual_info(Display *display, int screen, Visual *visual)
#else
static XVisualInfo *xw_visual_info(display, screen, visual)
     Display *display; int screen; Visual *visual;
#endif
{
  XVisualInfo *vi=NULL;  /* The return descriptor */
  XVisualInfo template;  /* The search template */
  int nret = 0;          /* The number of descriptors returned */
/*
 * Using the visual ID and the screen should unambiguously select the
 * information for the specified visual.
 */
  template.visualid = XVisualIDFromVisual(visual);
  template.screen = screen;
  vi = XGetVisualInfo(display, (long)(VisualIDMask | VisualScreenMask),
		      &template, &nret);
  if(vi == NULL || nret < 1) {
    fprintf(stderr,
      "%s: Error getting visual information for visual ID 0x%lx, screen %d.\n",
      XW_IDENT, (unsigned long)template.visualid, screen);
    vi = NULL;
  };
  return vi;
}

/*.......................................................................
 * Allocate the contents of xw->image. This contains buffers used to
 * construct and dispatch line-of-pixel images to the display.
 *
 * Note that xw_get_visual() must have been called before this function.
 *
 * Input:
 *  xw    XWdev * The PGPLOT /xw device descriptor.
 *  npix    int   The length of the buffer in pixels.
 * Output:
 *  return  int   0 - OK.
 *                1 - Error.
 */
#ifdef __STDC__
static int xw_get_image(XWdev *xw, int npix)
#else
static int xw_get_image(xw, npix)
     XWdev *xw; int npix;
#endif
{
/*
 * Create the X image that we use to compose lines of pixels with given
 * colors.
 */
  xw->image.xi = XCreateImage(xw->display, xw->color.vi->visual,
			      (unsigned)xw->color.vi->depth, ZPixmap, 0,
			      NULL, (unsigned)npix, 1, 32, 0);
  if(xw->image.xi==NULL) {
    fprintf(stderr, "%s: Failed to allocate XImage container.\n", XW_IDENT);
    return 1;
  };
/*
 * Allocate the image buffer.
 */
  xw->image.xi->data = malloc((size_t) xw->image.xi->bytes_per_line);
  if(!xw->image.xi->data) {
    fprintf(stderr, "%s: Failed to allocate image buffer.\n", XW_IDENT);
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Limit pixmap coordinates to lie within the pixmap area.
 *
 * Input:
 *  xw      XWdev *  The PGPLOT window context.
 * Input/Output:
 *  coord  XPoint *  The coordinates to be modified.
 */
#ifdef __STDC__
static void xw_limit_pcoords(XWdev *xw, XPoint *coord)
#else
static void xw_limit_pcoords(xw, coord)
     XWdev *xw; XPoint *coord;
#endif
{
  if(xw->pixmap != None) {
    if(coord->x >= xw->geom.width)
      coord->x = xw->geom.width - 1;
    if(coord->y >= xw->geom.height)
      coord->y = xw->geom.height - 1;
    if(coord->x < 0)
      coord->x = 0;
    if(coord->y < 0)
      coord->y = 0;
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
static int xw_nint(float f)
#else
static int xw_nint(f)
     float f;
#endif
{
  return (int) (f >= 0.0 ? (f + 0.5) : (f - 0.5));
}

/*.......................................................................
 * Scroll a rectanglular area vertically and/or horizontally.
 *
 * Input:
 *  xw     XWdev *  The PGPLOT window context.
 *  rbuf     float *  The array of float arguments sent by the PGPLOT
 *                    GREXEC() subroutine.
 */
#ifdef __STDC__
static void xw_scroll_rect(XWdev *xw, float *rbuf)
#else
static void xw_scroll_rect(xw, rbuf)
     XWdev *xw; float *rbuf;
#endif
{
  if(!xw->bad_device && xw->pixmap != None) {
    XPoint blc, trc;     /* The bottom left and top right rectangle corners */
    XPoint blc_orig, trc_orig; /* The vertices of the rectangle to be copied */
    XPoint blc_dest, trc_dest; /* The vertices of the destination of the copy */
    int dx, dy;                /* The amounts to scroll right and down */
    unsigned long fg;          /* The foreground color to be reinstated */
/*
 * Convert the rectangle vertices from PGPLOT coordinates to X coordinates.
 */
    xw_xy_to_XPoint(xw, &rbuf[0], &blc);
    xw_xy_to_XPoint(xw, &rbuf[2], &trc);
/*
 * Get the scroll offsets in X coordinates.
 */
    dx = xw_nint(rbuf[4]);
    dy = xw_nint(-rbuf[5]);
/*
 * Selected parts of the pixmap will need to be erased by drawing an
 * opaque rectangle over them in the background color. Set the foreground
 * color to equal the background. Keep a record of the previous foreground
 * color, so that it can be re-instated.
 */
    fg = xw->gcv.foreground;
    XSetForeground(xw->display, xw->gc, xw->color.pixel[0]);
/*
 * If either scroll extent exceeds the length of the associated
 * axis, then fill the area with the background color.
 */
    if(abs(dx) > trc.x - blc.x || abs(dy) > blc.y - trc.y) {
      XFillRectangle(xw->display, xw->pixmap, xw->gc, blc.x, trc.y,
		     (unsigned)(trc.x-blc.x+1), (unsigned)(blc.y-trc.y+1));
/*
 * Scroll within the rectangle by copying the area that is to be preserved
 * to a new location shifted appropriately in X and/or Y. Then clear the
 * vacated areas.
 */
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
      xw_limit_pcoords(xw, &blc_orig);
      xw_limit_pcoords(xw, &blc_dest);
      xw_limit_pcoords(xw, &trc_orig);
      xw_limit_pcoords(xw, &trc_dest);
/*
 * Scroll the rectangle to its shifted location.
 */
      XCopyArea(xw->display, xw->pixmap, xw->pixmap, xw->gc,
		blc_orig.x, trc_orig.y,
		trc_orig.x - blc_orig.x + 1,
		blc_orig.y - trc_orig.y + 1,
		blc_dest.x, trc_dest.y);
/*
 * Clear the vacated area to the left or right of the copied area.
 */
      if(dx > 0) {
	XFillRectangle(xw->display, xw->pixmap, xw->gc,
		       blc.x, trc.y,
		       (unsigned) dx,
		       (unsigned) (blc.y - trc.y + 1));
      } else if(dx < 0) {
	XFillRectangle(xw->display, xw->pixmap, xw->gc,
		       trc_dest.x, trc.y,
		       (unsigned) (-dx),
		       (unsigned) (blc.y - trc.y + 1));
      };
/*
 * Clear the vacated area above or below the copied area.
 */
      if(dy > 0) {
	XFillRectangle(xw->display, xw->pixmap, xw->gc,
		       blc.x, trc.y,
		       (unsigned) (trc.x - blc.x + 1),
		       (unsigned) dy);
      } else if(dy < 0) {
	XFillRectangle(xw->display, xw->pixmap, xw->gc,
		       blc.x, blc_dest.y,
		       (unsigned) (trc.x - blc.x + 1),
		       (unsigned) (-dy));
      };
    };
/*
 * Record the extent of the modified part of the pixmap.
 */
    xw_mark_modified(xw, blc.x, blc.y, 1);
    xw_mark_modified(xw, trc.x, trc.y, 1);
/*
 * Re-instate the original foreground color.
 */
    XSetForeground(xw->display, xw->gc, fg);
  };
  return;
}
