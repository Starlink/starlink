/*.......................................................................
 * PGPLOT driver for Starlink GWM graphics under X-Windows.
 *
 * Scope:  This driver ths designed to work with Starlink GWM windows.
 * Colour: Visual color maps of types, PsuedoColor, StaticColor, GrayScale
 *         and StaticGray are supported. Where insufficient colors are
 *         available in the default colormap, a private colormap is
 *         requested. If this fails, the device is treated as
 *         monochrome.
 * Cursor: The cursor is controlled by a mouse or equivalent. Buttons
 *         1 2 3 are mapped to characters A D X. The cursor can also
 *         be moved horizontally and vertically with the arrow keys.
 *         Each press of an arrow key moves the cursor one pixel. This
 *         can be increased to 10 pixels by pressing the shift key.
 *
 * Based on XWDRIV  Version 3.0 ( 1994 Nov 06 )
 *                  - M. C. Shepherd (mcs@astro.caltech.edu) ).
 * Authors
 *     BKM - B. K. McIlwrath (Starlink, RAL) <bkm@star.rl.ac.uk>
 * History
 *
 *     07 Oct 1998 Version 1.0 (BKM)
 *     23 Nov 1999 Minor revision to change Spider cursor to pointer.
 *     09 Dec 1999 Correct pixels-per-inch calculations.
 *     08 Aug 2000 Correct behaviour for GWM windows with non-zero xscoll
 *                 and yscroll values.
 *     15 Jan 2001 Correct erroneous clear of GWM window with /APPEND.
 *                 Remove XMapRaised of GWM window - except for input.
 *     17 Jan 2001 Make the "pixels/inch" calculation match that used in
 *                 GKS based PGPLOT so that coordinates in AGI line up.
 *     07 Mar 2001 Avoid multiple XOpenDisplay() calls by making the "display"
 *                 identifier static and use this value if defined.
 *     27 Apr 2001 Undo the change to pixels/inch above.
 *     29 May 2003 Correct bug for 24-bit displays when image buffer size
 *                 calculation was incorrect for 32-bits/pixel
 */

/*
 * Certain symbols in fcntl.h may not get defined
 * unless the _POSIX_SOURCE feature-test macro is set.
 */
#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif

/*
 * Allow gwmdriv to be callable by FORTRAN using the two commonest
 * calling conventions. Both conventions append length arguments for
 * each FORTRAN string at the end of the argument list, and convert the
 * name to lower-case, but one post-pends an underscore to the function
 * name (PG_PPU) while the other doesn't.
 * For other calling conventions you must write a
 * C wrapper routine to call gwmdriv() or gwmdriv_().
 */
#ifdef PG_PPU
#define GWMDRIV gwmdriv_
#else 
#define GWMDRIV gwmdriv
#endif


#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

/* X-Window include files */

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>

/* GWM include files */

#include "gwm.h"
#include "gwm_err.h"

/* Starlink F77 include file */

#include "f77.h"

extern F77_SUBROUTINE(grwarn)( CHARACTER(text) TRAIL(text) );

#define NCOLORS 16            /* Number of pre-defined PGPLOT colors */
#define GWM_IMAGE_LEN 1280    /* Length of the line-of-pixels buffer */
#define COLORMULT 65535       /* Normalized color intensity multiplier */
#define GWM_DEF_WIDTH 780     /* Default width (pixels) */
#define GWM_DEF_HEIGHT 512    /* Deafult height (pixels) */

#define GWM_IDENT "/gwm"      /* Name to prefix messages to user */

/*
 * Set the degree to which holding down the shift-key speeds up cursor
 * motion when an arrow key is held down.
 */
#define ARROW_KEY_VELOCITY 10

/* A container used to record the geometry of the X-window */

typedef struct {
  int x,y;              /* Locus of top left corner of window (pixels) */
  unsigned int width;   /* Width of window pixmap (pixels) */
  unsigned int height;  /* Height of window pixmap (pixels) */
  unsigned int depth;   /* Pixmap depth */
  int xpix_per_inch;    /* Number of pixels per inch along X */
  int ypix_per_inch;    /* Number of pixels per inch along Y */
  float xmetre, ymetre; /* Window size in metres	*/
  int xscroll;          /* X scroll offset (pixels) */
  int yscroll;          /* Y scroll offset (pixels) */
  int noclear;          /* No initial clear if /APPEND specified */ 
} GWMgeom;

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
  int initialized;   /* True after the first call to gwm_init_colors() */
  int nbuff;         /* The number of buffered color representation updates */
  int sbuff;         /* The index of the first buffered color representation */
} GWMcolor;

/*
 * Declare a polygon descriptor.
 */
typedef struct {
  XPoint *points;  /* Temporary array of polygon vertexes */
  int npoint;      /* Number of points in polygon */
  int ndone;       /* The number of points received so far */
} GWMpoly;

/*
 * Declare a container used to record the extent of the rectangular
 * pixmap area that has been modified since the last gwm_flush().
 */
typedef struct {
  int modified;    /* True if 'pixmap' has been modified since last update */
  int xmin,xmax;   /* X-axis extent of modified region (pixels) */
  int ymin,ymax;   /* Y-axis extent of modified region (pixels) */
} GWMupdate;

/*
 * Declare a container to encapsulate the buffers needed to
 * draw a line of pixels.
 */
typedef struct {
  int npix;            /* The max number of pixels in buff[] */
  unsigned char *buff; /* The image buffer registered to xi[] */
  XImage *xi;          /* Line of pixels Xlib image object */
} GWMimage;

/*
 * Declare a container used to hold event state information.
 */
typedef struct {
  long mask;       /* Current event mask */
  int no_buttons;  /* True after failure to acquire ButtonPressMask */
} GWMevent;

/*
 * Declare a function type, instances of which are to be called to flush
 * buffered opcodes, and return 0 if OK, or 1 on error.
 */
struct GWMdev;
typedef int (*Flush_Opcode_fn) (struct GWMdev *gwm);

/*
 * The following container is used to retain state information for /gwm
 * connections.
 */
typedef struct GWMdev {
  Display *display;   /* Display descriptor */
  Window window;      /* Window ID */
  char *name;         /* Window name */ 
  int number;         /* PGPLOT window number */
  Window inwin;       /* An InputOnly window used for cursor operations */
  Window managed;     /* The managed window of a (possible) GWM hierarchy */
  int screen;         /* The screen in which the window is displayed */
  int bad_device;     /* Set to 1 by gwm_bad_device() after fatal errors. */
  int last_error;     /* The last error code trapped by gwm_error() */
  Pixmap pixmap;      /* Pixmap ID */
  Cursor norm_cursor; /* ID of normal cursor */
  Cursor live_cursor; /* ID of active cursor */
  GWMpoly poly;       /* Polygon-fill accumulation descriptor */
  GWMcolor color;     /* Colormap descriptor */
  GWMgeom geom;       /* The size and position of the window */
  GWMupdate update;   /* Descriptor of un-drawn area of pixmap */
  GWMevent event;     /* Event state container */
  GWMimage image;     /* Line of pixels container */
  XGCValues gcv;      /* Publicly visible contents of 'gc' */
  GC gc;              /* Graphical context descriptor */
  int last_opcode;    /* Index of last opcode */
  Flush_Opcode_fn flush_opcode_fn; /* Function to flush a buffered opcode */
  struct GWMdev *next;/* Pointer to next /gwm device in list */
} GWMdev;

/* Create an alias for the standard X error handling function type */

typedef int (*Xerrorfn) (Display *, XErrorEvent *);

/* Private method functions that operate on GWMdev descriptors */

static GWMdev *new_GWMdev(char *display, int mode);
static GWMdev *del_GWMdev(GWMdev *gwm, int partial);
static int gwm_bad_device(GWMdev *gwm);

static int gwm_ok(GWMdev *gwm);
static int gwm_set_rgb(GWMdev *gwm, int ci, float red, float green, float blue);
static int gwm_init_colors(GWMdev *gwm);
static int gwm_get_image(GWMdev *gwm, int npix);
static int gwm_new_cursors(GWMdev *gwm);
static int gwm_clear(GWMdev *gwm);
static int gwm_set_ci(GWMdev *gwm, int ci);
static void gwm_mark_modified(GWMdev *gwm, int x, int y, int diameter);
static int gwm_flush(GWMdev *gwm);
static void gwm_XPoint_to_xy(GWMdev *gwm, XPoint *xp, float *xy);
static void gwm_xy_to_XPoint(GWMdev *gwm, float *xy, XPoint *xp);
static float gwm_xcolor_to_rgb(unsigned short urgb);
static int gwm_rgb_to_xcolor(float rgb);

static int gwm_image_line(GWMdev *gwm, XPoint *start, float *cells, int ncell);
static int gwm_read_cursor(GWMdev *gwm, int mode, int posn, XPoint *ref,
				XPoint *pos, char *key);
static int gwm_create_input_window(GWMdev *gwm);
static int gwm_shift_cursor(GWMdev *gwm, KeySym keysym, \
				 unsigned int modifiers);
static int gwm_expose(GWMdev *gwm, XEvent *event);
static int gwm_error(Display *display, XErrorEvent *event);
static int gwm_locate_cursor(GWMdev *gwm, XPoint *pos, int warp, XPoint *loc);
static int gwm_next_event(GWMdev *gwm, XEvent *event);
static int gwm_check_window_event(GWMdev *gwm, Window window,
				       long event_mask, XEvent *event);
static XVisualInfo *gwm_visual_info(Display *display, int screen,
					 Visual *visual);
static void gwm_limit_pcoords(GWMdev *gwm, XPoint *coord);
static void gwm_scroll_rect(GWMdev *gwm, float *rbuf);
static GWMdev *gwm_remove_device(GWMdev *gwm);
static GWMdev *gwm_select_device(int number);
static void gwm_errmess(char *buff);

/* Container for rubber-band cursor resources and status */

typedef struct {
  int line_width;  /* Rubber-band line width */
  int mode;        /* Cursor mode 1=line, 2=rectangle */
  XPoint ref;      /* Reference vertex of cursor */
  XPoint end;      /* End point of cursor */
} Band;

static Band *gwm_new_Band(GWMdev *gwm, int mode, XPoint *ref);
static int gwm_draw_cursor(GWMdev *gwm, Band *bc, XPoint *end);
static int gwm_erase_cursor(GWMdev *gwm, Band *bc);
static int gwm_end_input(GWMdev *gwm, Band *bc, int status);
static Band *gwm_del_Band(GWMdev *gwm, Band *bc);
static int gwm_bound_cursor(GWMdev *gwm, XPoint *xp);
static int gwm_cursor_line(GWMdev *gwm, int xa, int ya, int xb, int yb);
static int gwm_add_events(GWMdev *gwm, long events);
static int gwm_rem_events(GWMdev *gwm, long events);

/* Functions used to flush buffered opcodes */

static int gwm_update_colors(GWMdev *gwm);

/*
 * Declare the head of the list of open GWM device descriptors.
 * This has to have file scope to allow the X error handler to get at it.
 */

static GWMdev *device_list = NULL;

static int gwm_nint(float f);

/* Text buffer used to pass formatted error messages to gwm_errmess */
static char errbuff[80];

/*.......................................................................
 * This is the only external entry point to the /gwm device driver.
 * It is called by PGPLOT to open, perform operations on, return
 * information about and close /gwm windows.
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
 *  mode    int *  Device specific mode parameter - currently unused
 *  len     int    Added to the call line by the FORTRAN compiler.
 *                 This contains the declared size of chr[].  
 */
void GWMDRIV
(int *ifunc, float rbuf[], int *nbuf, char *chr, int *lchr, int *mode,
 int len)
{
  static GWMdev *gwm = NULL; /* The descriptor of the currently selected device */
  int i;
/*
 * If there is a buffered opcode and the latest opcode is not the same
 * as the last opcode, call the given flush function for the
 * buffered opcode.
 */
  if(gwm && !gwm->bad_device) {
    if(gwm->last_opcode != *ifunc) {
      if(gwm->flush_opcode_fn != (Flush_Opcode_fn) 0) {
	(*gwm->flush_opcode_fn)(gwm);
	gwm->flush_opcode_fn = (Flush_Opcode_fn) 0;
      }
/*
 * Record the current opcode for next time.
 */
      gwm->last_opcode = *ifunc;
    }
  }

/* Branch on opcode. */

  switch(*ifunc) {

/*--- IFUNC=1, Return device name ---------------------------------------*/

  case 1:
    {
      char *dev_name;
      dev_name = "GWM  (A Starlink GWM window that persists for re-use)";
      strncpy(chr, dev_name, len);
      *lchr = strlen(dev_name);
      for(i = *lchr; i < len; i++)
	chr[i] = ' ';
    }
    break;

/*--- IFUNC=2, Return physical min and max for plot device, and range
               of color indices -----------------------------------------*/
  case 2:
    rbuf[0] = 0.0;
    rbuf[1] = -1.0;  /* Report no effective max plot width */
    rbuf[2] = 0.0;
    rbuf[3] = -1.0;  /* Report no effective max plot height */
    rbuf[4] = 0.0;
    rbuf[5] = (gwm && !gwm->bad_device) ? gwm->color.ncol-1 : 1;
    *nbuf = 6;
    break;

/*--- IFUNC=3, Return device resolution ---------------------------------*/

  case 3:
    if(gwm_ok(gwm)) {
       rbuf[0] = gwm->geom.xpix_per_inch;
       rbuf[1] = gwm->geom.ypix_per_inch;
    } else {
      rbuf[0] = 1.0;
      rbuf[1] = 1.0;
    }
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
    chr[7] = 'N'; /* no prompt on PGEND - GWM windows are persistent */
    chr[8] = 'Y'; /* Can return color representation */
    chr[9] = 'N'; /* Not used */
    chr[10]= 'S'; /* Area-scroll available */
    *lchr = 11;
    break;

/*--- IFUNC=5, Return default file name ---------------------------------*/

  case 5:
    { 
      if(device_list == NULL) 
         strncpy(chr, "xwindows", len);
      else {
/*
 * Find the PGPLOT number of the last opened GWM device
 */
         struct GWMdev *dev = device_list;
         int number;
         while(dev->next)
            dev = dev->next;
         number = dev->number+1;
         sprintf(chr, "x%dwindows", number);
      }
      *lchr = strlen(chr);   
      for(i = *lchr; i < len; i++)
	chr[i] = ' ';
    }
    break;

/*--- IFUNC=6, Return default physical size of plot ---------------------*/

  case 6:
    if(gwm && !gwm->bad_device) {  /* Return the size of the pixmap */
       rbuf[0] = 0.0;
       rbuf[1] = (float) gwm->geom.width;
       rbuf[2] = 0.0;
       rbuf[3] = (float) gwm->geom.height;
       *nbuf = 4;
    } else {
       rbuf[0] = 0.0;
       rbuf[1] = GWM_DEF_WIDTH;
       rbuf[2] = 0.0;
       rbuf[3] = GWM_DEF_HEIGHT;
    }
    break;

/*--- IFUNC=7, Return misc defaults -------------------------------------*/

  case 7:
    rbuf[0] = 1.0;
    *nbuf = 1;
    break;

/*--- IFUNC=8, Select plot ----------------------------------------------*/

  case 8:
    gwm = gwm_select_device((int)(rbuf[1]+0.5));
    break;

/*--- IFUNC=9, Open workstation -----------------------------------------*/

  case 9:
    {
      struct GWMdev *dev;
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
      sprintf(errbuff, "%s: Display name too long.\n", GWM_IDENT);
      gwm_errmess(errbuff);
      return;
    } else {
      chr[*lchr] = '\0';
    }
/*
 * Either create or locate the GWM window.
 */
    if( (gwm = device_list) )
        while(gwm)
        {
          if(!strcmp(gwm->name, chr))
             break;
          gwm = gwm->next;
        }
    if(gwm==NULL)
        gwm = new_GWMdev(chr, *mode);
    if(gwm==NULL)
      return;
/*
 * Insert the device in the list of open devices.
 */
    if(device_list == NULL) {
       device_list = gwm;
       gwm->number = 1;
    }
    else
    {
       dev = device_list;
       while(dev->next)
          dev = dev->next;
       dev->next = gwm;
       gwm->number = dev->number + 1;
    }
    rbuf[0] = gwm->number; /* Number used to select this device */
    rbuf[1] = 1.0;
    gwm->geom.noclear = rbuf[2];
    *nbuf = 2;
    }
    break;

/*--- IFUNC=10, Close workstation ---------------------------------------*/

  case 10:
/*
 * Remove the device from the list of open devices and delete it.
 */
    gwm_remove_device(gwm);
    gwm = del_GWMdev(gwm,0);
    break;

/*--- IFUNC=11, Begin picture -------------------------------------------*/

  case 11:
/*
 *  Note that we are allowed to ignore any changes to view surface sizes
 *  (rbuf[0] and rbuf[1]) and, as GWM cannot yet do resizes, we ignore this
 *  for now.
 */
    if(gwm_ok(gwm)) {
/*
 * Reset the colormap color representations if necessary.
 */
    if(!gwm->color.initialized)
       gwm_init_colors(gwm);
/*
 * Clear the window and pixmap.
 */
    if(!gwm->geom.noclear)
       gwm_clear(gwm);
    }
    break;

/*--- IFUNC=12, Draw line -----------------------------------------------*/

  case 12:
    if(gwm_ok(gwm) && gwm->pixmap!=None) {
      XPoint start;
      XPoint end;
      gwm_xy_to_XPoint(gwm, &rbuf[0], &start);
      gwm_xy_to_XPoint(gwm, &rbuf[2], &end);
      XDrawLine(gwm->display, gwm->pixmap, gwm->gc, start.x,start.y,
                end.x,end.y);
      gwm_mark_modified(gwm, start.x, start.y, gwm->gcv.line_width);
      gwm_mark_modified(gwm, end.x, end.y, gwm->gcv.line_width);
    }
    break;

/*--- IFUNC=13, Draw dot ------------------------------------------------*/

  case 13:
    if(gwm_ok(gwm) && gwm->pixmap!=None) {
      XPoint xp;
      int radius = gwm->gcv.line_width/2;
      gwm_xy_to_XPoint(gwm, rbuf, &xp);
      if(radius < 1) {
	XDrawPoint(gwm->display, gwm->pixmap, gwm->gc, xp.x, xp.y);
      } else {
	unsigned int diameter = radius*2;
	int x = xp.x - radius;
	int y = xp.y - radius;
	XFillArc(gwm->display, gwm->pixmap, gwm->gc, x, y, diameter, diameter,
		 0, 23040);
      }
      gwm_mark_modified(gwm, xp.x, xp.y, gwm->gcv.line_width);
    }
    break;

/*--- IFUNC=14, End picture ---------------------------------------------*/

  case 14:
    break;

/*--- IFUNC=15, Select color index --------------------------------------*/

  case 15:
    if(gwm_ok(gwm))
      gwm_set_ci(gwm, (int) (rbuf[0] + 0.5));
    break;

/*--- IFUNC=16, Flush buffer. -------------------------------------------*/

  case 16:
    if(gwm_ok(gwm))
      gwm_flush(gwm);
    break;

/*--- IFUNC=17, Read cursor. --------------------------------------------*/

  case 17:
    if(gwm_ok(gwm)) {
      XPoint ref;                     /* Reference cursor coordinates */
      XPoint pos;                     /* Input/Output cursor coordinates */
      int mode = 0;                   /* Cursor band mode */
      int posn = 1;                   /* True to position the cursor */
      gwm_xy_to_XPoint(gwm, rbuf, &pos);
      gwm_xy_to_XPoint(gwm, &rbuf[2], &ref);
      mode = (int)(rbuf[4]+0.5);
      posn = (int)(rbuf[5]+0.5) > 0;
      if(gwm_read_cursor(gwm, mode, posn, &ref, &pos, chr)==0)
	gwm_XPoint_to_xy(gwm, &pos, rbuf);
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
    if(gwm_ok(gwm) && gwm->pixmap != None) {
/*
 * The first call specifies just the number of vertixes in the polygon.
 */
      if(gwm->poly.npoint == 0) {
	gwm->poly.npoint = (int) (rbuf[0] + 0.5);
	gwm->poly.points = (XPoint *) malloc(sizeof(XPoint) * 
                            gwm->poly.npoint);
	if(gwm->poly.points == NULL)
        {
	   sprintf(errbuff, "%s: Insufficient memory for polygon points.\n",
		   GWM_IDENT);
           gwm_errmess(errbuff);
        }
	gwm->poly.ndone = 0;
/*
 * The next gwm->poly.npoint calls specify the vertexes of the polygon.
 */
      } else {
/*
 * Ignore the points if the above malloc() failed.
 */
	if(gwm->poly.points) {
	  XPoint *xp = &gwm->poly.points[gwm->poly.ndone];
	  gwm_xy_to_XPoint(gwm, rbuf, xp);
	  gwm_mark_modified(gwm, xp->x, xp->y, 1);
	}
/*
 * Maintain the count of the number of points, even if no memory for the
 * points is available. Thus we can just ignore all calls until
 * gwm->poly.ndone == gwm->poly.npoint.
 */
	gwm->poly.ndone++;
/*
 * On the last call display the filled polygon and release the memory used
 * to store its vertexes.
 */
	if(gwm->poly.ndone >= gwm->poly.npoint) {
	  if(gwm->poly.points) {
	    XFillPolygon(gwm->display, gwm->pixmap, gwm->gc, gwm->poly.points,
			 gwm->poly.npoint, Complex, CoordModeOrigin); 
	    free((char *)gwm->poly.points);
	    gwm->poly.points = NULL;
	  }
	  gwm->poly.npoint = 0;
	}
      }
    }
    break;

/*--- IFUNC=21, Set color representation. -------------------------------*/

  case 21:
    if(gwm_ok(gwm)) {
      if(!gwm->color.initialized)
	gwm_init_colors(gwm);
      gwm_set_rgb(gwm, (int)(rbuf[0]+0.5), rbuf[1],rbuf[2],rbuf[3]);
    }
    break;

/*--- IFUNC=22, Set line width. -----------------------------------------*/

  case 22:
/*
 * The line width is provided in multiples of 0.005 inches.
 */
    if(gwm_ok(gwm)) {
      gwm->gcv.line_width = (int)(rbuf[0]*0.005 * gwm->geom.xpix_per_inch);
      XChangeGC(gwm->display, gwm->gc, (unsigned long) GCLineWidth, &gwm->gcv);
    }
    break;

/*--- IFUNC=23, Escape --------------------------------------------------*/
    /* (Not implemented: ignored) */
  case 23:
    break;

/*--- IFUNC=24, Rectangle Fill. -----------------------------------------*/

  case 24:
    if(gwm_ok(gwm) && gwm->pixmap != None) {
      XPoint blc;
      XPoint trc;
      gwm_xy_to_XPoint(gwm, &rbuf[0], &blc);
      gwm_xy_to_XPoint(gwm, &rbuf[2], &trc);
      XFillRectangle(gwm->display, gwm->pixmap, gwm->gc, blc.x, trc.y,
		     (unsigned)(trc.x-blc.x+1), (unsigned)(blc.y-trc.y+1));
      gwm_mark_modified(gwm, blc.x, blc.y, 1);
      gwm_mark_modified(gwm, trc.x, trc.y, 1);
    }
    break;

/*--- IFUNC=25, ---------------------------------------------------------*/
  /* (Not implemented: ignored) */
  case 25:
    break;

/*--- IFUNC=26, Line of pixels ------------------------------------------*/

  case 26:
    if(gwm_ok(gwm)) {
      XPoint start;
      gwm_xy_to_XPoint(gwm, rbuf, &start);
      gwm_image_line(gwm, &start, &rbuf[2], *nbuf - 2);
    }
    break;

/*--- IFUNC=29, Query color representation ------------------------------*/
  case 29:
    if(gwm_ok(gwm)) {
      int ci = (int) (rbuf[0] + 0.5);
      if(!gwm->color.initialized)
	gwm_init_colors(gwm);
      rbuf[1] = gwm_xcolor_to_rgb(gwm->color.xcolor[ci].red);
      rbuf[2] = gwm_xcolor_to_rgb(gwm->color.xcolor[ci].green);
      rbuf[3] = gwm_xcolor_to_rgb(gwm->color.xcolor[ci].blue);
    } else {
      rbuf[1] = rbuf[2] = rbuf[3] = 0;
    };
    *nbuf = 4;
    break;

/*--- IFUNC=30, Scroll rectangle ----------------------------------------*/
  case 30:
    gwm_scroll_rect(gwm, rbuf);
    break;

/*--- IFUNC=?, ----------------------------------------------------------*/

  default:
    sprintf(errbuff, "%s: Ignoring unimplemented opcode=%d.\n",
            GWM_IDENT, *ifunc);
    gwm_errmess(errbuff);
    *nbuf = -1;
    break;
  };
/*
 * After a server error, close the connection to the display and set all
 * server resources to 'None'. This both prevents calls on bad resources
 * and by deleting the client communication window, tells the server to
 * close the connection if the server hasn't already died.
 */
  if(gwm && gwm->bad_device && gwm->display)
    del_GWMdev(gwm, 1);
  return;
}

/*.......................................................................
 * Assign a given RGB color representation to a given color index.
 *
 * Input:
 *  gwm   GWMdev *  The /gwm device descriptor.
 *  ci      int    The color index to assign the color to. Out of range
 *                 indexes are quietly ignored.
 *  red   float    The fractional red brightness 0..1.
 *  green float    The fractional green brightness 0..1. 
 *  blue  float    The fractional blue brightness 0..1.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
static int gwm_set_rgb(GWMdev *gwm, int ci, float red, float green, float blue)
{
  float gray;   /* Gray-scale intensity */
  XColor *xc;   /* The descriptor of the new color */
/*
 * Device error?
 */
  if(gwm->bad_device)
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
  if(!gwm->color.monochrome && ci >= 0 && ci < gwm->color.ncol) {
/*
 * Get the color representation descriptor.
 */
    xc = &gwm->color.xcolor[ci];
/*
 * Get the pixel to be assigned the new color representation.
 */
    xc->pixel = gwm->color.pixel[ci];
    xc->flags = DoRed | DoGreen | DoBlue;
    xc->pad   = 0;
/*
 * Determine the appropriate RGB values for the type of colormap.
 */
    switch(gwm->color.vi->class) {
    case PseudoColor:
    case StaticColor:
    case DirectColor:
    case TrueColor:
      xc->red   = gwm_rgb_to_xcolor(red);
      xc->green = gwm_rgb_to_xcolor(green);
      xc->blue  = gwm_rgb_to_xcolor(blue);
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
      xc->red = xc->green = xc->blue = gwm_rgb_to_xcolor(gray);
      break;
    }
/*
 * Update the recorded range of color indexes whose color representations
 * have been changed since the last call to gwm_update_colors().
 */
    if(gwm->color.nbuff<=0) {
      gwm->color.sbuff = ci;
      gwm->color.nbuff = 1;
    } else if(ci < gwm->color.sbuff) {
      gwm->color.nbuff += gwm->color.sbuff - ci;
      gwm->color.sbuff = ci;
    } else if(ci > gwm->color.sbuff + gwm->color.nbuff-1) {
      gwm->color.nbuff = ci - gwm->color.sbuff + 1;
    }
/*
 * Register gwm_update_colors() to be called to flush the colors to the
 * window.
 */
    gwm->flush_opcode_fn = (Flush_Opcode_fn) gwm_update_colors;
  }
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
static int gwm_rgb_to_xcolor(float rgb)
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
static float gwm_xcolor_to_rgb(unsigned short urgb)
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
 * Flush color-representation changes made by gwm_set_rgb() to the /gwm
 * window. This updates the window colormap. If color index 0 is changed
 * then the background color is also updated.
 * 
 * Input:
 *  gwm     GWMdev *    The PGPLOT /gwm device descriptor.
 *  If gwm->color.nbuff > 0 {
 *    For(ci=gwm->color.sbuff; ci<gwm->color.sbuff + gwm->color.nbuff; ci++) {
 *      gwm->color.pixel[ci] = Color pixel to be changed.
 *      gwm->color.xcolor[ci]= Requested color representation.
 *    }
 *  }
 * Output:
 *  If gwm->color.nbuff > 0 {
 *    For(ci=gwm->color.sbuff; ci<gwm->color.sbuff + gwm->color.nbuff; ci++) {
 *      gwm->color.pixel[ci] = New color pixel if the colormap is readonly.
 *      gwm->color.xcolor[ci]= Actual color representation installed.
 *    }
 *  }
 *  return    int      0 - OK.
 *                     1 - Error.
 */
static int gwm_update_colors(GWMdev *gwm)
{
  int bad_colors = 0;  /* The number of failed color assignments */
  int i;
/*
 * Device error?
 */
  if(gwm->bad_device)
    return 1;
/*
 * Are there any colors to be updated?
 */
  if(!gwm->color.monochrome && gwm->color.nbuff > 0) {
    XColor *xc = &gwm->color.xcolor[gwm->color.sbuff];
    unsigned long *pixel = &gwm->color.pixel[gwm->color.sbuff];
    int nbuff = gwm->color.nbuff;
/*
 * Install the colors in the color map.
 */
    switch(gwm->color.vi->class) {
    case PseudoColor:
    case GrayScale:
    case DirectColor:
      XStoreColors(gwm->display, gwm->color.cmap, xc, nbuff);
      break;
    case StaticColor:
    case StaticGray:
    case TrueColor:
      for(i=0; i<nbuff && !gwm->bad_device; i++) {
	if(XAllocColor(gwm->display, gwm->color.cmap, &xc[i])) {
	  if(gwm->color.initialized)
	    XFreeColors(gwm->display, gwm->color.cmap, &pixel[i], 1, (long)0);
	  pixel[i] = xc[i].pixel;
	} else {
	  bad_colors++;
	}
      }
      break;
    }
/*
 * Device error?
 */
    if(gwm->bad_device)
      return 1;
/*
 * Update the background color?
 */
    if(gwm->color.sbuff == 0)
      XSetWindowBackground(gwm->display, gwm->window, pixel[0]);
/*
 * Did any of the color assignments fail?
 */
    if(bad_colors > 0) {
       sprintf(errbuff,
	      "%s: Error setting the color representations of %d colors.\n",
	      GWM_IDENT, bad_colors);
       gwm_errmess(errbuff);
    }
  }
/*
 * Reset buffer pointers.
 */
  gwm->color.nbuff = 0;
  gwm->color.sbuff = 0;
  return gwm->bad_device!=0;
}

/*.......................................................................
 * Initialize the color representations in the color table.
 *
 * Input:
 *  gwm     GWMdev *    The PGPLOT /gwm device descriptor.
 * Output:
 *  gwm->color.xcolor[0..ncol] The color pixel definitions.
 *  return    int      0 - OK.
 *                     1 - Error.
 */
static int gwm_init_colors(GWMdev *gwm)
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
  if(!gwm->color.monochrome) {
    int ncol = (NCOLORS < gwm->color.ncol) ? NCOLORS : gwm->color.ncol;
    for(i=0; i<ncol; i++) {
      if(gwm_set_rgb(gwm, i, ctable[i][0], ctable[i][1], ctable[i][2]))
	return 1;
    }
/*
 * Initialize the rest of the colors with a grey-scale ramp.
 */
    for(i=ncol; i<gwm->color.ncol; i++) {
      float grey = (float)(i-NCOLORS) / (float)(gwm->color.ncol-1-NCOLORS);
      if(gwm_set_rgb(gwm, i, grey, grey, grey))
	return 1;
    }
  }
/*
 * Flush the new color definitions to the display.
 */
  if(gwm_update_colors(gwm))
    return 1;
/*
 * Record the new colormap state.
 */
  gwm->color.initialized = 1;
/*
 * Start with the foreground color set to white.
 */
  if(gwm_set_ci(gwm, 1))
    return 1;
  return 0;
}

/*.......................................................................
 * Create two cursors. One cursor is for the main gwm window when
 * cursor input is not possible.  The other is used by the InputOnly
 * window which overlays the output window and is used for cursor
 * input.
 *
 *  gwm        GWMdev * The PGPLOT /gwm device descriptor.
 *
 * Output:
 *  return     int   0 - OK.
 *                   1 - Error.
 */
static int gwm_new_cursors(GWMdev *gwm)
{
  XColor bg,fg;   /* Background and foreground colors */
/*
 * Device error?
 */
  if(gwm->bad_device)
    return 1;
/*
 * Create the new cursors.
 */
  gwm->norm_cursor = XCreateFontCursor(gwm->display, XC_arrow);
  gwm->live_cursor = XCreateFontCursor(gwm->display, XC_crosshair);
  if(gwm->norm_cursor==None || gwm->live_cursor==None) {
    sprintf(errbuff, "%s: Error creating cursor.\n", GWM_IDENT);
    gwm_errmess(errbuff);
    return 1;
  };
/*
 * Initialize the common parts of the color descriptors.
 */
  bg.pixel = fg.pixel = 0;
  bg.flags = fg.flags = DoRed | DoGreen | DoBlue;
  bg.pad = fg.pad = 0;
/*
 * A black background will be used for all cursors.
 */
  bg.red = bg.green = bg.blue = 0;
/*
 * Give the normal cursor a yellow foreground color.
 */
  fg.red   = fg.green = COLORMULT;
  fg.blue  = gwm->color.monochrome ? COLORMULT : 0;
  XRecolorCursor(gwm->display, gwm->norm_cursor, &fg, &bg);
/*
 * Give the active cursor a red foreground color.
 */
  fg.red   = COLORMULT;
  fg.green = fg.blue = gwm->color.monochrome ? COLORMULT : 0;
  XRecolorCursor(gwm->display, gwm->live_cursor, &fg, &bg);

  return 0;
}

/*.......................................................................
 * Clear the window and pixmap to start a new page.
 *
 * Input:
 *  gwm   GWMdev *  The PGPLOT /gwm device descriptor.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
static int gwm_clear(GWMdev *gwm)
{
  unsigned long fg;     /* Saved foreground color */
/*
 * Device error?
 */
  if(gwm->bad_device)
    return 1;
/*
 * We are about to change the current foreground color, so save the
 * current value to be re-instated shortly.
 */
  fg = gwm->gcv.foreground;
/*
 * Clear the pixmap by drawing an opaque rectangle over it in the background
 * color.
 */
  gwm_set_ci(gwm, 0);
  if(gwm->pixmap != None) {
    XFillRectangle(gwm->display, gwm->pixmap, gwm->gc, 0, 0,
		   gwm->geom.width, gwm->geom.height);
    if(gwm->bad_device)
      return 1;
  }
/*
 * Re-instate the foreground color.
 */
  gwm->gcv.foreground = fg;
  XSetForeground(gwm->display, gwm->gc, gwm->gcv.foreground);
  if(gwm->bad_device)
    return 1;
/*
 * Mark the pixmap as unmodified.
 */
  gwm->update.modified = 0;
/*
 * Clear the window itself.
 */
  XClearArea(gwm->display, gwm->window, gwm->geom.xscroll, gwm->geom.yscroll,
             gwm->geom.width, gwm->geom.height, False);
  if(gwm->bad_device)
    return 1;
  XFlush(gwm->display);
  if(gwm->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * Set the foreground color.
 *
 * Input:
 *  gwm   GWMdev *  The PGPLOT /gwm device descriptor.
 *  ci      int    The PGPLOT color index to instate as the foreground
 *                 color.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
static int gwm_set_ci(GWMdev *gwm, int ci)
{
/*
 * Device error?
 */
  if(gwm->bad_device)
    return 1;
/*
 * Assign white to out-of range color indexes.
 */
  if(ci < 0 || ci >= gwm->color.ncol)
    ci = 1;
/*
 * Determine the color pixel associated with the given color index.
 */
  if(gwm->color.monochrome) {
    gwm->gcv.foreground = ci==1 ? WhitePixel(gwm->display, gwm->screen) :
                                 BlackPixel(gwm->display, gwm->screen);
  } else {
    gwm->gcv.foreground = gwm->color.pixel[ci];
  }
/*
 * Instate the new foreground color.
 */
  XSetForeground(gwm->display, gwm->gc, gwm->gcv.foreground);
  if(gwm->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * Update the vertices of the rectangular area that has been modified
 * since the last time the window was updated from the pixmap.
 *
 * Input:
 *  gwm     GWMdev * The PGPLOT /gwm device descriptor.
 *  x         int   The x-axis pixel index that the rectangular update area
 *                  must be extended to include.
 *  y         int   The y-axis pixel index that the rectangular update area
 *                  must be extended to include.
 *  diameter  int   The diameter of the locus in pixels. For line or
 *                  point drawing operations this is usually the line width.
 */
static void gwm_mark_modified(GWMdev *gwm, int x, int y, int diameter)
{
  int radius = diameter/2;
/*
 * Expand the current rectangle to include point (x,y).
 */
  if(gwm->update.modified) {
    if(x - radius < gwm->update.xmin)
      gwm->update.xmin = x - radius;
    if(x + radius > gwm->update.xmax)
      gwm->update.xmax = x + radius;
    if(y - radius < gwm->update.ymin)
      gwm->update.ymin = y - radius;
    if(y + radius > gwm->update.ymax)
      gwm->update.ymax = y + radius;
  } else {
    gwm->update.xmin = x - radius;
    gwm->update.xmax = x + radius;
    gwm->update.ymin = y - radius;
    gwm->update.ymax = y + radius;
    gwm->update.modified = 1;
  }
  return;
}

/*.......................................................................
 * Flush changes to the pixmap to the window.
 *
 * Input:
 *  gwm   GWMdev *  The PGPLOT /gwm device descriptor.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
static int gwm_flush(GWMdev *gwm)
{
  if(gwm->bad_device)
    return 1;
/*
 * Flush buffered opcodes if necessary.
 */
  if(gwm->flush_opcode_fn != (Flush_Opcode_fn) 0) {
    (*gwm->flush_opcode_fn)(gwm);
    gwm->flush_opcode_fn = (Flush_Opcode_fn) 0;
    if(gwm->bad_device)
      return 1;
  }
/*
 * Copy the modified rectangular area of the pixmap to the /gwm window.
 */
  if(gwm->update.modified) {
/*
 * Enforce bounds on the area to be updated.
 */
    if(gwm->update.xmin < 0)
      gwm->update.xmin = 0;
    if(gwm->update.ymin < 0)
      gwm->update.ymin = 0;
    if(gwm->update.xmax > gwm->geom.width - 1)
      gwm->update.xmax = gwm->geom.width - 1;
    if(gwm->update.ymax > gwm->geom.height - 1)
      gwm->update.ymax = gwm->geom.height - 1;
/*
 * Copy the area to be updated from the pixmap to the window.
 */
    if(gwm->pixmap != None && !gwm->bad_device) {
      XCopyArea(gwm->display, gwm->pixmap, gwm->window, gwm->gc,
		gwm->update.xmin, gwm->update.ymin,
		(unsigned) (gwm->update.xmax - gwm->update.xmin + 1),
		(unsigned) (gwm->update.ymax - gwm->update.ymin + 1),
		gwm->geom.xscroll + gwm->update.xmin,
                gwm->geom.yscroll + gwm->update.ymin);
      if(gwm->bad_device)
	return 1;
    }
    gwm->update.modified = 0;
  }
  XFlush(gwm->display);
  if(gwm->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * Open a /gwm window and return an initialized /gwm PGPLOT device descriptor.
 *
 * Input:
 *  gwm_name  char *   A string containing the name of the GWM window.
 *  mode        int     The type of window to open.
 *                       Unused at present 
 * Output:
 *  return    GWMdev *   THe PGPLOT /gwm device descriptor, or NULL on error.
 */
static GWMdev *new_GWMdev(char *gwm_name, int mode)
{
   XWindowAttributes attr;
   GWMdev *gwm = NULL;     /* The descriptor to be returned */
   static Display *display;
   Window window, parent, root, *children;
   unsigned int nchildren;
   int i;
   char *name;
   char *argv[3];
   int argc;
   unsigned long mask;
   int x, y;
   unsigned int border;
   unsigned int s_pix_width;   /* Screen width in pixels */
   unsigned int s_pix_height;  /* Screen height in pixels */
   unsigned int s_mm_width;    /* Screen width in mm */
   unsigned int s_mm_height;   /* Screen height in mm */
/*
 * Open a connection to the X display server if necessary.
 */
   if(display==NULL)
   {
      display = XOpenDisplay(NULL);
      if(display==NULL) {
         sprintf(errbuff, "%s: GWM cannot connect to X server\n", GWM_IDENT);
         gwm_errmess(errbuff);
         return del_GWMdev(gwm,0);
      }
   } 

/*
 * Allocate the descriptor.
 */
   gwm = (GWMdev *) calloc(1, sizeof(GWMdev));

/*
 * Check for a previously created GWM window of this name
 */
   if( GWM_FindWindow(display, gwm_name, &window) != GWM_SUCCESS)
   {
      argv[0] = "gwmXrefresh";
      argv[1] = gwm_name;
      argv[2] = NULL;
      argc = 1;
      if( GWM_CreateWindow( argc, argv, &display, gwm_name) != GWM_SUCCESS ||
          GWM_FindWindow( display, gwm_name, &window) != GWM_SUCCESS ) 
      {
         sprintf(errbuff, "%s: cannot create GWM window\n", GWM_IDENT);
         gwm_errmess(errbuff);
         return del_GWMdev(gwm,0);
      }
   } else {
/* Previous GWM window exists - check for a residual input child window 
 * and delete if found. Actually just delete ALL subwindows for now!
 */
      XQueryTree(display, window, &root, &parent, &children, &nchildren);
      for(i=0; i<nchildren; i++)
         XDestroyWindow(display, children[i]);
      XFree(children);
   }   
   gwm->window = window;
/*
 * The GWM window may be in a hierarchy (for example, a Tcl canvas) and
 * we need to find the "managed" window
 */
   XFetchName(display, window, &name);
   if( name == NULL) {
      do {
         XQueryTree(display, window, &root, &parent, &children,
                    &nchildren);
         XFree(children);
         XFetchName(display, window, &name);
         if( name != NULL ) {
            XFree(name);
            break;
         } else
           window = parent;
       } while( window != root);
       if(window == root)
          window = gwm->window;
       gwm->managed = window;
       window = gwm->window;
    } else
       gwm->managed = window;
/*    XMapRaised(display, gwm->managed); */
            
 /*
 * We cannot cope with overlay windows just for now (and should only get
 * these when existing windows are accessed)
 */
   if( GWM_GetOvMask(display, window, &mask) == GWM_SUCCESS &&
        (signed) mask != -1L ) {
      sprintf(errbuff, "%s: (new_GWMdev) - overlay windows not supported.\n",
              GWM_IDENT);
      gwm_errmess(errbuff);
      return del_GWMdev(gwm,0);
   }

   gwm->display = display;
   gwm->window = window;
   gwm->screen = DefaultScreen(gwm->display);
   gwm->name = (char *) malloc(strlen(gwm_name)+1);
   strcpy(gwm->name,gwm_name);
/*
 * Install an error handler for non-fatal errors. If we don't do this then
 * Xlib will do its own error handling, which includes killing the program.
 */
   XSetErrorHandler(gwm_error);
/*
 * We want to know if the PGPLOT window gets destroyed.
 */
  if(gwm_add_events(gwm, (long) StructureNotifyMask))
    return del_GWMdev(gwm,0);
/*
 * Get the window attributes
 */
  if(!XGetWindowAttributes(gwm->display, gwm->window, &attr)) {
     sprintf(errbuff,
	    "%s: (new_GWMdev) Error getting attributes for window 0x%lx.\n",
	    GWM_IDENT, (unsigned long) gwm->window);
     gwm_errmess(errbuff);
     return del_GWMdev(gwm,0);
  };
/*
 * Get the window colour information and visual details
 */
   GWM_GetColTable( display, window, &gwm->color.pixel,
                    (long *) &gwm->color.ncol);
   gwm->color.xcolor = (XColor *) malloc(sizeof(XColor) * gwm->color.ncol);
   gwm->color.vi = gwm_visual_info(gwm->display, gwm->screen, attr.visual);
   gwm->color.cmap = attr.colormap;
/*
 * Get the X resource identifier of the backing pixmap for the window
 */
   GWM_GetPixmap(display, window, &gwm->pixmap);
/*
 * Get the geometry for the pixmap (the fundamental dwarable for GWM) 
 */
   XGetGeometry( display, gwm->pixmap, &root, &x, &y, &gwm->geom.width,
	         &gwm->geom.height, &border, &gwm->geom.depth);
/*
 * Determine the current screen width and height in mm and pixels.
 */
   s_pix_width  = DisplayWidth(display, gwm->screen);
   s_mm_width   = DisplayWidthMM(display, gwm->screen);
   s_pix_height = DisplayHeight(display, gwm->screen);
   s_mm_height  = DisplayHeightMM(display, gwm->screen);
/*
 * Determine the device resolution in pixels per inch.
 */
   gwm->geom.xpix_per_inch = (25.4 * ((float)s_pix_width / 
                                          (float)s_mm_width))+0.5;
   gwm->geom.ypix_per_inch = (25.4 * ((float)s_pix_height / 
                                          (float)s_mm_height))+0.5;
/*
 *  find the pix map size in metres
 */
/*
 *  find the display size and convert the pix map size to metres
 */
   gwm->geom.xmetre = (float)gwm->geom.width *
		(float)s_mm_width / (float)s_pix_width / 1000.0;

   gwm->geom.ymetre = (float)gwm->geom.height *
		(float)s_mm_height / (float)s_pix_height / 1000.0;
/*
 * Get the X and Y scroll offsets of the window (with respect to the pixmap)
 */
   GWM_GetScroll(display, window, &gwm->geom.xscroll, &gwm->geom.yscroll);
/*
 * Get the IDs of the normal and active cursors.
 */
  if(gwm_new_cursors(gwm))
    return del_GWMdev(gwm,0);
/*
 * Set the cursor for the window.
 */
  XDefineCursor(gwm->display, gwm->window, gwm->norm_cursor);
/*
 * Create and initialize a graphical context descriptor. This is where
 * Line widths, line styles, fill styles, plot color etc.. are
 * recorded.
 */
   gwm->gcv.line_width = 1;
   gwm->gcv.cap_style = CapRound;
   gwm->gcv.join_style = JoinRound;
   gwm->gcv.fill_rule = EvenOddRule;
   gwm->gcv.graphics_exposures = False;
   gwm->gcv.foreground = WhitePixel(gwm->display, gwm->screen);
   gwm->gc = XCreateGC(gwm->display, gwm->window, (unsigned long) 
            (GCLineWidth | GCCapStyle | GCJoinStyle | GCFillRule | 
            GCGraphicsExposures | GCForeground), &gwm->gcv);
   if(gwm->gc==NULL || gwm->bad_device) {
      sprintf(errbuff, "%s: Failed to allocate graphical context.\n", GWM_IDENT);
      gwm_errmess(errbuff);
     return del_GWMdev(gwm,0);
   }
/*
 * Allocate the buffers that will be used to compose a line
 * of pixels.
 */
   if(gwm_get_image(gwm, GWM_IMAGE_LEN))
      return del_GWMdev(gwm,0);
/*
 * Return the initialized descriptor for use.
 */
   return gwm;
}

/*.......................................................................
 * Delete a PGPLOT /gwm device and its descriptor.
 *
 * Input:
 *  gwm      GWMdev *  The descriptor of the device to be deleted.
 *  partial   int    0 - Normal deletion - delete everything.
 *                   1 - Close the display connection and mark all
 *                       resources as deleted but don't delete the
 *                       container - also set gwm->bad_device==1.
 * Output:
 *  return  GWMdev *  Allways NULL. Use like gwm = del_GWMdev(gwm,0);
 */
static GWMdev *del_GWMdev(GWMdev *gwm, int partial)
{
  if(gwm) {
/*
 * Mark the device as unusable as the first operation so that if
 * any X errors are generated during cleanup, they are not reported.
 */
    gwm->bad_device = 1;
/*
 * Delete the graphical context descriptor.
 */
    if(gwm->gc)
      XFreeGC(gwm->display, gwm->gc);
    gwm->gc = NULL;
/*
 * Delete the image buffers.
 */
    gwm->image.npix = 0;
    if(gwm->image.buff)
      free(gwm->image.buff);
    gwm->image.buff = NULL;
    if(gwm->image.xi)
      XFree((char *)gwm->image.xi);
    gwm->image.xi = NULL;
/*
 * Check for un-freed polygon points.
 */
    if(gwm->poly.points)
      free((char *)gwm->poly.points);
    gwm->poly.points = NULL;
/*
 * Zap the arrays of color pixels and color pixel definitions.
 */
    if(gwm->color.pixel)
    {
      free((char *)gwm->color.pixel);
      gwm->color.pixel = NULL;
    }
    if(gwm->color.xcolor) {
      free((char *)gwm->color.xcolor);
      gwm->color.xcolor = NULL;
    }
/*
 * Discard the visual info descriptor.
 */
    if(gwm->color.vi) {
      XFree((char *)gwm->color.vi);
      gwm->color.vi = NULL;
    }
/*
 * Close the connection to the display server - this will also delete
 * all X-resources.
 */
    if(gwm->display != NULL) {
/*
 * Clear the local event mask for the PGPLOT /gwm window.
 */
      if(gwm->window != None)
	XSelectInput(gwm->display, gwm->window, (long) NoEventMask);
/*
 * Close the display if this is the last gwm window.
 */
      if( gwm == device_list ) {
         XCloseDisplay(gwm->display);
         gwm->display = NULL;
      }
    }
/*
 * Delete the window name.
 */
    if(gwm->name != NULL) {
       free(gwm->name);
       gwm->name = NULL;
    }
/*
 * Mark effected resources as deleted.
 */
    gwm->window = None;
    gwm->pixmap = None;
    gwm->flush_opcode_fn = (Flush_Opcode_fn) 0;
    gwm->update.modified = 0;
/*
 * Delete the descriptor if required.
 */
    if(!partial) {
      free((char *)gwm);
      gwm = NULL;
    };
  };
  return gwm;
}

/*.......................................................................
 * Before using a given /gwm device descriptor call this function to check
 * that it is usable. If it isn't, 0 will be returned and you should not
 * attempt to use the descriptor. If the descriptor is NULL an error
 * message will be presented.
 *
 * Input:
 *  gwm      GWMdev *  The device descriptor to be checked.
 * Output:
 *  return     int    1 - Descriptor OK.
 *                    0 - Error - don't use /gwm.
 */
static int gwm_ok(GWMdev *gwm)
{
  XEvent event;
  if(gwm==NULL) {
    sprintf(errbuff, "%s: Device not open.\n", GWM_IDENT);
    gwm_errmess(errbuff);
    return 0;
  }
/*
 * If the window is marked as unusable, it must have been set that way
 * after an error was detected. Assume that the error must already
 * have been reported.
 */
  if(gwm->bad_device)
    return 0;
/*
 * Check for any unprocessed DestroyNotify events on the window.
 */
  while( XCheckWindowEvent( gwm->display, gwm->window, 
                            StructureNotifyMask, &event) == True)
      if( event.type == DestroyNotify )
         return !gwm_bad_device(gwm);
  return 1;
}

/*.......................................................................
 * Present the active cursor, wait for the user to press a button or
 * keyboard key, then retrack the active cursor and return the cursor
 * position and key pressed.
 *
 * Input:
 *  gwm   GWMdev *   The PGPLOT /gwm device descriptor.
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
static int gwm_read_cursor(GWMdev *gwm, int mode, int posn, XPoint *ref,
			  XPoint *pos, char *key)
{
  int finished = 0;       /* True when cursor succesfully read */
  XEvent event;           /* The latest event */
  XPoint last;            /* Last recorded position of cursor */
  Band *bc=NULL;          /* Band-cursor descriptor */
  int warped=0;           /* Zero until the cursor has been positioned */
 /*
 * Device error?
 */
  if(gwm->bad_device)
    return 1;
/*
 * Ensure that the input positions are within the pixmap and window bounds.
 */
  if(gwm_bound_cursor(gwm, ref) || gwm_bound_cursor(gwm, pos))
    return 1;
/*
 * Create an InputOnly window as a child of the main GWM window.
 */
  if(!gwm_create_input_window(gwm))
    return 1;
/*
 * Make sure that the window is up to date.
 */
  if(gwm_flush(gwm))
    return gwm_end_input(gwm, bc, 1);
 /*
 * De-iconify and bring the GWM windows to the foreground.
 * Use the "managed" window to cope with the Tcl based server.
 */
  XMapRaised(gwm->display, gwm->managed);
  if(gwm->bad_device)
    return gwm_end_input(gwm, bc, 1);
  XSync(gwm->display, False);
   if(gwm->bad_device)
    return gwm_end_input(gwm, bc, 1);
/*
 * Set up for modes that maintain elastic lines following the cursor.
 */
  if((bc=gwm_new_Band(gwm, mode, ref))==NULL)
    return gwm_end_input(gwm, bc, 1);
/*
 * If the cursor is in the window, locate its position,
 * after warping if requested.
 */
  if(gwm_locate_cursor(gwm, pos, posn, &last)) {
    warped = 1;
/*
 * Draw the cursor.
 */
    if(gwm->bad_device || gwm_bound_cursor(gwm, &last) ||
       gwm_draw_cursor(gwm, bc, &last))
      return gwm_end_input(gwm, bc, 1);
  }
/*
 * Discard un-handled ButtonPress, KeyPress and MotionNotify events.
 *
 * while(gwm_check_window_event(gwm, gwm->inwin, (long)
 *	      (ButtonPressMask | KeyPressMask | PointerMotionMask), &event));
 * if(gwm->bad_device)
 *   return gwm_end_input(gwm, bc, 1);
 */
/*
 * Loop for cursor events.
 */
  while(!finished) {
/*
 * Handle the next selected event.
 */
    if(gwm_next_event(gwm, &event))
      return gwm_end_input(gwm, bc, 1);
    switch(event.type) {
    case Expose:
      if(gwm_expose(gwm, &event))
	return gwm_end_input(gwm, bc, 1);
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
	}
      }
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
	if(gwm->bad_device)
	  return gwm_end_input(gwm, bc, 1);
/*
 * Ignore modifier keys and all but single character keys.
 */
	if(nret==1 && (keysym < XK_Shift_L || keysym > XK_Hyper_R)) {
	  pos->x = event.xkey.x;
	  pos->y = event.xkey.y;
	  if(key)
	    *key = buffer[0];
	  finished = 1;
	}
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
	  if(gwm_shift_cursor(gwm, keysym, event.xkey.state))
	    return gwm_end_input(gwm, bc, 1);
	  break;
	}
      }
      break;
    case EnterNotify:
/*
 * The cursor may still be drawn if a button was pressed when the
 * cursor was last moved out of the window. The resulting
 * passive grab will have continued to deliver motion events to
 * the PGPLOT window.
 */
      if(gwm_erase_cursor(gwm, bc))
	return gwm_end_input(gwm, bc, 1);
/*
 * If the cursor is in the window, locate its position. If this is
 * the first time that the cursor has been in the window and warping
 * has been requested, this also inolves pre-positioning the cursor
 * and setting input focus.
 */
      if(gwm_locate_cursor(gwm, pos, posn && !warped, &last)) {
	warped = 1;
/*
 * Draw the cursor.
 */
	if(gwm->bad_device || gwm_bound_cursor(gwm, &last) ||
	   gwm_draw_cursor(gwm, bc, &last))
	  return gwm_end_input(gwm, bc, 1);
      }
      break;
    case LeaveNotify:
      if(gwm_erase_cursor(gwm, bc))
	return gwm_end_input(gwm, bc, 1);
      break;
    case MotionNotify:
/*
 * Discard all but the last MotionNotify event.
 */
      while(gwm_check_window_event(gwm, gwm->window, (long)(PointerMotionMask),
				  &event));
      if(gwm->bad_device || gwm_erase_cursor(gwm, bc))
	return gwm_end_input(gwm, bc, 1);
      last.x = event.xmotion.x;
      last.y = event.xmotion.y;
      if(gwm_bound_cursor(gwm, &last) || gwm_draw_cursor(gwm, bc, &last))
	return gwm_end_input(gwm, bc, 1);
      break;
    default:
      break;
    }
  }
/*
 * Clean up.
 */
  return gwm_end_input(gwm, bc, gwm->bad_device!=0);
}

/*.......................................................................
 * This is a private function of gwm_read_cursor(). Create an InputOnly
 * window which overlays (and is a child of) the main gwm window. This
 * is used to read the cursor as another application may be soliciting
 * ButtonPress events in the gwm window itself.
 *
 * Input:
 *  gwm          GWMdev *  The PGPLOT /gwm device descriptor.
 *
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
static int gwm_create_input_window(GWMdev *gwm)
{
   XSetWindowAttributes win_attrib;
/*
 * Ensure that the scroll area data is up to date. The user may have
 * messed with the GWM window size since it was opened.
 */
  GWM_GetScroll( gwm->display, gwm->window, &gwm->geom.xscroll,
                 &gwm->geom.yscroll );
/*
 * Create an InputOnly window as a child of the main GWM window.
 */
  win_attrib.event_mask =
      ButtonPressMask | KeyPressMask;
  gwm->inwin = XCreateWindow( gwm->display, gwm->window, gwm->geom.xscroll,
                         gwm->geom.yscroll, gwm->geom.width,
                         gwm->geom.height, 0, 0, InputOnly,
                         CopyFromParent, CWEventMask, &win_attrib);
  XMapWindow( gwm->display, gwm->inwin );
/*
 * Set the "active" cursor for this window.
 */
  XDefineCursor(gwm->display, gwm->inwin, gwm->live_cursor);

  return 1;
}

 
/*.......................................................................
 * This is a private function of gwm_read_cursor(). If the user has just
 * pressed one of the keyboard or keypad arrow keys, it moves the cursor
 * by one pixel in the corresponding direction. If one of the shift keys
 * is also held down, then the cursor is moved by ARROW_KEY_VELOCITY
 * pixels instead of one. If the resulting shift would move the cursor
 * out of the bounds of the pgplot window pixmap, then the motion is
 * aborted.
 *
 * Input:
 *  gwm          GWMdev *  The PGPLOT /gwm device descriptor.
 *  keysym      KeySym    The key symbol returned by XLookupString() wrt
 *                        the arrow-button key-press.
 *  modifiers unsigned    The Event::xkey.state key-modifier mask
 *                        associated with the key-press.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
static int gwm_shift_cursor(GWMdev *gwm, KeySym keysym, unsigned int modifiers)
{
  Window p_child;         /* The child window that contains the pointer */
  int p_win_x, p_win_y;   /* The pointer coordinates wrt gwm->window */
  int p_root_x, p_root_y; /* The pointer coordinates wrt the root window */
  Window p_root_win;      /* The root window that contains the cursor */
  unsigned int p_mask;    /* The bit-mask of button states etc.. */
  int dx=0;               /* The amount to move the cursor in X */
  int dy=0;               /* The amount to move the cursor in Y */
/*
 * Determine the current position of the cursor.
 */
  XQueryPointer(gwm->display, gwm->window, &p_root_win, &p_child,
		&p_root_x, &p_root_y, &p_win_x, &p_win_y, &p_mask);
  if(gwm->bad_device)
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
  }
/*
 * If one of the shift keys is held down, increase the size of the
 * move to ARROW_KEY_VELOCITY pixels in the specified direction.
 */
  if(modifiers & ShiftMask) {
    dx *= ARROW_KEY_VELOCITY;
    dy *= ARROW_KEY_VELOCITY;
  }
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
 * gwm_read_cursor() which also wants LeaveNotify events, would be
 * slower to operate and would be unavoidably subject to race conditions.
 */
  if(p_win_x < 0 || p_win_x >= gwm->geom.width ||
     p_win_y < 0 || p_win_y >= gwm->geom.height)
    return 0;
/*
 * Move the cursor to the new location.
 */
  XWarpPointer(gwm->display, None, gwm->window, 0, 0, 0, 0, p_win_x, p_win_y);
  if(gwm->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * Private return function of gwm_read_cursor().
 *
 * Input:
 *  gwm   GWMdev *   The PGPLOT /gwm device descriptor.
 *  bc     Band *   The cursor banding descriptor to be deleted.
 *  status  int     Required gwm_read_cursor() return status.
 * Output:
 *  return  int     The value of 'status'.
 */
static int gwm_end_input(GWMdev *gwm, Band *bc, int status)
{
  if(bc) {
    if(gwm_erase_cursor(gwm, bc))
      status=1;
    if(gwm_flush(gwm))
      status=1;
    bc = gwm_del_Band(gwm, bc);
  }
  XDestroyWindow( gwm->display, gwm->inwin);
  gwm->inwin = 0;
  XSync(gwm->display, False);
  return status;
}

/*.......................................................................
 * Convert from the coordinates sent by PGPLOT in rbuf[...] to an
 * X-windows point in the coordinate system of the window. 
 *
 * Input:
 *  gwm     GWMdev *   The PGPLOT /gwm device descriptor.
 *  xy      float [2] Array of two floats containing PGPLOT coordinates
 *                    arranged as x followed by y.
 * Output:
 *  xp     XPoint *   The converted coordinates will be assigned to xp->x
 *                    and xp->y.
 */
static void gwm_xy_to_XPoint(GWMdev *gwm, float *xy, XPoint *xp)
{
  xp->x = (int)(xy[0] + 0.5);
  xp->y = gwm->geom.height - (int)(xy[1] + 0.5);
}

/*.......................................................................
 * Convert from window pixel coordinates to PGPLOT coordinates, in a
 * form that can be returned to PGPLOT via rbuf[...].
 *
 * Input:
 *  gwm     GWMdev *   The PGPLOT /gwm device descriptor.
 *  xp     XPoint *   The window pixel-coordinates to be converted.
 * Output:
 *  xy      float [2] Output array of two floats in which to place the
 *                    PGPLOT coordinates, arranged as x followed by y.
 */
static void gwm_XPoint_to_xy(GWMdev *gwm, XPoint *xp, float *xy)
{
  xy[0] = (float) xp->x;
  xy[1] = (float) (gwm->geom.height - xp->y);
}

/*.......................................................................
 * Draw a horizontal line of pixels at a given location, from a float
 * array of PGPLOT color indexes.
 *
 * Input:
 *  gwm     GWMdev *   The PGPLOT /gwm device descriptor.
 *  start  XPoint *   The position to start the line at.
 *  cells   float *   An array of ncell pixel PGPLOT color indexes.
 *  ncell     int     The number of cells in cells[].
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
static int gwm_image_line(GWMdev *gwm, XPoint *start, float *cells, int ncell)
{
  int ndone;  /* The number of pixels drawn so far */
  int i;
/*
 * Device error?
 */
  if(gwm->bad_device)
    return 1;
/*
 * Quietly ignore the call if we don't have a pixmap.
 */
  if(gwm->pixmap != None) {
/*
 * Draw up to gwm->image.npix pixels at a time. This is the size of the
 * gwm->image.buff[] buffer.
 */
    for(ndone=0; !gwm->bad_device && ndone<ncell; ndone += gwm->image.npix) {
      int ntodo = ncell-ndone;
      int nimage = ntodo < gwm->image.npix ? ntodo : gwm->image.npix;
/*
 * Load the image buffer with the color cell indexes assigned to the
 * given PGPLOT color indexes.
 */
      if(gwm->color.vi->depth == 8) {
	for(i=0; i<nimage; i++)
	  gwm->image.buff[i] = gwm->color.pixel[(int) (cells[ndone+i] + 0.5)];
      } else {
	for(i=0; i<nimage; i++) {
	  XPutPixel(gwm->image.xi, i, 0,
		    gwm->color.pixel[(int) (cells[ndone+i] + 0.5)]);
	}
      }
/*
 * Display the image.
 */
      XPutImage(gwm->display, gwm->pixmap, gwm->gc, gwm->image.xi, 0, 0,
		start->x+ndone, start->y, (unsigned) nimage, (unsigned) 1);
    }
/*
 * Extend the region to be updated on the next flush.
 */
    gwm_mark_modified(gwm, start->x, start->y, 1);
    gwm_mark_modified(gwm, start->x + ncell - 1, start->y, 1);
  }
  if(gwm->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * Call this function when an Expose event is received. It will then
 * re-draw the exposed region from the gwm->pixmap.
 *
 * Input:
 *  gwm      GWMdev *  The PGPLOT /gwm device descriptor.
 *  event   XEvent *  The expose event.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
static int gwm_expose(GWMdev *gwm, XEvent *event)
{
/*
 * Device error?
 */
  if(gwm->bad_device)
    return 1;
  if(event->type == Expose && gwm->pixmap != None) {
    XCopyArea(gwm->display, gwm->pixmap, gwm->window, gwm->gc,
	      event->xexpose.x, event->xexpose.y,
	      (unsigned) event->xexpose.width, (unsigned) event->xexpose.height,
	      gwm->geom.xscroll + event->xexpose.x,
              gwm->geom.yscroll + event->xexpose.y);
    if(gwm->bad_device)
      return 1;
    XFlush(gwm->display);
    if(gwm->bad_device)
      return 1;
  }
  return 0;
}

/*.......................................................................
 * Set up for a band cursor and return its descriptor.
 *
 * Input:
 *  gwm   GWMdev *   The PGPLOT /gwm device descriptor.
 *  mode    int     0 - No band cursor.
 *                  1 - Line between reference position and cursor.
 *                  2 - Rectangle drawn with opposite corners at reference
 *                      and cursor position.
 *  ref  XPoint *   The reference position.
 * Output:
 *  return Band *   A pointer to a static internal container of cursor
 *                  resources. Call gwm_del_Band() to release these resources
 *                  and return the event mask to normal.
 */
static Band *gwm_new_Band(GWMdev *gwm, int mode, XPoint *ref)
{
  static Band band;        /* Return container */
  long event_mask=0;       /* Bit map of events to be caught */
/*
 * Device error?
 */
  if(gwm->bad_device)
    return NULL;
/*
 * Initialize values at least to the point at which gwm_del_Band() can
 * safely be called.
 */
  band.line_width = gwm->gcv.line_width;
  band.mode = mode;
  band.ref = *ref;
  band.end = *ref;
/*
 * All cursor types require us to catch the following events.
 */
  event_mask = KeyPressMask | ButtonPressMask | 
               EnterWindowMask | LeaveWindowMask;
/*
 * Set up for a band cursor?
 */
  if(band.mode != 0 ) {
/*
 * Arrange for the band cursor to be drawn with a line width of 0.
 */
    if(band.line_width != 0) {
      XGCValues attr;
      band.line_width = attr.line_width = 0;
      XChangeGC(gwm->display, gwm->gc, (unsigned long) GCLineWidth, &attr);
      if(gwm->bad_device)
	return NULL;
    }
/*
 * Select for cursor motion events along with the normal events.
 */
      event_mask |= PointerMotionMask;
  }
/*
 * Register the additional event types that are now to be caught.
 */
  XSelectInput(gwm->display, gwm->inwin, event_mask);
  return &band;
}

/*.......................................................................
 * Release band cursor resources and return the window event mask to
 * its normal state.
 *
 * Input:
 *  gwm   GWMdev *   The PGPLOT /gwm device descriptor.
 *  bc     Band *   The band-cursor descriptor.
 * Output:
 *  return Band *   Always NULL.
 */
static Band *gwm_del_Band(GWMdev *gwm, Band *bc)
{
/*
 * Prevent the event buffer from overflowing by removing superflous events
 * from the set of those to be caught.
 */
  gwm_rem_events(gwm, (long) (ExposureMask | KeyPressMask | ButtonPressMask |
			    EnterWindowMask | LeaveWindowMask |
			    PointerMotionMask));
/*
 * If the line width was changed for rubber banding, re-instate the
 * original line width.
 */
  if(bc->line_width != gwm->gcv.line_width)
    XChangeGC(gwm->display, gwm->gc, (unsigned long) GCLineWidth, &gwm->gcv);
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
static int gwm_bound_cursor(GWMdev *gwm, XPoint *xp)
{
  XWindowAttributes attr;  /* Current window attributes */
  int xmax, ymax;          /* Max usable X and Y coordinates */
/*
 * Device error?
 */
  if(gwm->bad_device)
    return 1;
/*
 * Get the current window dimensions.
 */
  XGetWindowAttributes(gwm->display, gwm->window, &attr);
  if(gwm->bad_device)
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
  xmax = ((gwm->geom.width < attr.width) ? gwm->geom.width : attr.width) - 1;
  ymax = ((gwm->geom.height < attr.height) ? gwm->geom.height : attr.height) - 1;
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
 *  gwm  GWMdev *   The PGPLOT /gwm device descriptor.
 *  bc    Band *   A cursor descriptor returned by gwm_new_band().
 *  end XPoint *   The current cursor position.
 * Output:
 *  return int     0 - OK.
 *                 1 - Error.
 */
static int gwm_draw_cursor(GWMdev *gwm, Band *bc, XPoint *end)
{
/*
 * Device error?
 */
  if(gwm->bad_device)
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
    /* Nothing to be done  - use X-windows system cursor */
    break;
  case 1:
    XDrawLine(gwm->display, gwm->window, gwm->gc,
              gwm->geom.xscroll + bc->ref.x,
              gwm->geom.yscroll + bc->ref.y,
	      gwm->geom.xscroll + bc->end.x,
              gwm->geom.yscroll + bc->end.y);
    break;
  case 2:  /* Draw a rectangle */
    {
      int x = bc->ref.x < bc->end.x ? bc->ref.x : bc->end.x;
      int y = bc->ref.y < bc->end.y ? bc->ref.y : bc->end.y;
      unsigned int width = (unsigned int) abs(bc->ref.x - bc->end.x);
      unsigned int height = (unsigned int) abs(bc->ref.y - bc->end.y);
      XDrawRectangle(gwm->display, gwm->window, gwm->gc, 
                     gwm->geom.xscroll + x,
                     gwm->geom.yscroll + y,
                     width, height);
    }
    break;
  case 3:  /* Two horizontal lines */
    XDrawLine(gwm->display, gwm->window, gwm->gc,
              gwm->geom.xscroll,
              gwm->geom.yscroll + bc->end.y,
	      gwm->geom.xscroll + (int)gwm->geom.width-1,
              gwm->geom.yscroll + bc->end.y);
    if(gwm->bad_device)
      return 1;
    XDrawLine(gwm->display, gwm->window, gwm->gc,
              gwm->geom.xscroll,
              gwm->geom.yscroll + bc->ref.y,
	      gwm->geom.xscroll + (int)gwm->geom.width-1,
              gwm->geom.yscroll + bc->ref.y);
    break;
  case 4:  /* Two vertical lines */
    XDrawLine(gwm->display, gwm->window, gwm->gc,
              gwm->geom.xscroll + bc->end.x,
              gwm->geom.yscroll,
	      gwm->geom.xscroll + bc->end.x,
              gwm->geom.yscroll + (int)gwm->geom.height-1);
    if(gwm->bad_device)
      return 1;
    XDrawLine(gwm->display, gwm->window, gwm->gc,
              gwm->geom.xscroll + bc->ref.x,
              gwm->geom.yscroll,
	      gwm->geom.xscroll + bc->ref.x,
              gwm->geom.yscroll + (int)gwm->geom.height-1);
    break;
  case 5: /* One horizontal line through the cursor */
    XDrawLine(gwm->display, gwm->window, gwm->gc,
              gwm->geom.xscroll,
              gwm->geom.yscroll + bc->end.y,
	      gwm->geom.xscroll + (int)gwm->geom.width-1,
              gwm->geom.yscroll + bc->end.y);
    break;
  case 6: /* One vertical line through the cursor */
    XDrawLine(gwm->display, gwm->window, gwm->gc,
              gwm->geom.xscroll + bc->end.x,
              gwm->geom.yscroll,
	      gwm->geom.xscroll + bc->end.x,
              gwm->geom.yscroll + (int)gwm->geom.height-1);
    break;
  case 7: /* Cross hair */
    XDrawLine(gwm->display, gwm->window, gwm->gc,
              gwm->geom.xscroll,
              gwm->geom.yscroll + bc->end.y,
	      gwm->geom.xscroll + (int)gwm->geom.width-1,
              gwm->geom.yscroll + bc->end.y);
    if(gwm->bad_device)
      return 1;
    XDrawLine(gwm->display, gwm->window, gwm->gc,
              gwm->geom.xscroll + bc->end.x,
              gwm->geom.yscroll,
	      gwm->geom.xscroll + bc->end.x,
              gwm->geom.yscroll + (int)gwm->geom.height-1);
    break;
  }
  if(gwm->bad_device)
    return 1;
  XFlush(gwm->display);
  if(gwm->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * Erase a previously drawn cursor.
 *
 * Input:
 *  gwm  GWMdev *   The PGPLOT /gwm device descriptor.
 *  bc    Band *   A cursor descriptor returned by gwm_new_band().
 * Output:
 *  return int     0 - OK.
 *                 1 - Error.
 */
static int gwm_erase_cursor(GWMdev *gwm, Band *bc)
{
/*
 * Device error?
 */
  if(gwm->bad_device)
    return 1;
/*
 * Erase the cursor.
 */
  switch(bc->mode) {
  case 0: default:
    /* Nothing to be done  - X-windows system cursor */
    break;
  case 1:   /* Line cursor */
    if(gwm_cursor_line(gwm, bc->ref.x, bc->ref.y, bc->end.x, bc->end.y))
      return 1;
    break;
  case 2:   /* Rectangle cursor */
    if(gwm_cursor_line(gwm, bc->ref.x, bc->ref.y, bc->ref.x, bc->end.y) ||
       gwm_cursor_line(gwm, bc->ref.x, bc->end.y, bc->end.x, bc->end.y) ||
       gwm_cursor_line(gwm, bc->end.x, bc->end.y, bc->end.x, bc->ref.y) ||
       gwm_cursor_line(gwm, bc->end.x, bc->ref.y, bc->ref.x, bc->ref.y))
      return 1;
    break;
  case 3:  /* Two horizontal lines */
    if(gwm_cursor_line(gwm, 0, bc->end.y, (int)gwm->geom.width-1,bc->end.y) ||
       gwm_cursor_line(gwm, 0, bc->ref.y, (int)gwm->geom.width-1,bc->ref.y))
      return 1;
    break;
  case 4:  /* Two vertical lines */
    if(gwm_cursor_line(gwm, bc->end.x, 0, bc->end.x, (int)gwm->geom.height-1) ||
       gwm_cursor_line(gwm, bc->ref.x, 0, bc->ref.x, (int)gwm->geom.height-1))
      return 1;
    break;
  case 5: /* One horizontal line through the cursor */
    if(gwm_cursor_line(gwm, 0, bc->end.y, (int)gwm->geom.width-1,bc->end.y))
      return 1;
    break;
  case 6: /* One vertical line through the cursor */
    if(gwm_cursor_line(gwm, bc->end.x, 0, bc->end.x, (int)gwm->geom.height-1))
      return 1;
    break;
  case 7: /* Cross hair */
    if(gwm_cursor_line(gwm, 0, bc->end.y, (int)gwm->geom.width-1,bc->end.y) ||
       gwm_cursor_line(gwm, bc->end.x, 0, bc->end.x, (int)gwm->geom.height-1))
      return 1;
    break;
  }
  return 0;
}

/*.......................................................................
 * Restore the pixels under a given line.
 *
 * Input:
 *  gwm   GWMdev *  The PGPLOT /gwm device descriptor.
 *  xa, ya  int    The start pixel of the line.
 *  xb, yb  int    The end pixel of the line.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
static int gwm_cursor_line(GWMdev *gwm, int xa, int ya, int xb, int yb)
{
  int xlen = xb - xa;  /* X-axis displacement of line */
  int ylen = yb - ya;  /* Y-axis displacement of line */
  int xmin,xmax;       /* Min/max X-axis end points */
  int ymin,ymax;       /* Min/max Y-axis end points */
#define PIXINC 21
/*
 * Device error?
 */
  if(gwm->bad_device)
    return 1;
/*
 * Silently ignore the call if a pixmap is not available.
 */
  if(gwm->pixmap != None) {
/*
 * Get sorted versions of xa and xb.
 */
    if(xlen > 0) {
      xmin = xa;
      xmax = xb;
    } else {
      xmin = xb;
      xmax = xa;
    }
/*
 * Get sorted versions of ya and yb.
 */
    if(ylen > 0) {
      ymin = ya;
      ymax = yb;
    } else {
      ymin = yb;
      ymax = ya;
    }
/*
 * Vertical line?
 */
    if(xlen==0) {
      XCopyArea(gwm->display, gwm->pixmap, gwm->window, gwm->gc, xmin, ymin,
		(unsigned) 1, (unsigned) (ymax-ymin+1),
                gwm->geom.xscroll + xmin,
                gwm->geom.yscroll + ymin);
    }
/*
 * Horizontal line?
 */
    else if(ylen==0) {
      XCopyArea(gwm->display, gwm->pixmap, gwm->window, gwm->gc, xmin, ymin,
		(unsigned) (xmax-xmin+1), (unsigned) 1,
                gwm->geom.xscroll + xmin,
                gwm->geom.yscroll + ymin);
    }
/*
 * Diagonal line encompasing fewer x-axis lines than y-axis lines?
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
        if( y1 < 0) y1=0;
	y2 = (int)(ycent + yhi+0.5);/* Note round-up semantics */
	XCopyArea(gwm->display, gwm->pixmap, gwm->window, gwm->gc,
		  x, y1, (unsigned) (PIXINC+1), (unsigned) (y2-y1+1),
                  gwm->geom.xscroll + x,
                  gwm->geom.yscroll + y1);
      }
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
        if(x1 < 0) x1 = 0;
	x2 = (int)(xcent + xhi+0.5);/* Note round-up semantics */
	XCopyArea(gwm->display, gwm->pixmap, gwm->window, gwm->gc,
		  x1, y, (unsigned) (x2-x1+1), (unsigned) (PIXINC+1),
                  gwm->geom.xscroll + x1,
                  gwm->geom.yscroll + y);
      }
    }
  }
/*
 * Check for device errors.
 */
  if(gwm->bad_device)
    return 1;
  return 0;
}

/*.......................................................................
 * Add to the set of events to be caught.
 *
 * Input:
 *  gwm   GWMdev *  The PGPLOT /gwm device descriptor.
 *  events long    The bit mask of events to be added to those already
 *                 being selected.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
static int gwm_add_events(GWMdev *gwm, long events)
{
/*
 * Device error?
 */
  if(gwm->bad_device)
    return 1;
/*
 * Get the union of the new events with the current event mask.
 */
  gwm->event.mask |= events;
/*
 * Register the modified selection with the server.
 */
  XSync(gwm->display, False);
  if(gwm->bad_device)
    return 1;
  gwm->last_error = 0;
  XSelectInput(gwm->display, gwm->window, gwm->event.mask);
  if(gwm->bad_device)
    return 1;
/*
 * Have we successfully acquired ButtonPress events?
 */
  if(gwm->event.mask & ButtonPressMask)
    gwm->event.no_buttons = 0;
  return 0;
}

/*.......................................................................
 * Remove selected events from the set of events to be caught.
 *
 * Input:
 *  gwm   GWMdev *  The PGPLOT /gwm device descriptor.
 *  events long    The bit mask of events to be removed from the set
 *                 being selected.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
static int gwm_rem_events(GWMdev *gwm, long events)
{
/*
 * Device error?
 */
  if(gwm->bad_device)
    return 1;
/*
 * Clear the bits in our current event mask that correspond to the events
 * to be removed.
 */
  gwm->event.mask &= ~events;
/*
 * Register the modified selection with the server.
 */
  XSelectInput(gwm->display, gwm->inwin, gwm->event.mask);
  if(gwm->bad_device)
    return 1;
  XSync(gwm->display, False);
  if(gwm->bad_device)
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
static int gwm_error(Display *display, XErrorEvent *event)
{
  char errtxt[81]; /* Buffer to receive error message in */
  GWMdev *gwm;
/*
 * Find the device that is the source of the error.
 */
  for(gwm=device_list; gwm!=NULL && gwm->display!=display; gwm = gwm->next);
/*
 * If a device was located, check if the error implies that server resources
 * have become unusable for that device.
 */
  if(gwm && !gwm->bad_device) {
    gwm->last_error = event->error_code;
    switch(event->error_code) {
    case BadAtom: case BadColor: case BadCursor: case BadDrawable:
    case BadGC: case BadIDChoice: case BadPixmap: case BadWindow:
/*
 * Get a message describing the error.
 */
      XGetErrorText(display, (int)event->error_code,errtxt,(int)sizeof(errtxt));
      sprintf(errbuff, "%s: XErrorEvent: %s\n", GWM_IDENT, errtxt);
      gwm_errmess(errbuff);
/*
 * Report the operation that caused it. These opcode numbers are listed in
 * <X11/Xproto.h>.
 */
      sprintf(errbuff, "%s: Major opcode of failed request: %d\n", GWM_IDENT,
	      (int) event->request_code);
      gwm_errmess(errbuff);
/*
 * Report the loss of the window and mark the device as unusable.
 */
      gwm_bad_device(gwm);
      break;
    }
  }
  return 0;
}

/*.......................................................................
 * Front end to XNextEvent() to get the next event from the X server,
 * while checking for DestroyNotify events on the PGPLOT window.
 *
 * Input:
 *  gwm     GWMdev *  The PGPLOT /gwm device descriptor.
 * Input/Output:
 *  event  XEvent *  The event structure for the returned event.
 * Output:
 *  return    int    0 - OK.
 *                   1 - The PGPLOT window has been destroyed.
 */
static int gwm_next_event(GWMdev *gwm, XEvent *event)
{
/*
 * Check that we still have a window.
 */
  if(gwm->bad_device)
    return 1;
/*
 * Wait for the next event.
 */
  XNextEvent(gwm->display, event);
  switch(event->type) {
  case DestroyNotify:
    if(event->xdestroywindow.window == gwm->window)
      return gwm_bad_device(gwm);
  }
  return 0;
}

/*.......................................................................
 * Front end to XCheckWindowEvent() to check and return for the next event
 * that matches the given event_mask, without blocking if no matching event
 * is there and while also checking for DestroyNotify events on said window.
 *
 * Input:
 *  gwm       GWMdev *  The PGPLOT /gwm device descriptor.
 *  window   Window    The window on which to watch for events.
 *  event_mask long    The bit mask of event types wanted.
 * Input/Output:
 *  event    XEvent *  The event structure for the returned event.
 * Output:
 *  return      int    0 - No matching event.
 *                     1 - Got an event that matches the mask.
 */
static int gwm_check_window_event(GWMdev *gwm, Window window, long event_mask,
				 XEvent *event)
{
  int want_structure = 0;  /* True if the caller selects StructureNotifyMask */
/*
 * Check that we still have a window.
 */
  if(gwm->bad_device)
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
  while(XCheckWindowEvent(gwm->display, window, event_mask, event)==True) {
    switch(event->type) {
    case DestroyNotify:
      if(window == gwm->window) {  /* Have we lost the plot window? */
	gwm_bad_device(gwm);
	return want_structure;
      } else if(want_structure) {
	return 1;
      }
      break;
    case CirculateNotify:
    case ConfigureNotify:
      if(want_structure)  /* Ignore unselected StructureNotifyMask events */
	return 1;
      break;
    default:
      return 1;  /* One of the requested events was found */
    }
  }
  return 0;
}

/*.......................................................................
 * Remove a given PGPLOT /gwm device descriptor from the list of open
 * devices.
 *
 * Input:
 *  gwm      GWMdev *  The PGPLOT /gwm device descriptor to be removed.
 * Output:
 *  return   GWMdev *  The removed descriptor.
 */
static GWMdev *gwm_remove_device(GWMdev *gwm)
{
  GWMdev *prev;   /* Pointer to previous device in list */
  GWMdev *next;   /* Pointer to next device in list */
/*
 * Find the position of the device in the device list.
 */
  prev = NULL;
  next = device_list;
  while(next && next!=gwm) {
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
  }
/*
 * The descriptor is no longer in a list.
 */
  gwm->next = NULL;
  return gwm;
}

/*.......................................................................
 * Select a given device by its PGPLOT window number: gwm->number.
 *
 * Input:
 *  number      int   The device number to search for.
 * Output:
 *  return   GWMdev * The descriptor of the located device, or NULL
 *                    on error.
 */
static GWMdev *gwm_select_device(int number)
{
/*
 * Search for the cited device.
 */
  GWMdev *gwm = device_list;
  while(gwm && gwm->number != number)
    gwm = gwm->next;
  if(gwm==NULL || gwm->number!=number) {
    sprintf(errbuff, "%s: No such device (%d).\n", GWM_IDENT, number);
    gwm_errmess(errbuff);
    return NULL;
  }
  return gwm;
}

/*.......................................................................
 * After a fatal error has occured, this function should be called to
 * mark the specified device as unusable. It emits an error message
 * and sets gwm->bad_device=1.
 *
 * Input:
 *  gwm     GWMdev *  The descriptor of the device on which the error
 *                   occurred.
 * Output:
 *  gwm->bad_device   This flag is set to 1.
 *  return    int    Allways 1 (intended as a boolean to say that the
 *                   device is unusable). This can be used as the return
 *                   value for functions that use 1 to denote an error
 *                   return. eg.
 *                     if(error_occurred)
 *                       return gwm_bad_device(gwm);
 */
static int gwm_bad_device(GWMdev *gwm)
{
/*
 * Only report an error if this is the first time that this function
 * has been called on this device.
 */
  if(gwm && !gwm->bad_device) {
    sprintf(errbuff, "%s: Lost PGPLOT window %d.\n", GWM_IDENT, gwm->number);
    gwm_errmess(errbuff);
    gwm->bad_device = 1;
  }
  return 1;
}

/*.......................................................................
 * If the cursor is within the plot window, warp it to a given position.
 *
 * Input:
 *  gwm   GWMdev * The PGPLOT /gwm device descriptor.
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
static int gwm_locate_cursor(GWMdev *gwm, XPoint *pos, int warp, XPoint *loc)
{
  XWindowAttributes attr; /* Current window attributes */
  Window p_child;         /* The child of /gwm (None) containing the pointer */
  int p_win_x, p_win_y;   /* The pointer coordinates in gwm->window */
  int p_root_x, p_root_y; /* The pointer coordinates in the root window */
  Window p_root_win;      /* The root window containing the cursor */
  unsigned int p_mask;    /* Bit mask of button states etc.. */
  int inwindow=0;         /* True if the cursor is in the window */
/*
 * Device error?
 */
  if(gwm->bad_device)
    return 0;
/*
 * Query the current state of the window.
 */
  XSync(gwm->display, False);
  if(gwm->bad_device)
    return 0;
  XGetWindowAttributes(gwm->display, gwm->inwin, &attr);
  if(gwm->bad_device)
    return 0;
/*
 * Determine the current position of the pointer.
 */
  XQueryPointer(gwm->display, gwm->inwin, &p_root_win, &p_child,
		&p_root_x, &p_root_y, &p_win_x, &p_win_y, &p_mask);
  if(gwm->bad_device)
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
      XWarpPointer(gwm->display, None, gwm->inwin, 0, 0, 0, 0, pos->x, pos->y);
      if(gwm->bad_device)
	return 0;
      loc->x = pos->x;
      loc->y = pos->y;
/*
 * Return the current position of the cursor without warping.
 */
    } else {
      loc->x = p_win_x;
      loc->y = p_win_y;
    }
  }
  return inwindow;
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
static XVisualInfo *gwm_visual_info(Display *display, int screen, Visual *visual)
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
    sprintf(errbuff,
      "%s: Error getting visual information for visual ID 0x%lx, screen %d.\n",
      GWM_IDENT, (unsigned long)template.visualid, screen);
    gwm_errmess(errbuff);
    vi = NULL;
  }
  return vi;
}

/*.......................................................................
 * Allocate the contents of gwm->image. This contains buffers used to
 * construct and dispatch line-of-pixel images to the display.
 *
 * Input:
 *  gwm   GWMdev * The PGPLOT /gwm device descriptor.
 *  npix    int   The length of the buffer in pixels.
 * Output:
 *  return  int   0 - OK.
 *                1 - Error.
 */
static int gwm_get_image(GWMdev *gwm, int npix)
{
  unsigned nbyte;  /* The number of bytes in the buffer */
  XPixmapFormatValues *pixmaps;
  int npixmap;
  int i, bpp;
/*
 * Determine the required size of the buffer. This is determined by
 * the size of a pixel (the depth of the window), the number of
 * pixels required, and the max depth that gwmdriv allows (32-bit).
 */
  gwm->image.npix = npix;
  pixmaps = XListPixmapFormats(gwm->display, &npixmap); 
  bpp = 32;
  for(i=0; i<npixmap; i++) {
     if(pixmaps[i].depth == gwm->color.vi->depth) {
        bpp = pixmaps[i].bits_per_pixel;
        break;
        }
  }
  XFree(pixmaps);
  nbyte = bpp*npix/8;
  gwm->image.buff = (unsigned char *) malloc(nbyte * sizeof(char));
  if(!gwm->image.buff)  {
    sprintf(errbuff, "%s: Failed to allocate image buffer.\n", GWM_IDENT);
    gwm_errmess(errbuff);
    return 1;
  }
/*
 * Create an X image container for use in transfering pixels to and from
 * gwm->image.buff[].
 */
  gwm->image.xi = XCreateImage(gwm->display, gwm->color.vi->visual,
			      (unsigned)gwm->color.vi->depth, ZPixmap, 0,
			      (char *)gwm->image.buff, (unsigned)npix, 1, 32, 0);
  if(gwm->image.xi==NULL) {
    sprintf(errbuff, "%s: Failed to allocate XImage container.\n", GWM_IDENT);
    gwm_errmess(errbuff);
    return 1;
  }
  return 0;
}

/*.......................................................................
 * Limit pixmap coordinates to lie within the pixmap area.
 *
 * Input:
 *  gwm     GWMdev *  The PGPLOT window context.
 * Input/Output:
 *  coord  XPoint *  The coordinates to be modified.
 */
static void gwm_limit_pcoords(GWMdev *gwm, XPoint *coord)
{
  if(gwm->pixmap != None) {
    if(coord->x >= gwm->geom.width)
      coord->x = gwm->geom.width - 1;
    if(coord->y >= gwm->geom.height)
      coord->y = gwm->geom.height - 1;
    if(coord->x < 0)
      coord->x = 0;
    if(coord->y < 0)
      coord->y = 0;
  }
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
static int gwm_nint(float f)
{
  return (int) (f >= 0.0 ? (f + 0.5) : (f - 0.5));
}

/*.......................................................................
 * Scroll a rectanglular area vertically and/or horizontally.
 *
 * Input:
 *  gwm    GWMdev *  The PGPLOT window context.
 *  rbuf     float *  The array of float arguments sent by the PGPLOT
 *                    GREXEC() subroutine.
 */
static void gwm_scroll_rect(GWMdev *gwm, float *rbuf)
{
  if(!gwm->bad_device && gwm->pixmap != None) {
    XPoint blc, trc;     /* The bottom left and top right rectangle corners */
    XPoint blc_orig, trc_orig; /* The vertices of the rectangle to be copied */
    XPoint blc_dest, trc_dest; /* The vertices of the destination of the copy */
    int dx, dy;                /* The amounts to scroll right and down */
    unsigned long fg;          /* The foreground color to be reinstated */
/*
 * Convert the rectangle vertices from PGPLOT coordinates to X coordinates.
 */
    gwm_xy_to_XPoint(gwm, &rbuf[0], &blc);
    gwm_xy_to_XPoint(gwm, &rbuf[2], &trc);
/*
 * Get the scroll offsets in X coordinates.
 */
    dx = gwm_nint(rbuf[4]);
    dy = gwm_nint(-rbuf[5]);
/*
 * Selected parts of the pixmap will need to be erased by drawing an
 * opaque rectangle over them in the background color. Set the foreground
 * color to equal the background. Keep a record of the previous foreground
 * color, so that it can be re-instated.
 */
    fg = gwm->gcv.foreground;
    XSetForeground(gwm->display, gwm->gc, gwm->color.pixel[0]);
/*
 * If either scroll extent exceeds the length of the associated
 * axis, then fill the area with the background color.
 */
    if(abs(dx) > trc.x - blc.x || abs(dy) > blc.y - trc.y) {
      XFillRectangle(gwm->display, gwm->pixmap, gwm->gc, blc.x, trc.y,
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
      }
      if(dy > 0) {
	blc_orig.y = blc.y - dy;
	trc_dest.y = trc.y + dy;
      } else if(dy < 0) {
	trc_orig.y = trc.y - dy;
	blc_dest.y = blc.y + dy;
      }
/*
 * Constrain the coordinates to lie within the pixmap.
 */
      gwm_limit_pcoords(gwm, &blc_orig);
      gwm_limit_pcoords(gwm, &blc_dest);
      gwm_limit_pcoords(gwm, &trc_orig);
      gwm_limit_pcoords(gwm, &trc_dest);
/*
 * Scroll the rectangle to its shifted location.
 */
      XCopyArea(gwm->display, gwm->pixmap, gwm->pixmap, gwm->gc,
		blc_orig.x, trc_orig.y,
		trc_orig.x - blc_orig.x + 1,
		blc_orig.y - trc_orig.y + 1,
		blc_dest.x, trc_dest.y);
/*
 * Clear the vacated area to the left or right of the copied area.
 */
      if(dx > 0) {
	XFillRectangle(gwm->display, gwm->pixmap, gwm->gc,
		       blc.x, trc.y,
		       (unsigned) dx,
		       (unsigned) (blc.y - trc.y + 1));
      } else if(dx < 0) {
	XFillRectangle(gwm->display, gwm->pixmap, gwm->gc,
		       trc_dest.x, trc.y,
		       (unsigned) (-dx),
		       (unsigned) (blc.y - trc.y + 1));
      }
/*
 * Clear the vacated area above or below the copied area.
 */
      if(dy > 0) {
	XFillRectangle(gwm->display, gwm->pixmap, gwm->gc,
		       blc.x, trc.y,
		       (unsigned) (trc.x - blc.x + 1),
		       (unsigned) dy);
      } else if(dy < 0) {
	XFillRectangle(gwm->display, gwm->pixmap, gwm->gc,
		       blc.x, blc_dest.y,
		       (unsigned) (trc.x - blc.x + 1),
		       (unsigned) (-dy));
      }
    }
/*
 * Record the extent of the modified part of the pixmap.
 */
    gwm_mark_modified(gwm, blc.x, blc.y, 1);
    gwm_mark_modified(gwm, trc.x, trc.y, 1);
/*
 * Re-instate the original foreground color.
 */
    XSetForeground(gwm->display, gwm->gc, fg);
  }
  return;
}

/*.......................................................................
 * Output a formatted /gwm driver error message
 *
 * Input:
 *  char *buff   The error message.
 * Output:
 *  None.
 */
static void gwm_errmess(char *buff)
{
   DECLARE_CHARACTER(text,80);
/*
 *  The text in "buff" was originally output via a printf and should
 *  contain a trailing '/n'.  We wish to omit this for output using
 *  GRWARN.
 */
   if( buff[strlen(buff) - 1] == '\n' )
      buff[strlen(buff)-1] = '\0';
   cnf_exprt(buff, text, text_length);
   F77_CALL(grwarn) (CHARACTER_ARG(text) TRAIL_ARG(text) );
}   
