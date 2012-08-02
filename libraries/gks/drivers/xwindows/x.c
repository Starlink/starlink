/*
 *  Support routines for X-windows driver for GKS.
 *
 *	D L Terrett	Starlink    2-JUL-1990
 *
 *  Each X workstation is allocated a "window index" that is used to
 *  access the appropriate element of the various static data structures
 *  that used by the support routines. Each GKS workstation is allocated a
 *  workstation index (in GKS) and this is used as an index into an array
 *  containing window indexes to find the window index for the specified
 *  workstation.
 *
 *  Overlay workstations are assummed to have only one bitplane in
 *  several places.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#if HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <stdio.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#if defined(VMS)
#  include "gwm_dir:gwm.h"
#  include "gwm_dir:gwm_err.h"
#else
#  include "gwm.h"
#  include "gwm_err.h"
#endif

/* Used to check for VMS or Ultrix here. Now use configure */
#if HAVE_X11_DECWMHINTS_H
#  include <X11/DECWmHints.h>
#endif

#define MAXWIN 4                        /* max number of open X workstations */
#define GNO 0
#define GYES 1
#if !defined(NULL)
#define NULL 0
#endif

#define GKS_STORE_ERR	300
#define GKS_IO_ERR	304

#if !defined(VMS)
#define gk0xop gk0xop_
#define gk0xfa gk0xfa_
#define gk0xpl gk0xpl_
#define gk0xdi gk0xdi_
#define gk0xls gk0xls_
#define gk0xsc gk0xsc_
#define gk0xcl gk0xcl_
#define gk0xfl gk0xfl_
#define gk0xup gk0xup_
#define gk0xcr gk0xcr_
#define gk0xgd gk0xgd_
#define gk0xrl gk0xrl_
#define gk0xsi gk0xsi_
#define gk0xws gk0xws_
#define gk0xcf gk0xcf_
#define gk0xcw gk0xcw_
#endif

static int driver_init = 0;		/* Flag to say whether driver has
					   been initialised		*/
static Display *display = NULL;		/* display id */
static int indx[MAXWIN];		/* maps GKS workstation index   */
					/* to window index		*/

/*
 *  This structure hold all the information about each window; currently
 *  only one display is supported because there is no way of passing a
 *  display name to the driver when a workstation is opened, however
 *  this structure allows for the possibility of multiple displays in
 *  the future
 */
static struct
{
    int wktype;				/* GKS workstation type		*/
    int conid;				/* Connection id		*/
    Display *display;			/* Display id			*/
    Screen *screen;			/* Screen number		*/
    Window win;				/* Window id			*/
    Pixmap pix;				/* Backing pixmap id		*/
    GC gc;				/* Graphics context		*/
    unsigned long mask;			/* Plane mask			*/
    unsigned long *pixels;		/* Pointer to allocated pixels	*/
    unsigned long npix;			/* Number of allocated pixels	*/
    unsigned int width;			/* Pixmap width			*/
    unsigned int height;		/* Pixmap height		*/
    float xmetre, ymetre;		/* Window size in metres	*/
    unsigned int depth;			/* Pixmap depth			*/
    Colormap colormap;			/* Colour map			*/
    int xscroll, yscroll;		/* Scroll offsets		*/
    Font font;				/* Font id			*/
    int font_h, font_w, font_d;		/* Font metrics			*/
    Pixmap cursor;			/* Cursor pixmap id		*/
    Bool ctwrite;			/* Colour table writeable 	*/
    Bool mono;				/* monochrome display 		*/
    Bool new;				/* Window created by us		*/
    Bool overlay;			/* Overlay workstation 		*/
    XVisualInfo visualInfo;		/* Visual of window		*/
} wdt[MAXWIN];

static char cursor_bits[] =		    /* cursor bitmap */
{
   0x00, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
   0x80, 0x00, 0xfe, 0x3f, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
   0x80, 0x00, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00};

static char mask_bits[] =		    /* cursor mask bitmap */
{
   0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01,
   0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01,
   0xc0, 0x01, 0xc0, 0x01, 0xc0, 0x01, 0x00, 0x00};


int gk0xin(int wktype, int *winind)
/*
   Initialises the X display for the specified workstation type and "attaches"
    to (or if necessary creates) the window and fills in the wdt structure
    with all the information that relates to the window.
*/
{
    int i, j, nfonts, nitems, argc;
    char name[32], *pname, *argv[4], **fontlist;
    int x, y;
    unsigned int border;
    Window root;
    XWindowAttributes winatt;
    XGCValues xgcv;
    XColor fore_color, back_color;
    XFontStruct *fontinfo;
    XVisualInfo vinfo_template, *vinfo;
    Pixmap cursor_pixmap, cursor_mask;

/*
 *  Initialize the driver if required. This involves setting the wktype
 *  element of the wdt structure to zero and setting the flag to show that
 *  the initialization has been done and opening the display.
 */
    if (!driver_init)
    {
        display = XOpenDisplay( NULL );
#ifdef SYNC
	XSynchronize( display, 1 );	    /* only when debugging  */
#endif
        if (!display) return GKS_IO_ERR;

	for ( i = 0; i < MAXWIN; i++ )
	{
	    wdt[i].wktype = 0;
            wdt[i].display = 0;
        }
	driver_init = 1;
    }
    else
    {

/*
 *  	Look to see if this workstation type has already been initialized. If
 *  	it has then just return the index for the workstation
 */
    	for ( i = 0; i < MAXWIN; i++ )
	{
	    if ( wdt[i].wktype == wktype )
	    {
    		*winind = i;
		return 0;
	    }
	}
    }

/*
 *  Find a free slot in the wdt array for this workstation type
 */
    for ( i = 0; i < MAXWIN; i++) if ( wdt[i].wktype == 0 ) break;
    if ( i == MAXWIN) return GKS_IO_ERR;

/*
 *  Set the conection id to -1 to show that there isn't a workstation open
 *  on this workstation type yet.
 */
    wdt[i].conid = -1;

/*
 *  Determine whether this is an overlay workstation or not from the
 *  value of the workstation type
 */
    if ( wktype >= 3805 )
	wdt[i].overlay = True;
    else
	wdt[i].overlay = False;

/*
 *  Convert the workstation id to a window name
 */
    sprintf( name, "GKS_%04d", wktype);
    pname = name;
    while( pname = getenv( name ) ) strcpy( name, pname);

/*
 *  Look for an existing window with this name. If it doesn't exist then
 *  create it.
 */
    if ( GWM_FindWindow( display, name, &wdt[i].win) != GWM_SUCCESS)
    {
	argc = 0;
	argv[argc++] = "GKS";
	argv[argc++] = name;
	if (wdt[i].overlay)
	{
	    argv[argc++] = "-overlay";
	}
	argv[argc] = NULL;

        if ( GWM_CreateWindow( argc, argv, &display, name) != GWM_SUCCESS)
	    return GKS_IO_ERR;
    	if ( GWM_FindWindow( display, name, &wdt[i].win) != GWM_SUCCESS)
	    return GKS_IO_ERR;
	wdt[i].new = True;
    }
    else
    {
	wdt[i].new = False;
    }

/*
 *  Fill in the wdt structure with information about the window. If the
 *  window id matches an existing workstation then copy the information
 *  from there.
 */
    for ( j = 0; j < MAXWIN; j++ )
    {
	if ( wdt[j].wktype != 0 && wdt[j].display == display &&
	     wdt[j].win == wdt[i].win && i != j )
        {
	    wdt[i].pix = wdt[j].pix;
	    wdt[i].pixels = wdt[j].pixels;
	    wdt[i].npix = wdt[j].npix;
	    wdt[i].width = wdt[j].width;
	    wdt[i].height = wdt[j].height;
	    wdt[i].depth = wdt[j].depth;
	    wdt[i].mask = wdt[j].mask;
	    wdt[i].visualInfo = wdt[j].visualInfo;
	    wdt[i].colormap = wdt[j].colormap;
	    wdt[i].screen = wdt[j].screen;
	    wdt[i].xmetre = wdt[j].xmetre;
	    wdt[i].ymetre = wdt[j].ymetre;
            break;
	}
    }

/*
 *  No matching window so the information has to come from GWM and X
 *  queries.
 */
    if ( j == MAXWIN )
    {
	GWM_GetPixmap( display, wdt[i].win, &wdt[i].pix);
	GWM_GetColTable( display, wdt[i].win, &wdt[i].pixels, &wdt[i].npix);

	XGetGeometry( display, wdt[i].pix, &root, &x, &y, &wdt[i].width,
	    &wdt[i].height, &border, &wdt[i].depth);

	if( GWM_GetOvMask( display, wdt[i].win, &wdt[i].mask) != GWM_SUCCESS)
	     wdt[i].mask = 0xffffffff;

/*
 *  If this is an overlay workstation but the window hasn't got an
 *  overlay plane then exit with an error status
 */
	if (wdt[i].overlay && (wdt[i].mask == 0xffffffff)) return GKS_IO_ERR;

	XGetWindowAttributes( display, wdt[i].win, &winatt);
        vinfo_template.visualid = XVisualIDFromVisual( winatt.visual );
	vinfo = XGetVisualInfo( display, VisualIDMask, &vinfo_template,
                &nitems);
	wdt[i].visualInfo = *vinfo;
        XFree( vinfo );
	wdt[i].colormap = winatt.colormap;
	wdt[i].screen = winatt.screen;

/*
 *  find the display size and convert the pix map size to metres
 */
	wdt[i].xmetre = (float)wdt[i].width *
		(float)WidthMMOfScreen( wdt[i].screen)
		/ (float)WidthOfScreen( wdt[i].screen ) / 1000.0;

	wdt[i].ymetre = (float)wdt[i].height *
		(float)HeightMMOfScreen( wdt[i].screen)
		/ (float)HeightOfScreen( wdt[i].screen ) / 1000.0;
    }

/*
 *  The scroll offsets are different for the overlay and normal windows
 */
    if ( wdt[i].overlay )
	GWM_GetOvScroll( display, wdt[i].win, &wdt[i].xscroll,
	    &wdt[i].yscroll);
    else
	GWM_GetScroll( display, wdt[i].win, &wdt[i].xscroll,
	    &wdt[i].yscroll);

/*
 *  The remaining items are shared by windows on the same screen so can be
 *  copied from the wdt structure of any other window in the same screen.
 */
    for ( j = 0; j < MAXWIN; j++ )
    {
	if ( wdt[j].display == display &&
	     wdt[j].screen == wdt[i].screen && i != j )
        {
	    wdt[i].font = wdt[j].font;
	    wdt[i].font_h = wdt[j].font_h;
	    wdt[i].font_w = wdt[j].font_w;
	    wdt[i].font_d = wdt[j].font_d;
	    wdt[i].cursor = wdt[j].cursor;
            break;
	}
    }

/*
 *  Otherwise the font and cursor pixmap have to be created
 */
    if ( j == MAXWIN )
    {

/*
 * 	Colour table properties
 */
        switch (wdt[i].visualInfo.class)
	{
	    case StaticGray:  wdt[i].mono = True;  wdt[i].ctwrite = False;
		break;
	    case StaticColor: wdt[i].mono = False; wdt[i].ctwrite = False;
		break;
	    case TrueColor:   wdt[i].mono = False; wdt[i].ctwrite = False;
		break;
	    case GrayScale:   wdt[i].mono = True;  wdt[i].ctwrite = True;
		break;
	    case PseudoColor: wdt[i].mono = False; wdt[i].ctwrite = True;
		break;
	    case DirectColor: wdt[i].mono = False; wdt[i].ctwrite = True;
		break;
	 }

/*
 *  	Create the cursor using the same bit map for the cursor shape and mask
 *  	so that the cursor background is transparent
 */
	back_color.pixel = WhitePixelOfScreen(wdt[i].screen);
	XQueryColor(display, DefaultColormapOfScreen(wdt[i].screen),
            &back_color);
	fore_color.pixel = BlackPixelOfScreen(wdt[i].screen);
	XQueryColor(display, XDefaultColormapOfScreen(wdt[i].screen),
            &fore_color);

	cursor_pixmap = XCreateBitmapFromData( display, root,
	    cursor_bits, 16, 16);
	cursor_mask = XCreateBitmapFromData( display, root,
	    mask_bits, 16, 16);
	wdt[i].cursor = XCreatePixmapCursor( display, cursor_pixmap,
	    cursor_mask, &fore_color, &back_color, 7, 8);
	XFreePixmap( display, cursor_pixmap);

/*
 *  	Register a font for prompting for input and find out the character
 *  	size. Use "variable" if available, otherwise use "fixed".
 */
	fontlist = XListFonts( display, "variable", 1, &nfonts);
        if (nfonts==1)
	    wdt[i].font = XLoadFont( display, "variable");
	else
	    wdt[i].font = XLoadFont( display, "fixed");
        XFreeFontNames (fontlist);

	fontinfo = XQueryFont( display, wdt[i].font);
	wdt[i].font_h = (int)fontinfo->max_bounds.ascent;
	wdt[i].font_d = (int)fontinfo->max_bounds.descent;
	wdt[i].font_w = (int)fontinfo->max_bounds.width;
        XFreeFontInfo( NULL, fontinfo, 1);
    }

/*
 *  Create a graphics context for this window (this has to be done after
 *  the font has been registered)
 */
    if (wdt[i].overlay)
    {
	xgcv.plane_mask = ~wdt[i].mask;
	xgcv.foreground = 0;
	xgcv.background = 0;
    }
    else
    {
	xgcv.plane_mask = wdt[i].mask;
	xgcv.foreground = (wdt[i].pixels)[0];
	xgcv.background = (wdt[i].pixels)[0];
    }
    xgcv.graphics_exposures = 0;
    xgcv.fill_style = FillSolid;
    xgcv.fill_rule = EvenOddRule;
    xgcv.font = wdt[i].font;
    wdt[i].gc = XCreateGC( display, wdt[i].pix,
		GCForeground | GCBackground | GCGraphicsExposures |
		GCFillStyle | GCFillRule | GCFont | GCPlaneMask,
		&xgcv );

/*
 *  Set the workstation type and display in the wdt element and return the
 *  index into the wdt array for this workstation type
 */
    wdt[i].wktype = wktype;
    wdt[i].display  = display;
    *winind = i;
    return 0;
}

int gk0xop(int *kwkix, int *wktyp, int *conid, int *xsize, int *ysize,
	   float *xsizm, float *ysizm, int *colours, int *dyn,
	   int *colour, int *new)
/*
    Associates a window with a workstation index and returns various
    information about the window and display

    Arguments (given):	kwkix: workstation index
			wktyp: workstation type
			conid: connection id
               (returned):
			xsize, ysize; window size in pixels
			xsizm, ysizm; window size in metres
			colours; number of colours allocated
			dyn; whether colour table is dynamic
			colour; whether colour or monochrome
			new; whether window was created
*/
{
    int kerror;

/*
 *  Initialize the display for this workstation type
 */
    kerror = gk0xin(*wktyp, &(indx[*kwkix-1]));
    if (kerror != 0 ) return kerror;

/*
 *  Check that the window for this workstation type hasn't already been
 *  opened with a different connection id.
 */
    if (wdt[indx[*kwkix-1]].conid != -1 )
    {
	if (wdt[indx[*kwkix-1]].conid != *conid) return GKS_IO_ERR;
    }

/*
 *  record the connection id in the wdt structure
 */
    wdt[indx[*kwkix-1]].conid = *conid;

/*
 *  return information about the window and display
 */
    *xsize = wdt[indx[*kwkix-1]].width;
    *ysize = wdt[indx[*kwkix-1]].height;
    *xsizm = wdt[indx[*kwkix-1]].xmetre;
    *ysizm = wdt[indx[*kwkix-1]].ymetre;
    if (wdt[indx[*kwkix-1]].overlay)
	*colours = 2;
    else
	*colours = wdt[indx[*kwkix-1]].npix;
    *dyn = wdt[indx[*kwkix-1]].ctwrite ? GYES : GNO;
    *colour = wdt[indx[*kwkix-1]].mono ? GNO : GYES;
    *new = wdt[indx[*kwkix-1]].new ? GYES : GNO;

/*
 *  mark this window as not new any more in case the workstation is closed
 *  and then reopened sometime
 */
    wdt[indx[*kwkix-1]].new = False;

    return 0;
}


void gk0xcw(int *kwkix)
/*
    Close a workstation

    Arguments: (given): kwkix; workstation index
*/
{
    int j;

/*
 *  If this isn't an overlay workstation and the colour table is static,
 *  write the colour table back to the window.
 */
    if((!wdt[indx[*kwkix-1]].overlay) && (!wdt[indx[*kwkix-1]].ctwrite)) {
	GWM_SetColTable( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].win,
	    wdt[indx[*kwkix-1]].pixels, wdt[indx[*kwkix-1]].npix);
    }

/*
 *  If there are no other workstations using this colour table then free
 *  the space used for the colour table and the other X resources allocated.
 */
    for ( j = 0; j < MAXWIN; j++ )
	if ( wdt[j].pixels == wdt[indx[*kwkix-1]].pixels &&
	    wdt[j].wktype != 0 && indx[*kwkix-1] != j ) break;

    if ( j == MAXWIN)
    {
	free((char*)wdt[indx[*kwkix-1]].pixels);
        wdt[indx[*kwkix-1]].pixels = NULL;

        XFreeGC( wdt[indx[*kwkix-1]].display,  wdt[indx[*kwkix-1]].gc);
        XUnloadFont( wdt[indx[*kwkix-1]].display,  wdt[indx[*kwkix-1]].font);
	XFreeCursor( wdt[indx[*kwkix-1]].display,  wdt[indx[*kwkix-1]].cursor);
    }

/*
 * Flush the X buffer incase the window is destroyed by some outside agency.
 */
    XFlush( wdt[indx[*kwkix-1]].display );

/*
 *  Mark the entry in the workstation table as free.
 */
    wdt[indx[*kwkix-1]].wktype = 0;
}

int gk0xfa(int *kwkix, int *n, float (*x)[], float (*y)[])
/*
    Plots a filled area

    Arguments: (given): kwkix; workstation index
			n; number of points
			x; x coordinates of points (DC)
			y; y coordinates of points (DC)
*/
{
    XPoint *point;
    int i;

/*
 *  Allocate space for the line segement
 */
    point = (XPoint*)malloc( sizeof(XPoint) * ((*n)+1) );
    if ( !point ) return GKS_STORE_ERR;

    for ( i = 0; i < *n; i++ )
    {
	point[i].x = (short) ((*x)[i] + 0.5);
	point[i].y = (short) (wdt[indx[*kwkix-1]].height - 1
	    - (int)((*y)[i] + 0.5));
    }
    point[*n].x = (short) ((*x)[0] + 0.5);
    point[*n].y = (short) (wdt[indx[*kwkix-1]].height - 1
	- (int)((*y)[0] + 0.5));

/*
 *  Plot in backing pixmap
 */
    XFillPolygon( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].pix,
	wdt[indx[*kwkix-1]].gc, point,  (*n)+1, Complex, CoordModeOrigin);

/*
 *  Add scroll offsets to coordinates and plot into the window
 */
    for ( i = 0; i <= *n; i++)
    {
	point[i].x += wdt[indx[*kwkix-1]].xscroll;
	point[i].y += wdt[indx[*kwkix-1]].yscroll;
    }
    XFillPolygon( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].win,
	wdt[indx[*kwkix-1]].gc, point,  (*n)+1, Complex, CoordModeOrigin);

    free( point );
    return 0;
}


int gk0xpl(int *kwkix, int *n, float (*x)[], float (*y)[])
/*
    Plots a polyline

    Arguments: (given): kwkix; workstation index
			n; number of points
			x; x coordinates of points (DC)
			y; y coordinates of points (DC)
*/
{
    XPoint *point;
    int i, j;

/*
 *  Allocate space for the line segement
 */
    point = (XPoint*)malloc( sizeof(XPoint) * (*n) );
    if ( !point ) return GKS_STORE_ERR;

/*
 *  Convert the points to X points eliminating redundant points
 */
    point[0].x = (short) ((*x)[0] + 0.5);
    point[0].y = (short) (wdt[indx[*kwkix-1]].height - 1
	    - (int)((*y)[0] + 0.5));
    for ( i = j = 1; i < *n; i++ )
    {
	point[j].x = (short) ((*x)[i] + 0.5);
	point[j].y = (short) (wdt[indx[*kwkix-1]].height - 1
	    - (int)((*y)[i] + 0.5));
        if ( point[j].x != point[j-1].x || point[j].y != point[j-1].y ) j++;
    }

/*
 *  Plot in backing pixmap
 */
    if (j > 1)
    	XDrawLines( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].pix,
	    wdt[indx[*kwkix-1]].gc, point,  j, CoordModeOrigin);
    else
	XDrawPoint( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].pix,
            wdt[indx[*kwkix-1]].gc, point[0].x, point[0].y);

/*
 *  Add scroll offsets to coordinates and plot into the window
 */
    for ( i = 0; i < j; i++)
    {
	point[i].x += wdt[indx[*kwkix-1]].xscroll;
	point[i].y += wdt[indx[*kwkix-1]].yscroll;
    }
    if (j > 1)
    	XDrawLines( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].win,
	    wdt[indx[*kwkix-1]].gc, point,  j, CoordModeOrigin);
    else
	XDrawPoint( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].win,
            wdt[indx[*kwkix-1]].gc, point[0].x, point[0].y);

    free( point );
    return 0;
}


int gk0xdi( int *kwkix, float *x, float *y, int *nx, int *ny, int *nxdim,
	     int (*array)[] )
/*
    Draws a cell array

    Arguments: (given): kwkix; workstation index
			x, y; location of top left corner
			nx, ny; dimensions of cell array
			nxdim; first dimension of cell array
			array; cell array

    The image is only written to the backing pixmap and not to the window; we
    rely on someone else to copy the pixmap to the image because cell arrays
    are typically passed to this routine in lots of small pieces.
*/
{
    XImage *ximage;
    int x0, y0, i, j, ibase, abase, nbytes, depth, byteorder;
    unsigned long val;
    unsigned char *imageb;

/*
 *  Determine the number of bytes per image pixel
 */
    if( wdt[indx[*kwkix-1]].depth <= 8)
	nbytes = 1;
    else
    	if( wdt[indx[*kwkix-1]].depth <= 16)
	    nbytes = 2;
	else
	    nbytes = 4;

/*
 *  Create the image structure
 */
    ximage = XCreateImage( wdt[indx[*kwkix-1]].display,
		    wdt[indx[*kwkix-1]].visualInfo.visual,
		    wdt[indx[*kwkix-1]].depth,
		    ZPixmap, 0, (char*)imageb, *nx, *ny, nbytes * 8, 0);
    byteorder = ximage->byte_order;

/*
 *  Allocate space for the image
 */
    imageb = (unsigned char*)malloc( (*nx) * (*ny) * nbytes);
    ximage->data = imageb;
    if (!imageb) return GKS_STORE_ERR;

/*
 *  For the overlay workstation fill the image with 1s or 0s
 */
    if (wdt[indx[*kwkix-1]].overlay)
    {
	for (i = 0; i < *ny; i++)
	    {
	    ibase = i * (*nx);
	    abase = i * (*nxdim);
	    if (nbytes == 1)
	    {
		for ( j = 0; j < *nx; j++) imageb[ibase + j] =
		    (*array)[abase + j] ? ~0 : 0;
	    }
	    else
	    {
		if (nbytes == 2)
		{
	    	    for ( j = 0; j < *nx; j++)
                    {
                        imageb[(ibase + j) * 2] =
                        imageb[(ibase + j) * 2 + 1] =
                            (*array)[abase + j] ? ~0: 0;
                    }
		}
		else
		{
	    	    for ( j = 0; j < *nx; j++)
                    {
                        imageb[(ibase + j) * 4] =
                        imageb[(ibase + j) * 4 + 1] =
                        imageb[(ibase + j) * 4 + 2] =
                        imageb[(ibase + j) * 4 + 3] =
                            (*array)[abase + j] ? ~0: 0;
                    }
		}
	    }
	}
    }
    else
    {

/*
 *  Copy the data indexed via the pixel array
 */
	for (i = 0; i < *ny; i++)
	    {
	    ibase = i * (*nx);
	    abase = i * (*nxdim);
	    if (nbytes == 1)
	    {
		for ( j = 0; j < *nx; j++) imageb[ibase + j] =
		    (wdt[indx[*kwkix-1]].pixels)
                    [(*array)[abase + j] % wdt[indx[*kwkix-1]].npix];
	    }
	    else
	    {
		if (nbytes == 2)
		{
	    	    for ( j = 0; j < *nx; j++) {
			val = (wdt[indx[*kwkix-1]].pixels)
			    [(*array)[abase + j] % wdt[indx[*kwkix-1]].npix];
                        imageb[(ibase + j) * 2] =  ( val ) & 0xff;
                        imageb[(ibase + j) * 2 + 1] =  ( val >> 8 ) & 0xff;
                    }
		}
		else
		{
/*
 *  Ammendment RTP 9 Jan 2002:
 *       Changed order of stored bytes
 */
	    	    for ( j = 0; j < *nx; j++) {
			val = (wdt[indx[*kwkix-1]].pixels)
			    [(*array)[abase + j] % wdt[indx[*kwkix-1]].npix];
            if ( byteorder == LSBFirst ) {
                        imageb[(ibase + j) * 4] = val & 0xff;
                        imageb[(ibase + j) * 4 + 1] =  ( val >> 8 ) & 0xff;
                        imageb[(ibase + j) * 4 + 2] =  ( val >> 16 ) & 0xff;
                        imageb[(ibase + j) * 4 + 3] =  ( val >> 24 ) & 0xff;
                    } else {
                        imageb[(ibase + j) * 4 + 3] = val & 0xff;
                        imageb[(ibase + j) * 4 + 2] =  ( val >> 8 ) & 0xff;
                        imageb[(ibase + j) * 4 + 1] =  ( val >> 16 ) & 0xff;
                        imageb[(ibase + j) * 4] =  ( val >> 24 ) & 0xff;
                    }
                    }
		}
	    }
	}
    }


/*
 *  Scale the cell array coordinates
 */
    x0 = (int) (*x + 0.5);
    y0 = wdt[indx[*kwkix-1]].height - 1 - (int)(*y + 0.5);

/*
 *  Draw the image
 */
    XPutImage( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].pix,
	wdt[indx[*kwkix-1]].gc, ximage, 0, 0, x0, y0, *nx, *ny);

/*
 *  Free the image structure and the data
 */
    (void)XDestroyImage( ximage );

#ifdef VMS
/*
 *  On VMS XDestroyImage doesn't seem to free the data
 */
    free( imageb );
#endif

    return 0;
}


void gk0xls(int *kwkix, int *style, int *width, int *colour)
/*
    Sets the current line drawing style

    Arguments: (given): kwkix; workstation index
			style; dash style
			width; line width
			colour; colour index
*/
#define GLSOLI 1
#define GLDASH 2
#define GLDOT 3
#define GLDASD 4
{
    char dash_list[6];
    int n;

/*
 *  Set foreground colour
 */
    if (wdt[indx[*kwkix-1]].overlay)
    {
	XSetForeground( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].gc,
	    *colour ? ~0 : 0);
    }
    else
	XSetForeground( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].gc,
	    (wdt[indx[*kwkix-1]].pixels)[*colour] );

/*
 *  Set line style and line width
 */
    if (*style==GLSOLI)
    {
	if (*width == 1)
	    XSetLineAttributes(wdt[indx[*kwkix-1]].display,
		wdt[indx[*kwkix-1]].gc, 0, LineSolid, CapRound, JoinRound);
	else
	    XSetLineAttributes(wdt[indx[*kwkix-1]].display,
		wdt[indx[*kwkix-1]].gc, *width, LineSolid, CapRound,
		JoinRound);
    }
    else
    {
	XSetLineAttributes(wdt[indx[*kwkix-1]].display,
		wdt[indx[*kwkix-1]].gc, *width, LineOnOffDash, CapRound,
		JoinRound);
        switch (*style)
        {
            case GLDASH:
	    {
		dash_list[0] = 7 * (*width);
		dash_list[1] = dash_list[0];
		n = 2;
		break;
	    }
	    case GLDOT:
	    {
		dash_list[0] = 1 * (*width);
		dash_list[1] = 2 * (*width);
		n = 2;
		break;
	    }
	    case GLDASD:
	    {
		dash_list[0] = 7 * (*width);
		dash_list[1] = 5 * (*width);
		dash_list[2] = 1 * (*width);
		dash_list[3] = dash_list[1];
		n = 4;
		break;
	    }
	    case 5:
	    {
		dash_list[0] = 8 * (*width);
		dash_list[1] = 4 * (*width);
		dash_list[2] = 1 * (*width);
		dash_list[3] = dash_list[1];
		dash_list[4] = 1;
		dash_list[5] = 4;
		n = 6;		    /* this give a bad attrib error so style*/
		break;		    /* 5 is disabled in the driver	    */
	    }
        }
	XSetDashes( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].gc, 1,
	    dash_list, n);
    }
}


void gk0xsc(int *kwkix, int *cind, float *r, float *g, float *b)
/*
    Set colour of colour index

    Arguments: (given): kwkix; workstation index
			cind; colour index
			r,g,b; colour

    Amendments: RTP (7 January 2002) change cast to (unsigned short)
*/
{
    unsigned int i;
    XColor color;

    color.red = (unsigned short) (*r * 65535.0);
    color.green = (unsigned short) (*g * 65535.0);
    color.blue = (unsigned short) (*b * 65535.0);
    color.flags = DoRed | DoGreen | DoBlue;

    if ( wdt[indx[*kwkix-1]].ctwrite )
    {

/*
 *	colour table is dynamic so load the colour we want
 */
	if (wdt[indx[*kwkix-1]].overlay)
	{

/*
 *	The background is not settable on overlays
 */
	    if (*cind != 0)
	    {

/*
 *          For an overlay workstation we have to set every colour table
 *          entry in the table of allocated colours with the overlay plane
 *          bit set but only entry 1 can be set.
 */
	        for ( i = 0; i < wdt[indx[*kwkix-1]].npix; i++ )
	        {
		    color.pixel = (wdt[indx[*kwkix-1]].pixels)[i] |
		        ~wdt[indx[*kwkix-1]].mask;
		    XStoreColor( wdt[indx[*kwkix-1]].display,
		        wdt[indx[*kwkix-1]].colormap, &color);
	        }
	    }
	}
	else
	{

/*
 *      Normal workstation; just set the requested entry.
 */
	    color.pixel = (wdt[indx[*kwkix-1]].pixels)[*cind];
	    XStoreColor( wdt[indx[*kwkix-1]].display,
		wdt[indx[*kwkix-1]].colormap, &color);
	}
    }
    else
    {

/*
 *	colour table is static so ask X for the nearest available colour
 */
	if (!wdt[indx[*kwkix-1]].overlay)
					/* but never change an overlay */
	{
	    (void)XAllocColor( wdt[indx[*kwkix-1]].display,
	        wdt[indx[*kwkix-1]].colormap, &color );
	    (wdt[indx[*kwkix-1]].pixels)[*cind] = color.pixel;
	}
    }
}


void gk0xcl(int *kwkix)
/*
 *  Clear the window
 */
{
    XSetForeground( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].gc,
	(wdt[indx[*kwkix-1]].pixels)[0] );

/*
 *  Fill the pixmap
 */
    XFillRectangle( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].pix,
	wdt[indx[*kwkix-1]].gc, 0, 0, wdt[indx[*kwkix-1]].width,
	wdt[indx[*kwkix-1]].height );

/*
 *  Copy the pixmap to the window
 */
    XCopyArea( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].pix,
	wdt[indx[*kwkix-1]].win, wdt[indx[*kwkix-1]].gc, 0, 0,
	wdt[indx[*kwkix-1]].width, wdt[indx[*kwkix-1]].height,
	wdt[indx[*kwkix-1]].xscroll, wdt[indx[*kwkix-1]].yscroll);
}


void gk0xfl( int *kwkix)
/*
    Flush the Xlib output buffer
*/
{
    XSync( wdt[indx[*kwkix-1]].display, 0 );
}


void gk0xup( int *kwkix)
/*
    Update the window by copying the pixmap to it (this is only used after
    a series of calls to gk0xdi when plotting a cell array).
*/
{
    XCopyArea( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].pix,
	wdt[indx[*kwkix-1]].win, wdt[indx[*kwkix-1]].gc, 0, 0,
	wdt[indx[*kwkix-1]].width, wdt[indx[*kwkix-1]].height,
	wdt[indx[*kwkix-1]].xscroll, wdt[indx[*kwkix-1]].yscroll);
}


void gk0xcr( int *kwkix, int *cind, float *r, float *g, float *b)
/*
    Return the r,g,b colours of the specified entry in the windows
    colour map.

    Arguments: (given): kwkix; workstation index
			cind; colour index
	       (returned):r,g,b; colour
*/
{
    XColor color;

    if (wdt[indx[*kwkix-1]].overlay)
    {
 	    if ( *cind == 0 )
	    {
	        *r = 0; *g = 0; *b = 0;
	    }
	    else
	    {
	        color.pixel = (wdt[indx[*kwkix-1]].pixels)[1] |
		        ~wdt[indx[*kwkix-1]].mask;
	        XQueryColor( wdt[indx[*kwkix-1]].display,
		        wdt[indx[*kwkix-1]].colormap, &color );
	        *r = (float)color.red/65535.0;
	        *g = (float)color.green/65535.0;
	        *b = (float)color.blue/65535.0;
	    }
    }
    else  /* Non-Overlay */
    {
	color.pixel = (wdt[indx[*kwkix-1]].pixels)[*cind];
	XQueryColor( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].colormap,
                 &color );
	    *r = (float)color.red/65535.0;
	    *g = (float)color.green/65535.0;
	    *b = (float)color.blue/65535.0;
    }
}


int gk0xgd( int *wktyp, int *cind, float *r, float *g, float *b)
/*
    Return the r,g,b colours of the nearest available colour in the
    windows colour map, except for colour indexes 0 and 1 for which the
    background and foreground colours of the window (as set when the
    window was created are returned.

    Overlay planes have black (ie. transparent backgrounds).

    Arguments: (given): wktyp; workstation type
			 cind; colour index
	       (returned):r,g,b; colour
*/
{
    int winind, kerror, status = GWM_SUCCESS;
    float min, max;
    XColor screen_def;
    char *pcolor_name, color_name[14];
    int red, green, blue;

    kerror = gk0xin(*wktyp, &winind);
    if (kerror != 0) return kerror;

    if (wdt[winind].overlay)
    {

/*
 *	overlay planes always have black backgrounds
 */
	if ( *cind == 0 )
	{
	    *r = 0; *g = 0; *b = 0;
	}
	else
	{

/*
 *	    Overlay forgrounds are just whatever is currently set
 */
	    screen_def.pixel = (wdt[winind].pixels)[1] | ~wdt[winind].mask;
	    XQueryColor( wdt[winind].display, wdt[winind].colormap,
		&screen_def );
	    *r = (float)screen_def.red/65535.0;
	    *g = (float)screen_def.green/65535.0;
	    *b = (float)screen_def.blue/65535.0;
	}
    }
    else

/*
 *  Normal window
 */
    {

	if ( *cind == 0 )
	{
	    status = GWM_GetBgCol( wdt[winind].display, wdt[winind].win,
		&pcolor_name);
	}
	else
	{
	    if ( *cind == 1 )
	    {
	    	status = GWM_GetFgCol( wdt[winind].display, wdt[winind].win,
		    &pcolor_name);
	    }
	}
	if ( (*cind > 1) || (status != GWM_SUCCESS) )
	{
            red = (*r)*65535.0;
            green = (*g)*65535.0;
            blue = (*b)*65535.0;
	    sprintf( color_name, "#%04x%04x%04x", red, green, blue );
	    pcolor_name = color_name;
	}

/*
 *	Convert the colour specification to RGB
 */
	status = XParseColor( wdt[winind].display, wdt[winind].colormap,
	    pcolor_name, &screen_def);

/*
 *	Free the memory allocated by GMW_GetXg
 */
	if (pcolor_name != color_name) XFree( pcolor_name);

	if ( !status ) return GKS_IO_ERR;

/*
 *	If the colour table is static then allocate a read-only colour
 *      cell to find out what the nearest colour the hardware can supply
 *	is.
 */
	if ( !wdt[winind].ctwrite ) status = XAllocColor( wdt[winind].display,
		wdt[winind].colormap, &screen_def);

/*
 *	If the table is writeable but mono then convert the colour to
 *      the equivalent grey otherwise just scale to 0.0-1.0.
 */
	if ( wdt[winind].mono )
	{
	    max = screen_def.red > screen_def.green ? screen_def.red :
		screen_def.green;
	    max = max > screen_def.blue ? max : screen_def.blue;
	    min = screen_def.red < screen_def.green ? screen_def.red :
		screen_def.green;
	    min = min < screen_def.blue ? min : screen_def.blue;
	    *r = 0.5 * ( (float)min + (float)max )/65535.0;
	    *g = *b = *r;
	}
	else
	{
	    *r = (float)screen_def.red/65535.0;
	    *g = (float)screen_def.green/65535.0;
	    *b = (float)screen_def.blue/65535.0;
	}
    }
    return 0;
}

void gk0xsi( int *kwkix, char *prompt, int *lprompt, char *reply, int *lreply,
             int *nout)
/*
    Prompt for keyboard inpout and return the characters types

    Arguments: (given): kwkix; workstation index
			prompt; address of prompt string
			lprompt; length of prompt
			reply; string input
			lreply; length of input buffer
		(returned): nout; number of characters input

On VMS the pointers to character strings are actually pointers to VMS string
descriptors so the arguments are copied in a system dependent way to local
copies of the pointers. The default code is right for UNIX.

*/
#define STRING_SIZE 2
{
    Window win;
    XEvent event;
    KeySym keysym;
    char *prompt_l, *reply_l;
    int width_w, height_w, nbytes;
    int direction, font_ascent, font_descent;
    XCharStruct overall;
    char string[2];
/*
 *  fix up the pointers the strings
 */
#ifdef VMS
    prompt_l = *(int*)(prompt + 4);
    reply_l = *(int*)(reply + 4);
#else
    prompt_l = prompt;
    reply_l = reply;
#endif

/*
 *  Calculate a suitable window size from the fonts size and the number
 *  of characters
 */
    width_w = (*lprompt + *lreply + 2) * wdt[indx[*kwkix-1]].font_w;
    height_w = (wdt[indx[*kwkix-1]].font_d + wdt[indx[*kwkix-1]].font_h) * 2;

/*
 *  Create the window
 */
    win = XCreateSimpleWindow( wdt[indx[*kwkix-1]].display,
	wdt[indx[*kwkix-1]].win, (wdt[indx[*kwkix-1]].width - width_w)/2,
	(wdt[indx[*kwkix-1]].height - height_w)/2,
	width_w, height_w,  2,
	wdt[indx[*kwkix-1]].pixels[1], wdt[indx[*kwkix-1]].pixels[0]);

/*
 *  Set foreground colour
 */
    if ( wdt[indx[*kwkix-1]].overlay)
	XSetForeground( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].gc,
	    ~0);
    else
	XSetForeground( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].gc,
	    (wdt[indx[*kwkix-1]].pixels)[1] );
/*
 *  Map the window
 */
    XSelectInput( wdt[indx[*kwkix-1]].display, win, StructureNotifyMask );
    XMapWindow( wdt[indx[*kwkix-1]].display, win );
    XWindowEvent( wdt[indx[*kwkix-1]].display, win, StructureNotifyMask,
	&event);

/*
 *  Draw the prompt
 */
    XDrawString( wdt[indx[*kwkix-1]].display, win, wdt[indx[*kwkix-1]].gc,
	wdt[indx[*kwkix-1]].font_w, (3*height_w)/4,
	prompt_l, *lprompt);
    XFlush( wdt[indx[*kwkix-1]].display );

/*
 *  Find the extent of the text string
 */
    XQueryTextExtents(wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].font,
	prompt_l, *lprompt, &direction, &font_ascent, &font_descent, &overall);

/*
 *   Get input focus
 */
    XSetInputFocus( wdt[indx[*kwkix-1]].display, win, RevertToPointerRoot,
	CurrentTime);

/*
 *  Ask for notification of key press events
 */
    XSelectInput( wdt[indx[*kwkix-1]].display, win, KeyPressMask);

/*
 *  Loop reading keyboard events
 */
    for ( *nout = 0; *nout < *lreply;)
    {
	XWindowEvent( wdt[indx[*kwkix-1]].display, win, KeyPressMask, &event);

/*
 *  Convert key hit to ASCII
 */
	nbytes = XLookupString( &event.xkey, string, STRING_SIZE, &keysym, 0);

/*
 *   Ignore non printing keys
 */

	if (!IsCursorKey(keysym) && !IsFunctionKey(keysym) &&
	    !IsKeypadKey(keysym) && !IsMiscFunctionKey(keysym) &&
	    !IsModifierKey(keysym) && !IsPFKey(keysym))
	{

/*
 *  Record the key and echo it to the window
 */
	    if ( nbytes >0 )
	    {
		reply_l[(*nout)++] = string[0];
		XDrawString( wdt[indx[*kwkix-1]].display, win,
		    wdt[indx[*kwkix-1]].gc,
		    wdt[indx[*kwkix-1]].font_w + overall.width,
		    (3*height_w)/4, reply_l, *nout);
		XFlush( wdt[indx[*kwkix-1]].display );
	    }
	}
    }

/*
 *  Destroy the window
 */
    XDestroyWindow(wdt[indx[*kwkix-1]].display, win);
    XSync( wdt[indx[*kwkix-1]].display, 0 );
}

int gk0xws( int *wktyp, int *xp, int *yp, float *xm, float *ym)
/*
    Return the window size in pixels and metres

    Arguments: (given): wktyp; workstation type
	       (returned):xp, yp; window size in pixels
			 :xm, ym; window size in metres
*/
{
    int winind, kerror;

    kerror = gk0xin(*wktyp, &winind);
    if (kerror != 0) return kerror;

    *xp = wdt[winind].width;
    *yp = wdt[winind].height;
    *xm = wdt[winind].xmetre;
    *ym = wdt[winind].ymetre;

    return 0;
}

int gk0xcf( int *wktyp, int *col, int *dyn, int *bpc, int *ncols)
/*
    Return various information about the colour facilites available to
    a window

    Arguments: (given): wktyp; workstation type
	       (returned):col; whether colour is supported
			  dyn; whether the colour table is dynamic
			  bpc; the number of bits per colour;
			  ncols; the number of entries in the colour table
*/
{
    int winind, kerror;

    kerror = gk0xin(*wktyp, &winind);
    if (kerror != 0) return kerror;

    *dyn = wdt[winind].ctwrite ? GYES : GNO;
    *col = wdt[winind].mono ? GNO : GYES;
    if (wdt[winind].overlay)
    {
	*bpc = 1;
	*ncols = 2;
    }
    else
    {
	*bpc = wdt[winind].visualInfo.bits_per_rgb;
	*ncols = wdt[winind].npix;
    }

    return 0;
}

void gk0xrl( int *kwkix, int *prompt, float *inix, float *iniy,
		float *x, float *y, int *key)
/*
    Return cursor position and key pressed

    Arguments: (given): kwkix; workstation index
			prompt; whether cursor should be grabbed
			inix, iniy; initial cursor position
	       (returned):  x, y; selected cursor position
			    key; ASCII code of key pressed
*/
#define STRING_SIZE 2
{
    Window root_window, child_window, focus, inwin;
    int ix, iy, rx, ry, wx, wy, dumx, dumy;
    int nbytes, revert;
    unsigned int button_mask;
    XSetWindowAttributes wind_attrib;
    XWindowAttributes attrib;
    XEvent event;
    char string[STRING_SIZE];
    KeySym keysym;
    int xscroll, yscroll;

/*
 *  Save window and position of cursor and the current keyboard owner so that
 *  we can put them back when we are done.
 */
    XQueryPointer( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].win,
	&root_window, &child_window, &rx, &ry, &wx, &wy, &button_mask);
    XGetInputFocus( wdt[indx[*kwkix-1]].display, &focus, &revert);

/*
 *  Ensure that the scroll data is up to date. If the user has messed
 *  with the window size since the device was opened the picture can
 *  aways be corrected by refreshing it but we could end up returning
 *  incorrect cursor positions.
 */
    if ( wdt[indx[*kwkix-1]].overlay )
	GWM_GetOvScroll( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].win,
	    &wdt[indx[*kwkix-1]].xscroll, &wdt[indx[*kwkix-1]].yscroll);
    else
	GWM_GetScroll( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].win,
	    &wdt[indx[*kwkix-1]].xscroll, &wdt[indx[*kwkix-1]].yscroll);

/*
**  Create and map an input only window on top of the drawing area window
*/
    wind_attrib.event_mask =
	ButtonPressMask | KeyPressMask | StructureNotifyMask;
    inwin = XCreateWindow( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].win,
        wdt[indx[*kwkix-1]].xscroll, wdt[indx[*kwkix-1]].yscroll,
        wdt[indx[*kwkix-1]].width, wdt[indx[*kwkix-1]].height, 0, 0,
	InputOnly, CopyFromParent, CWEventMask, &wind_attrib );
    XMapWindow( wdt[indx[*kwkix-1]].display, inwin);
    XWindowEvent( wdt[indx[*kwkix-1]].display, inwin, StructureNotifyMask, &event );

/*
 * Define the cursor
 */
    if (*prompt == GYES)
    {
	XDefineCursor( wdt[indx[*kwkix-1]].display, inwin,
	    wdt[indx[*kwkix-1]].cursor);

/*
 *  Move the pointer to the specified initial position
 */
	ix = (short) (*inix + 0.5);
	iy = (short) (wdt[indx[*kwkix-1]].height - 1
	    - (int)(*iniy + 0.5));
	XWarpPointer( wdt[indx[*kwkix-1]].display, None, inwin, 0, 0, 0, 0,
	    ix, iy);
    }

/*
 *   Get input focus. We have to check that the window is mapped first
 *   and grab the server while we make the check to avoid a race
 *   condition.
 */
    XGrabServer( wdt[indx[*kwkix-1]].display);
    XGetWindowAttributes( wdt[indx[*kwkix-1]].display,
	wdt[indx[*kwkix-1]].win, &attrib);
    if ( attrib.map_state != IsViewable)
    {
    	XUngrabServer( wdt[indx[*kwkix-1]].display);
    	XSelectInput( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].win,
	    attrib.your_event_mask | StructureNotifyMask);
	XMapWindow( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].win);
    	XWindowEvent( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].win,
	    StructureNotifyMask, &event);

/*
 *    Check again as mapping the window is not guaranteed to make it viewable
 */
        XGetWindowAttributes( wdt[indx[*kwkix-1]].display,
	    wdt[indx[*kwkix-1]].win, &attrib);
        if ( attrib.map_state == IsViewable)
        {
    	    XSetInputFocus( wdt[indx[*kwkix-1]].display, inwin,
	        RevertToPointerRoot, CurrentTime);
        }
    }
    else
    {
    	XSetInputFocus( wdt[indx[*kwkix-1]].display, inwin,
	    RevertToPointerRoot, CurrentTime);
    	XUngrabServer( wdt[indx[*kwkix-1]].display);
    }

/*
 *  Wait for a suitable event to arrive
 */
    for ( *key = 0; *key == 0;)
    {
	XWindowEvent( wdt[indx[*kwkix-1]].display, inwin,
	    ButtonPressMask | KeyPressMask, &event);

/*
 *  Decode the key or button press
 */
	if (event.type == ButtonPress)
	{
	    switch (event.xbutton.button)
	    {
		case Button1:
		    *key = -1;
		    break;
		case Button2:
		    *key = -2;
		    break;
		case Button3:
		    *key = -3;
		    break;
		case Button4:
		    *key = -4;
		    break;
		case Button5:
		    *key = -5;
		    break;
	    }
	}
	else
	{
	    nbytes = XLookupString( &event.xkey, string, STRING_SIZE, &keysym,
									    0);
/*
 *   Ignore non printing keys
 */

	    if (!IsCursorKey(keysym) && !IsFunctionKey(keysym) &&
		!IsKeypadKey(keysym) && !IsMiscFunctionKey(keysym) &&
		!IsModifierKey(keysym) && !IsPFKey(keysym))

		if ( nbytes >0 ) *key = string[0];
	}
    }

/*
 *  Read the cursor position unless this is a faked input event.
 */
    if ( event.xany.send_event ) {
        ix = event.xkey.x;
        iy = event.xkey.y;
    } else {
        XQueryPointer( wdt[indx[*kwkix-1]].display, inwin,
	    &root_window, &child_window, &dumx, &dumy, &ix, &iy, &button_mask);
    }
    if (*prompt == GYES)
    {

/*
 *  Check the scrolls again.
 */
    if ( wdt[indx[*kwkix-1]].overlay )
	GWM_GetOvScroll( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].win,
	    &xscroll, &yscroll);
    else
	GWM_GetScroll( wdt[indx[*kwkix-1]].display, wdt[indx[*kwkix-1]].win,
	    &xscroll, &yscroll);

/*
 *   Destroy the crosshair cursor and move the original cursor back to its
 *   original position
 */
	XUndefineCursor( wdt[indx[*kwkix-1]].display, inwin);
	XWarpPointer( wdt[indx[*kwkix-1]].display, None, root_window, 0, 0,
	    0, 0, rx, ry);
    }

/*
 *  Restore the original input focus
 */
    XSetInputFocus( wdt[indx[*kwkix-1]].display, focus, revert, CurrentTime);
    XFlush( wdt[indx[*kwkix-1]].display );

/*
 *  Destroy the input window
 */
    XDestroyWindow(wdt[indx[*kwkix-1]].display, inwin);

/*
 *  Return the cursor position
 */
    *x = (float)(ix + wdt[indx[*kwkix-1]].xscroll - xscroll);
    *y = (float) (wdt[indx[*kwkix-1]].height - 1 - iy + yscroll -
                   wdt[indx[*kwkix-1]].yscroll);

/*
 *  Update the scrolls.
*/
    wdt[indx[*kwkix-1]].xscroll = xscroll;
    wdt[indx[*kwkix-1]].yscroll = yscroll;
}
