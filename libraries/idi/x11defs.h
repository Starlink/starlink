
/* ----------------------------------------------------------------- */
/*            Include file x11defs.h                                 */
/* ----------------------------------------------------------------- */

# include <X11/Xlib.h>
# include <X11/Xutil.h>
/*
# include <X11/cursorfont.h>
# include <X11/keysym.h>
*/

#if defined(VMS)
#define NOSHARE extern noshare
#else
#define NOSHARE
#endif

#define XLUTFACT 65535.0




NOSHARE Colormap             cmap;
NOSHARE Cursor               cur_def, cur_def_int;
NOSHARE Display              *display_id;
NOSHARE GC                   gcima, gcdraw, gccurs, gclut, gcalph, gcpix;
NOSHARE KeySym               key;
NOSHARE Pixmap               pixmap_id;
NOSHARE Status               istat;
NOSHARE Visual               *visual;
NOSHARE Window               w_id, lutwnd, alph;


NOSHARE XColor               defs[256], xcol;
NOSHARE XEvent               evw_data, event, *eventptr;
NOSHARE XGCValues	     values_ima, values_draw, values_curs;
NOSHARE XFontStruct	     *font_info;
NOSHARE XImage               *ima, *zima[MAX_MEM], *lutima;
NOSHARE XPoint               vlist[8192];
NOSHARE XSegment	     vect[1024];
NOSHARE XSegment             curso0[4], curso1[4], roio[4];
NOSHARE XSetWindowAttributes setattr;
/* XSizeHints         hint; */
NOSHARE XWindowAttributes    attr;


NOSHARE unsigned long        event_mask;
NOSHARE int                  screen;
NOSHARE unsigned long int    black, white;
NOSHARE unsigned int	    cur_id;

Colormap ColormapOfWindow();
Visual *VisualOfWindow();


