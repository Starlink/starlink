/*
**++
**  FUNCTIONAL DESCRIPTION:
**
**	Internal Function and structure definitions for GWM
**
**--
*/
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include "gwm.h"

/*
**  Structure for passing window information to the event handling routine
*/
struct wininfo
    {
	Display *display;
	Window win_id;
	int visclass;
	Atom winname_atom;
	unsigned long *ctable;
	unsigned long ctsize;
	Pixmap pix_id;
	Atom pix_atom;
	unsigned int win_width;
	unsigned int win_height;
	int x_pos;
	int y_pos;
	unsigned int pix_width;
	unsigned int pix_height;
	int x_offset;
	int y_offset;
	Atom xoff_atom;
	Atom yoff_atom;
	int x_ov_offset;
	int y_ov_offset;
	Atom xov_off_atom;
	Atom yov_off_atom;
	Atom col_atom;
	GC gc;
	unsigned long mask;
	Colormap cmap;
    };

/*
**  Functions
*/
int GWM_MakeWindow ( Display *display, char name[], char *title, int ncols,
			Boolean inter, Boolean iconic, unsigned int border,
			char *fg, char *bg, Boolean overlay, char *ovcol,
			int size_x, int size_y, int pos_x, int pos_y,
			Window *win_id);

int GWM_MakeIntoWindow ( Display *display, Window win_id, char *name,
                         unsigned int width, unsigned int height, int ncols,
                         int mincols, int fg, int bg, Boolean ovl, int ovcol);

int GWM_GetWinInfo( Display *display, Window win_id, struct wininfo **info);

int GWM_ProcessEvent( struct wininfo *info, XEvent *event);

void GWM_Error( int status );

void GWM_ErrorMessage( int status, char *msgbuf, int msgbuflen );

#if defined(VMS)
int GWM_VMSexecvp( char *name, char *argv[], Display **display,
	char *win_name);
#endif
