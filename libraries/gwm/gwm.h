/*
**++
**  FUNCTIONAL DESCRIPTION:
**
**	Function and structure definitions for GWM
**
**--
*/
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>

/*
**  Functions
*/
int GWM_FindWindow( Display *display, char name[], Window *win_id);

int GWM_CreateWindow( int argc, char *argv[], Display **display, char[]);

int GWM_DestroyWindow( Display *display, char name[]);

int GWM_GetPixmap( Display *display, Window win, Pixmap *pix_id);

int GWM_GetOvMask( Display *display, Window win, unsigned long *mask);

int GWM_SetPixmap( Display *display, Window win, Pixmap pix_id);

int GWM_GetColTable( Display *display, Window win, unsigned long
	**table, unsigned long *size);

int GWM_SetColTable( Display *display, Window win, unsigned long
	*table, unsigned long size);

int GWM_GetScroll( Display *display, Window win, int *xscroll, int *yscroll);

int GWM_GetOvScroll( Display *display, Window win, int *xscroll, int *yscroll);

int GWM_SetScroll( Display *display, Window win, int xscroll, int yscroll);

int GWM_SetOvScroll( Display *display, Window win, int xscroll, int yscroll);

int GWM_GetBgCol( Display *display, Window win_id, char **bg);

int GWM_SetBgCol( Display *display, Window win_id, char *bg);

int GWM_GetFgCol( Display *display, Window win_id, char **bg);

int GWM_SetFgCol( Display *display, Window win_id, char *bg);

int GWM_SetColour( Display *display, Window win, unsigned long entry,
	unsigned long r, unsigned long g, unsigned long b);
