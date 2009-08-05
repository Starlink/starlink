/*
  This file implements the Gwm canvas item type.
*/
#include <math.h>

#include <stdlib.h>
#if defined(HAVE_MALLOC_H)
#  include <malloc.h>
#endif

#include "tcl.h"
#include "tk.h"
#include "X11/Xatom.h"
#include "gwm.h"
#include "gwm_sys.h"
#include "gwm_err.h"
#include "tkGwm_sys.h"

#define BUFLEN 64

extern Tk_ConfigSpec itemSpecs[];

static void itemPosition(GwmItem*, double, double);
static int GwmCanvCmd(ClientData, Tcl_Interp*, int, char**);
static void Clear(GwmItem *);
static void PropChangeHandler(ClientData, XEvent*);

int tkgwmItemCreate(Tcl_Interp* interp, Tk_Canvas canvas, Tk_Item* itemPtr,
   int argc, char** argv)
/*
   Create a gwm canvas item
*/
{
    GwmItem* gwmItemPtr = (GwmItem*)itemPtr;
    Window  win = Tk_WindowId(Tk_CanvasTkwin(canvas));
    Display* display = Tk_Display(Tk_CanvasTkwin(canvas));
    double x;
    double y;
    Atom atom;
    Atom actualType;
    int actualFormat;
    unsigned long nitems;
    unsigned long bytesAfter;
    int dummy;
    int status;
    XGCValues gcvalues;
    char msgbuf[BUFLEN];

/*
   Check number of arguments.
*/
    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args:  should be \"",
	    Tk_PathName(Tk_CanvasTkwin(canvas)),
	    " create gwm x y ?options?\"", (char *) NULL);
	return TCL_ERROR;
    }

/*
   Check that it isn't already a gwm window.
*/
    if ( win == 0 )
    {
	Tk_MakeWindowExist(Tk_CanvasTkwin(canvas));
	win = Tk_WindowId(Tk_CanvasTkwin(canvas));
    }
    atom = Tk_InternAtom( Tk_CanvasTkwin(canvas), "GWM_name");
    (void)XGetWindowProperty( display, win, atom, 0, 1, False, XA_STRING,
	&actualType, &actualFormat, &nitems, &bytesAfter,
	(unsigned char**)(&dummy));
    if (actualType != None)
    {
	Tcl_AppendResult(interp, "Canvas already has a gwm item", (char*)NULL);
	return TCL_ERROR;
    }

/*
   Get the window position from the first two arguments.
*/
    if (Tk_CanvasGetCoord(interp, canvas, argv[0], &x) != TCL_OK)
	return TCL_ERROR;
    if (Tk_CanvasGetCoord(interp, canvas, argv[1], &y) != TCL_OK)
	return TCL_ERROR;

/*
   Initialise the item record.
*/
    gwmItemPtr->tkwin = Tk_CanvasTkwin(canvas);
    gwmItemPtr->interp = interp;
    gwmItemPtr->bg = NULL;
    gwmItemPtr->fg = NULL;
#if 0
    gwmItemPtr->ovcolour = NULL;
#endif
    gwmItemPtr->name = NULL;
    gwmItemPtr->command = NULL;
/*
   Parse the remaining arguments.
*/
   if (tkgwmItemConfigure(interp, canvas, itemPtr, argc - 2, &argv[2], 0)
	!= TCL_OK) return TCL_ERROR;

/*
   Turn the canvas window into a gwm window.
*/
    status = GWM_MakeIntoWindow(display, win, gwmItemPtr->name,
        gwmItemPtr->width, gwmItemPtr->height, gwmItemPtr->cols,
	gwmItemPtr->mincols, gwmItemPtr->fg->pixel, gwmItemPtr->bg->pixel,
	0, 0);
#if 0
    status = GWM_MakeIntoWindow(display, win, gwmItemPtr->name,
        gwmItemPtr->width, gwmItemPtr->height, gwmItemPtr->cols,
	gwmItemPtr->mincols, gwmItemPtr->fg->pixel, gwmItemPtr->bg->pixel,
	gwmItemPtr->overlay, gwmItemPtr->ovcolour->pixel);
#endif

    if (status == GWM_SUCCESS) {
        status = GWM_GetWinInfo(display, win, &(gwmItemPtr->info));
        gwmItemPtr->cols = gwmItemPtr->info->ctsize;
	gcvalues.background = (gwmItemPtr->info->ctable)[0];
	gcvalues.plane_mask = gwmItemPtr->info->mask;
        gwmItemPtr->gc = Tk_GetGC( Tk_CanvasTkwin(canvas),
	    GCBackground | GCPlaneMask, &gcvalues);
#if 0
	if (gwmItemPtr->overlay)
	{
	    gcvalues.plane_mask = ~(gwmItemPtr->info->mask);
            gwmItemPtr->ovgc = Tk_GetGC( Tk_CanvasTkwin(canvas),
		GCBackground | GCPlaneMask, &gcvalues);
	}
#endif

/*
   Request notification of property notify events.
*/
	Tk_CreateEventHandler(Tk_CanvasTkwin(canvas), PropertyChangeMask,
	    PropChangeHandler, gwmItemPtr);

/*
   Set the bounding box from the size position and overlay scroll offsets.
*/
	itemPosition(gwmItemPtr, x, y);

    } else {
        GWM_ErrorMessage(status, msgbuf, BUFLEN);
        Tcl_AppendResult(interp, msgbuf, (char *) NULL);
        return TCL_ERROR;
    }
    return TCL_OK;
}


int tkgwmItemConfigure(Tcl_Interp* interp, Tk_Canvas canvas, Tk_Item* itemPtr,
   int argc, char** argv, int flags)
{
    Atom atom;
    GwmItem* gwmItemPtr = (GwmItem*)itemPtr;
    Tk_ConfigSpec *p;
    Tk_Window tkwin;
    XColor col;
    const char *colour;
    int i;

/*
   Parse argv and update the itemSpecs structure
*/
    tkwin = Tk_CanvasTkwin(canvas);
    if (Tk_ConfigureWidget(interp, tkwin, itemSpecs, argc, (const char **)argv,
            (char *) gwmItemPtr, flags) != TCL_OK) {
        return TCL_ERROR;
    }

    /* Set config flags (TK_CONFIG_OPTION_SPECIFIED not supported in Tk8.5) */
    for ( p = itemSpecs; p->type != TK_CONFIG_END; p++ ) {

        /*  Default is cleared. */
        p->specFlags &= ~TK_CONFIG_OPTION_SPECIFIED;

        for ( i = 0; i < argc; i +=2 ) {
            if ( strcmp( argv[i], p->argvName ) == 0 ) {
                /*  Option in list, so set. */
                p->specFlags |= TK_CONFIG_OPTION_SPECIFIED;
                break;
            }
        }
    }

/*
   Apply any changes to the gwm window.
*/
    if (flags)
    {
	if (itemSpecs[0].specFlags & TK_CONFIG_OPTION_SPECIFIED)
	{
	    col.flags = DoRed | DoGreen | DoBlue;
	    col.pixel =  (gwmItemPtr->info->ctable)[0];
	    col.red = gwmItemPtr->bg->red;
	    col.green = gwmItemPtr->bg->green;
	    col.blue = gwmItemPtr->bg->blue;
	    XStoreColor( gwmItemPtr->info->display, gwmItemPtr->info->cmap,
		&col);
	    colour = Tk_NameOfColor(&col);
	    atom = Tk_InternAtom( tkwin, "GWM_background");
	    XChangeProperty( gwmItemPtr->info->display,
		gwmItemPtr->info->win_id, atom, XA_STRING, 8, PropModeReplace,
		(unsigned char*)colour,  strlen(colour));
	}
	if (itemSpecs[1].specFlags & TK_CONFIG_OPTION_SPECIFIED)
	{
	    col.flags = DoRed | DoGreen | DoBlue;
	    col.pixel =  (gwmItemPtr->info->ctable)[1];
	    col.red = gwmItemPtr->fg->red;
	    col.green = gwmItemPtr->fg->green;
	    col.blue = gwmItemPtr->fg->blue;
	    XStoreColor( gwmItemPtr->info->display, gwmItemPtr->info->cmap,
		&col);
	    colour = Tk_NameOfColor(&col);
	    atom = Tk_InternAtom( tkwin, "GWM_foreground");
	    XChangeProperty( gwmItemPtr->info->display,
		gwmItemPtr->info->win_id, atom, XA_STRING, 8, PropModeReplace,
		(unsigned char*)colour,  strlen(colour));
	}
#if 0
	if (itemSpecs[3].specFlags & TK_CONFIG_OPTION_SPECIFIED)
	{
	    col.flags = DoRed | DoGreen | DoBlue;
	    col.red = gwmItemPtr->ovcolour->red;
	    col.green = gwmItemPtr->ovcolour->green;
	    col.blue = gwmItemPtr->ovcolour->blue;
	    for (i=0; i < gwmItemPtr->info->ctsize; i++)
	    {
		col.pixel =  (gwmItemPtr->info->ctable)[i] |
		    ~(gwmItemPtr->info->mask);
		XStoreColor( gwmItemPtr->info->display, gwmItemPtr->info->cmap,
		    &col);
	    }
	}
	if (itemSpecs[4].specFlags & TK_CONFIG_OPTION_SPECIFIED ||
	    itemSpecs[5].specFlags & TK_CONFIG_OPTION_SPECIFIED)
	{
	    itemPosition(gwmItemPtr, gwmItemPtr->info->x_offset,
		gwmItemPtr->info->y_offset);
	}
#endif
    }
    else
    {

/*
   Create the widget control command.
*/
	if ((itemSpecs[2].specFlags & TK_CONFIG_OPTION_SPECIFIED) &&
	    gwmItemPtr->command != NULL)
	{
	    gwmItemPtr->itemCmd =
                Tcl_CreateCommand(interp,
                                  (const char *)gwmItemPtr->command,
                                  (Tcl_CmdProc *)GwmCanvCmd,
                                  (ClientData)itemPtr,
                                  (void (*)()) NULL);
	}
    }

    return TCL_OK;
}


int tkgwmItemCoord(Tcl_Interp* interp, Tk_Canvas canvas, Tk_Item* itemPtr,
   int argc, char** argv)
{
    char c0[TCL_DOUBLE_SPACE], c1[TCL_DOUBLE_SPACE];
    GwmItem* gwmItemPtr = (GwmItem*)itemPtr;
    double newX;
    double newY;

    if ( argc == 0 )
    {
        Tcl_PrintDouble(interp, gwmItemPtr->header.x1, c0);
        Tcl_PrintDouble(interp, gwmItemPtr->header.y1, c1);
        Tcl_AppendResult(interp, c0, " ", c1, (char *) NULL);
    }
    else if (argc == 2)
    {
        if (Tk_CanvasGetCoord(interp, canvas, argv[0], &newX) != TCL_OK)
	    return TCL_ERROR;
        if (Tk_CanvasGetCoord(interp, canvas, argv[1], &newY) != TCL_OK)
	    return TCL_ERROR;
        itemPosition(gwmItemPtr, newX, newY);
    }
    else
    {
        Tcl_AppendResult(interp, "wrong # coordinates:  expected 0 or 2",
            (char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}


void tkgwmItemDelete(Tk_Canvas canvas, Tk_Item* itemPtr, Display* display)
{
    GwmItem* gwmItemPtr = (GwmItem*)itemPtr;
    char *propName;
    Atom atom;
    Atom actualType;
    int actualFormat;
    unsigned long nitems;
    unsigned long bytesAfter;
    unsigned char *dummy;

/*
   Delete the property change event handler.
*/
    Tk_DeleteEventHandler(gwmItemPtr->tkwin, PropertyChangeMask,
	PropChangeHandler, gwmItemPtr);

/*
   Remove the gwm window name from the root window.
*/
    propName = (char*)malloc( strlen(gwmItemPtr->name) + 5 );
    (void)strcpy( propName, "GWM_");
    (void)strcat( propName, gwmItemPtr->name );
    atom = XInternAtom(display, propName, False);
    free( propName );
    (void)XGetWindowProperty( display, DefaultRootWindow( display ) , atom,
        0, 1, True, XA_WINDOW, &actualType, &actualFormat, &nitems,
        &bytesAfter, &dummy);
    XFree(dummy);

/*
   Remove the gwm window properties from the canvas window (if it still
   exists).
*/
    if (Tk_CanvasTkwin(canvas))
    {
	atom = Tk_InternAtom( Tk_CanvasTkwin(canvas), "GWM_name" );
	(void)XGetWindowProperty( display, gwmItemPtr->info->win_id , atom,
	    0, 1, True, XA_STRING, &actualType, &actualFormat, &nitems,
	    &bytesAfter, &dummy);
	XFree(dummy);
	(void)XGetWindowProperty( display, gwmItemPtr->info->win_id , atom,
	    0, bytesAfter, True, XA_STRING, &actualType, &actualFormat, &nitems,
	    &bytesAfter, &dummy);
	XFree(dummy);

	(void)XGetWindowProperty( display, gwmItemPtr->info->win_id ,
	    gwmItemPtr->info->pix_atom, 0, 1, True, XA_PIXMAP, &actualType,
	    &actualFormat, &nitems, &bytesAfter, &dummy);
	XFree(dummy);

	atom = Tk_InternAtom( Tk_CanvasTkwin(canvas), "GWM_colour_table");
	(void)XGetWindowProperty( display, gwmItemPtr->info->win_id ,
	    atom, 0, gwmItemPtr->info->ctsize, True, XA_INTEGER, &actualType,
	    &actualFormat, &nitems, &bytesAfter, &dummy);
	XFree(dummy);

	atom = Tk_InternAtom( Tk_CanvasTkwin(canvas), "GWM_ov_mask");
	(void)XGetWindowProperty( display, gwmItemPtr->info->win_id , atom,
	    0, 1, True, XA_INTEGER, &actualType, &actualFormat, &nitems,
	    &bytesAfter, &dummy);
	XFree(dummy);

	(void)XGetWindowProperty( display, gwmItemPtr->info->win_id ,
	    gwmItemPtr->info->xoff_atom, 0, 1, True, XA_INTEGER, &actualType,
	    &actualFormat, &nitems, &bytesAfter, &dummy);
	XFree(dummy);

	(void)XGetWindowProperty( display, gwmItemPtr->info->win_id ,
	    gwmItemPtr->info->yoff_atom, 0, 1, True, XA_INTEGER, &actualType,
	    &actualFormat, &nitems, &bytesAfter, &dummy);
	XFree(dummy);

	(void)XGetWindowProperty( display, gwmItemPtr->info->win_id ,
	    gwmItemPtr->info->xov_off_atom, 0, 1, True, XA_INTEGER,
	    &actualType, &actualFormat, &nitems, &bytesAfter, &dummy);
	XFree(dummy);

	(void)XGetWindowProperty( display, gwmItemPtr->info->win_id ,
	    gwmItemPtr->info->yov_off_atom, 0, 1, True, XA_INTEGER,
	    &actualType, &actualFormat, &nitems, &bytesAfter, &dummy);
	XFree(dummy);

	atom = Tk_InternAtom( Tk_CanvasTkwin(canvas), "GWM_foreground");
	(void)XGetWindowProperty( display, gwmItemPtr->info->win_id , atom,
	    0, 1, True, XA_STRING, &actualType, &actualFormat, &nitems,
	    &bytesAfter, &dummy);
	XFree(dummy);
	(void)XGetWindowProperty( display, gwmItemPtr->info->win_id , atom,
	    0, bytesAfter, True, XA_STRING, &actualType, &actualFormat, &nitems,
	    &bytesAfter, &dummy);
	XFree(dummy);

	atom = Tk_InternAtom( Tk_CanvasTkwin(canvas), "GWM_background");
	(void)XGetWindowProperty( display, gwmItemPtr->info->win_id , atom,
	    0, 1, True, XA_STRING, &actualType, &actualFormat, &nitems,
	    &bytesAfter, &dummy);
	XFree(dummy);
	(void)XGetWindowProperty( display, gwmItemPtr->info->win_id , atom,
	    0, bytesAfter, True, XA_STRING, &actualType, &actualFormat, &nitems,
	    &bytesAfter, &dummy);
	XFree(dummy);

	(void)XGetWindowProperty( display, gwmItemPtr->info->win_id , atom,
	    0, bytesAfter, True, XA_STRING, &actualType, &actualFormat, &nitems,
	    &bytesAfter, &dummy);
	XFree(dummy);
    }

/*
   Delete the pixmap.
*/
    XFreePixmap( display, gwmItemPtr->info->pix_id);
    Tk_FreeXId( display, gwmItemPtr->info->pix_id);

/*
   Release the colour table entries.
*/
    if ( gwmItemPtr->info->visclass == GrayScale ||
	gwmItemPtr->info->visclass == PseudoColor ||
	gwmItemPtr->info->visclass == DirectColor )
    {
	XFreeColors( display, gwmItemPtr->info->cmap,
	    gwmItemPtr->info->ctable, gwmItemPtr->info->ctsize, 0);
    }

/*
   Free the GCs.
*/
    Tk_FreeGC( display, gwmItemPtr->gc);
#if 0
    if (gwmItemPtr->overlay) Tk_FreeGC( display, gwmItemPtr->ovgc);
#endif

/*
   Free the gwm info structure and colour table array.
*/
    free( gwmItemPtr->info->ctable);
    free( gwmItemPtr->info);

/*
   Delete the widget control command.
*/
    if (gwmItemPtr->command != NULL)
    {
	Tcl_DeleteCommand(gwmItemPtr->interp,
	    Tcl_GetCommandName(gwmItemPtr->interp, gwmItemPtr->itemCmd));
    }

}


void tkgwmItemDisplay(Tk_Canvas canvas, Tk_Item* itemPtr, Display* display,
   Drawable dst, int x, int y, int width, int height)
{
/*
   Copy the pixmap to the window.
*/
    GwmItem* gwmItemPtr = (GwmItem*)itemPtr;
    int redrawX;
    int redrawY;
    int redrawWidth;
    int redrawHeight;
    short drawableX;
    short drawableY;

/*
   Calculate the position (in canvas coordinates) and size of the area
   that needs to be re-drawn.
*/
    if ( x > gwmItemPtr->info->x_offset )
	redrawX = x;
    else
	redrawX = gwmItemPtr->info->x_offset;
    if ( y > gwmItemPtr->info->y_offset )
	redrawY = y;
    else
	redrawY = gwmItemPtr->info->y_offset;
    if ( x + width > gwmItemPtr->info->x_offset + gwmItemPtr->info->pix_width )
	redrawWidth = gwmItemPtr->info->x_offset +
	    gwmItemPtr->info->pix_width - redrawX;
    else
	redrawWidth = x + width - redrawX;
    if ( y + height > gwmItemPtr->info->y_offset +
	    gwmItemPtr->info->pix_height )
	redrawHeight = gwmItemPtr->info->y_offset +
	    gwmItemPtr->info->pix_height - redrawY;
    else
	redrawHeight = y + height - redrawY;

/*
   Convert the canvas coordinates to coordinates in the destination
   drawable.
*/
    Tk_CanvasDrawableCoords(canvas, (double)redrawX, (double)redrawY,
	 &drawableX, &drawableY);

/*
   Copy the relevant area of the pixmap.
*/
     XCopyArea( display, gwmItemPtr->info->pix_id, dst,
	gwmItemPtr->gc, redrawX - gwmItemPtr->info->x_offset,
	redrawY - gwmItemPtr->info->y_offset, redrawWidth, redrawHeight,
	drawableX, drawableY);

/*
   Repeat for the overlay plane (if it exists).
*/
#if 0
    if (gwmItemPtr->overlay)
    {
	if ( x > gwmItemPtr->info->x_ov_offset )
	    redrawX = x;
	else
	    redrawX = gwmItemPtr->info->x_ov_offset;
	if ( y > gwmItemPtr->info->y_ov_offset )
	    redrawY = y;
	else
	    redrawY = gwmItemPtr->info->y_ov_offset;
	if ( x + width > gwmItemPtr->info->x_ov_offset +
	        gwmItemPtr->info->pix_width )
	    redrawWidth = gwmItemPtr->info->x_ov_offset +
		gwmItemPtr->info->pix_width - redrawX;
	else
	    redrawWidth = x + width - redrawX;
	if ( y + height > gwmItemPtr->info->y_ov_offset +
	 	gwmItemPtr->info->pix_height )
	    redrawHeight = gwmItemPtr->info->y_ov_offset +
		gwmItemPtr->info->pix_height - redrawY;
	else
	    redrawHeight = y + height - redrawY;
	Tk_CanvasDrawableCoords(canvas, (double)redrawX, (double)redrawY,
	    &drawableX, &drawableY);
	XCopyArea( display, gwmItemPtr->info->pix_id, dst,
	    gwmItemPtr->ovgc, redrawX - gwmItemPtr->info->x_ov_offset,
	    redrawY - gwmItemPtr->info->y_ov_offset, redrawWidth, redrawHeight,
	    drawableX, drawableY);
    }
#endif
}


double tkgwmItemPoint(Tk_Canvas canvas, Tk_Item* itemPtr, double* pointPtr)
{
    GwmItem* gwmItemPtr = (GwmItem*)itemPtr;
    double x1 = (double)gwmItemPtr->header.x1;
    double x2 = (double)gwmItemPtr->header.x2;
    double y1 = (double)gwmItemPtr->header.y1;
    double y2 = (double)gwmItemPtr->header.y2;
/*
   Compute the shortest distance between the given point and the gwm
   window.
*/

    if ( pointPtr[0] < x1 )
    {
	if ( pointPtr[1] < y1 )
	{
	    return sqrt( (x1 - pointPtr[0])*(x1 - pointPtr[0]) +
		(y1 - pointPtr[1])*(y1 - pointPtr[1]) );
	}
	else if ( pointPtr[1] <= y2)
	{
	    return x1 - pointPtr[0];
	}
	else
	{
	    return sqrt( (x1 - pointPtr[0])*(x1 - pointPtr[0]) +
                (pointPtr[1] - y2)*(pointPtr[1] - y2) );
	}
    }
    else if ( pointPtr[0] <= x2 )
    {
	if ( pointPtr[1] < y1 )
	{
	    return y1 - pointPtr[1];
	}
	else if ( pointPtr[1] <= y2)
	{
	    return 0.0;
	}
	else
	{
	    return pointPtr[1] - y1;
	}
    }
    else
    {
	if ( pointPtr[1] < y1 )
	{
	    return sqrt( (pointPtr[0] - x2)*(pointPtr[0] - x2) +
		(y1 - pointPtr[1])*(y1 - pointPtr[1]) );
	}
	else if ( pointPtr[1] <= y2)
	{
	    return pointPtr[0] - x2;
	}
	else
	{
	    return sqrt( (pointPtr[0] - x2)*(pointPtr[0] - x2) +
                (pointPtr[1] - y2)*(pointPtr[1] - y2) );
	}
    }
}


int tkgwmItemArea(Tk_Canvas canvas, Tk_Item* itemPtr, double* rectPtr)
{
    GwmItem* gwmItemPtr = (GwmItem*)itemPtr;
    int overlap1;
    int overlap2;
/*
   Test the given area and the gwm items bounding box for overlap.
*/
    if ( rectPtr[0] >= (double)gwmItemPtr->header.x1 &&
         rectPtr[0] < (double)gwmItemPtr->header.x2 &&
         rectPtr[1] >= (double)gwmItemPtr->header.y1 &&
         rectPtr[1] < (double)gwmItemPtr->header.y2 )
    {
	overlap1 = 1;
    }
    else
    {
        overlap1 = 0;
    }
    if ( rectPtr[2] >= (double)gwmItemPtr->header.x1 &&
         rectPtr[2] < (double)gwmItemPtr->header.x2 &&
         rectPtr[3] >= (double)gwmItemPtr->header.y1 &&
         rectPtr[3] < (double)gwmItemPtr->header.y2 )
    {
	overlap2 = 1;
    }
    else
    {
        overlap2 = 0;
    }
    if ( overlap1 && overlap2)
    {
	return 1;
    }
    else if ( overlap1 || overlap2 )
    {
        return 0;
    }
    else
    {
	return -1;
    }
}


void tkgwmItemScale(Tk_Canvas canvas, Tk_Item* itemPtr, double originX,
   double originY, double scaleX, double scaleY)
{
    GwmItem* gwmItemPtr = (GwmItem*)itemPtr;
    double X;
    double Y;
/*
  Set new position.
*/
    X = originX + scaleX * (gwmItemPtr->header.x1 - originX);
    Y = originY + scaleY * (gwmItemPtr->header.y1 - originY);
    itemPosition(gwmItemPtr, X, Y);
}


void tkgwmItemTranslate(Tk_Canvas canvas, Tk_Item* itemPtr, double deltaX,
   double deltaY)
{
    GwmItem* gwmItemPtr = (GwmItem*)itemPtr;
    double X;
    double Y;
/*
  Set new position.
*/
    X = gwmItemPtr->header.x1 + deltaX;
    Y = gwmItemPtr->header.y1 + deltaY;
    itemPosition(gwmItemPtr, X, Y);
}

static void itemPosition(GwmItem* itemPtr, double X,
    double Y)
/*
   Updates the bounding box in the item header and the scroll offsets in
   the info structure and updates the window scroll properties to reflect
   a new window canvas item position.

   The gwm window size is taken from the info structure and the overlay
   scroll offsets from the item header.
*/
{
    long local_offset;

/*
   Set the bounding box from the size, position and overlay scroll offsets.
*/
    itemPtr->header.x1 = (int)floor(X);
    itemPtr->header.y1 = (int)floor(Y);
    itemPtr->header.x2 = (int)ceil(X) + itemPtr->info->pix_width;
    itemPtr->header.y2 = (int)ceil(Y) + itemPtr->info->pix_height;
#if 0
    if (itemPtr->xovoffset > 0)
    {
	itemPtr->header.x2 += itemPtr->xovoffset;
    }
    else
    {
	itemPtr->header.x1 += itemPtr->xovoffset;
    }
    if (itemPtr->yovoffset > 0)
    {
	itemPtr->header.y2 += itemPtr->yovoffset;
    }
    else
    {
	itemPtr->header.y1 += itemPtr->yovoffset;
    }
#endif

/*
   Set the offsets in the in info structure.
*/
    itemPtr->info->x_offset = (int)floor(X);
    itemPtr->info->y_offset = (int)floor(Y);
#if 0
    itemPtr->info->x_ov_offset = itemPtr->info->x_offset + itemPtr->xovoffset;
    itemPtr->info->y_ov_offset = itemPtr->info->y_offset + itemPtr->yovoffset;
#endif

/*
   Update the gwm window scroll properties.
*/
    local_offset = (long)itemPtr->info->x_offset;
    XChangeProperty( itemPtr->info->display, itemPtr->info->win_id,
	itemPtr->info->xoff_atom, XA_INTEGER, 32, PropModeReplace,
	(unsigned char*)(&local_offset), 1 );
    local_offset = (long)itemPtr->info->y_offset;
    XChangeProperty( itemPtr->info->display, itemPtr->info->win_id,
	itemPtr->info->yoff_atom, XA_INTEGER, 32, PropModeReplace,
	(unsigned char*)(&local_offset), 1 );

#if 0
    if (itemPtr->overlay)
    {
	local_offset = (long)itemPtr->info->x_ov_offset;
	XChangeProperty( itemPtr->info->display, itemPtr->info->win_id,
	    itemPtr->info->xov_off_atom, XA_INTEGER, 32, PropModeReplace,
	    (unsigned char*)(&local_offset), 1 );
	local_offset = (long)itemPtr->info->y_ov_offset;
	XChangeProperty( itemPtr->info->display, itemPtr->info->win_id,
	    itemPtr->info->yov_off_atom, XA_INTEGER, 32, PropModeReplace,
	    (unsigned char*)(&local_offset), 1 );
    }
#endif
}

static int
GwmCanvCmd(clientData, interp, argc, argv)
    ClientData clientData;              /* Information about gwm canvas item. */
    Tcl_Interp *interp;                 /* Current interpreter. */
    int argc;                           /* Number of arguments. */
    char **argv;                        /* Argument strings. */
{
    GwmItem* gwmItemPtr = (GwmItem*)clientData;
    int result = TCL_OK;

    int length;
    char c;
    int ctentry;
    XColor color;
#if 0
    int i;
#endif

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " option ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);

/*
 * "clear" command
 */
    if ((c == 'c') && (strncmp(argv[1], "clear", length) == 0)) {
	if (argc > 2 ) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " clear\"", (char *) NULL);
	    goto error;
	}
	Clear(gwmItemPtr);

/*
 * "get" command
 */
    } else if ((c == 'g') && (strncmp(argv[1], "get", length) == 0)) {
	if (argc != 4 ) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " get option arg\"", (char *) NULL);
	    goto error;
	}
    	c = argv[2][0];
    	length = strlen(argv[2]);

/*
 *  "get colour"
 */
    	if ((c == 'c') && (strncmp(argv[2], "colour", length) == 0)) {
	    if (Tcl_GetInt(interp, argv[3], &ctentry) != TCL_OK) {
		Tcl_AppendResult(interp,
			"invalid colour table entry specification \"", argv[3],
			"\"", (char *) NULL);
		goto error;
	    }
	    if ( ctentry < 0 || ctentry >= (int)gwmItemPtr->info->ctsize ) {
		Tcl_AppendResult(interp, "colour table entry \"",  argv[3],
			"\" out of range", (char *) NULL);
#if 0
	    if ( ctentry < -1 || ctentry >= (int)gwmItemPtr->info->ctsize ) {
		Tcl_AppendResult(interp, "colour table entry \"",  argv[3],
			"\" out of range", (char *) NULL);
#endif
		goto error;
	    }
	    if ( ctentry >= 0 ) {
	    	color.pixel = gwmItemPtr->info->ctable[ctentry];
#if 0
	    } else {
		if (gwmItemPtr->overlay) {
		    color.pixel = gwmItemPtr->info->ctable[0] |
			~gwmItemPtr->info->mask;
		} else {
		    Tcl_AppendResult(interp, "widget \"", argv[0],
			"\" does not have an overlay", (char *) NULL);
	    	    goto error;
		}
#endif
	    }
	    XQueryColor( gwmItemPtr->info->display, gwmItemPtr->info->cmap,
		&color);
	    Tcl_AppendResult(interp, Tk_NameOfColor(&color), (char *) NULL);

/*
 *  Unrecognised get option
 */
	} else {
	    Tcl_AppendResult(interp, "bad option \"", argv[2],
		    "\":  must be get colour", (char *) NULL);
	    goto error;
	}

#if 0
/*
 * "ovclear" command
 */
    } else if ((c == 'o') && (strncmp(argv[1], "ovclear", length) == 0)) {
	if (argc > 2 ) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " ovclear\"", (char *) NULL);
	    goto error;
	}
	Clear(gwmItemPtr);
#endif

/*
 * "set" command
 */
    } else if ((c == 's') && (strncmp(argv[1], "set", length) == 0)) {
	if (argc != 5 ) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " set option arg arg\"", (char *) NULL);
	    goto error;
	}
    	c = argv[2][0];
    	length = strlen(argv[2]);

/*
 *  "set colour"
 */
    	if ((c == 'c') && (strncmp(argv[2], "colour", length) == 0)) {
	    if (Tcl_GetInt(interp, argv[3], &ctentry) != TCL_OK) {
		Tcl_AppendResult(interp,
			"invalid colour table entry specification \"", argv[3],
			"\"", (char *) NULL);
		goto error;
	    }
	    if ( gwmItemPtr->info->visclass != PseudoColor &&
		gwmItemPtr->info->visclass != DirectColor &&
		gwmItemPtr->info->visclass != GrayScale ) {
		Tcl_AppendResult(interp, "colour table is read only",
			(char *) NULL);
		goto error;
	    }
	    if ( ctentry < 0 || ctentry >= (int)gwmItemPtr->info->ctsize ) {
		Tcl_AppendResult(interp, "colour table entry \"",  argv[3],
			"\" out of range", (char *) NULL);
#if 0
	    if ( ctentry < -1 || ctentry >= (int)gwmItemPtr->info->ctsize ) {
		Tcl_AppendResult(interp, "colour table entry \"",  argv[3],
			"\" out of range", (char *) NULL);
#endif
		goto error;
	    }
	    if ( !XParseColor( gwmItemPtr->info->display,
		    gwmItemPtr->info->cmap, argv[4], &color) ) {
		Tcl_AppendResult(interp, "invalid colour \"",  argv[4], "\"",
			(char *) NULL);
		goto error;
	    }
	    color.pixel = gwmItemPtr->info->ctable[ctentry];
	    if (ctentry >= 0) {
	    	color.flags =  DoRed | DoGreen | DoBlue;
	    	XStoreColor(gwmItemPtr->info->display, gwmItemPtr->info->cmap,
		    &color);
	    } else {
#if 0
		if (gwmItemPtr->overlay) {
	    	    for ( i = 0; i < gwmItemPtr->info->ctsize; i++ ) {
		    	color.pixel = (gwmItemPtr->info->ctable)[i] |
			    ~gwmItemPtr->info->mask;
		        XStoreColor( gwmItemPtr->info->display,
			gwmItemPtr->info->cmap, &color);
		    }
		} else {
		    Tcl_AppendResult(interp, "widget \"", argv[0],
			"\" does not have an overlay", (char *) NULL);
		    goto error;
	    	}
#endif
	    }
/*
 *  Unrecognised set option
 */
	} else {
	    Tcl_AppendResult(interp, "bad option \"", argv[2],
		    "\":  must be set colour or set crosshair", (char *) NULL);
	    goto error;
	}

/*
 * Command not recognised
 */
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
		"\":  must be configure, clear, get, ovclear, print or set",
		(char *) NULL);
	goto error;
    }
    return result;

    error:
    return TCL_ERROR;
}
/*
 *----------------------------------------------------------------------
 *
 * Clear --
 *
 *	This procedure clears the gwm window. It is called by the
 *      widget clear command
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The Gwm window is cleared
 *
 *----------------------------------------------------------------------
 */

static void Clear(GwmItem *gwmItemPtr)
{
    GC gc;                                        /* graphics context        */
    XGCValues gcval;                              /* graphics context values */


#if 0
    if (gwmItemPtr->overlay)
    {
/*
**  Create a graphics context with the foreground set to 0 and the plane
**  mask set to protect all planes except the overlay plane.
*/
	gcval.foreground = 0;
	gcval.plane_mask = ~gwmItemPtr->info->mask;
	gc = Tk_GetGC( gwmItemPtr->tkwin, GCForeground | GCPlaneMask, &gcval);
    }
    else
#endif
    {
/*
**  Create a graphics context with the foreground set to the background
**  colour of the window (entry 0 in the colour table array) and the plane
**  mask set to protect the overlay plane.
*/
	gcval.foreground = gwmItemPtr->info->ctable[0];
	gcval.plane_mask = gwmItemPtr->info->mask;
	gc = Tk_GetGC( gwmItemPtr->tkwin, GCForeground | GCPlaneMask, &gcval);
    }
/*
**  Erase the contents of the pixmap
*/
    XFillRectangle( gwmItemPtr->info->display, gwmItemPtr->info->pix_id, gc,
	0, 0, gwmItemPtr->info->pix_width, gwmItemPtr->info->pix_height);

/*
**  Erase the area of the window occupied by the pixmap
*/
    XFillRectangle( gwmItemPtr->info->display, gwmItemPtr->info->win_id, gc,
	gwmItemPtr->info->x_offset, gwmItemPtr->info->y_offset,
	gwmItemPtr->info->pix_width, gwmItemPtr->info->pix_height);

/*
**  Delete the graphics context
*/
    Tk_FreeGC( gwmItemPtr->info->display, gc );

}

static void
 PropChangeHandler(clientData, event)
    ClientData clientData;              /* Information about gwm canvas item. */
    XEvent *event;			/* X event structure */
{
    GwmItem* gwmItemPtr = (GwmItem*)clientData;
    Bool status;
    Atom actual_type;
    int actual_format;
    unsigned long nitems, bytes_after;
    Pixmap *local_pix_id;
    long *plocal_offset;
    int x, y;
    unsigned int border, depth;
    Window root;



/*
  If the property has been deleted then ignore the event
*/
    if ( event->xproperty.state == PropertyDelete) return;

/*
  Update the window info structure with the new value of the
  property that has change.
*/
    if ( event->xproperty.atom == gwmItemPtr->info->pix_atom )
    {
	status = XGetWindowProperty( gwmItemPtr->info->display,
	    gwmItemPtr->info->win_id, event->xproperty.atom, 0, 1, False,
            XA_PIXMAP, &actual_type, &actual_format, &nitems,
            &bytes_after, (unsigned char**)(&local_pix_id));
        if (!status)
        {
            gwmItemPtr->info->pix_id = *local_pix_id;
            XFree( (char*)local_pix_id );

/*
  Copy the new pixmap size into the info structure and update the item
  header.
*/
	    XGetGeometry( gwmItemPtr->info->display, gwmItemPtr->info->pix_id,
		&root, &x, &y, &(gwmItemPtr->info->pix_width),
		&(gwmItemPtr->info->pix_height), &border, &depth);
	    gwmItemPtr->width = gwmItemPtr->info->pix_width;
	    gwmItemPtr->height = gwmItemPtr->info->pix_height;
        }
    }
    else if ( event->xproperty.atom == gwmItemPtr->info->xoff_atom)
    {
        status = XGetWindowProperty( gwmItemPtr->info->display,
            gwmItemPtr->info->win_id, event->xproperty.atom, 0, 1, False,
            XA_INTEGER, &actual_type, &actual_format,
            &nitems, &bytes_after,
            (unsigned char**)(&plocal_offset));
        if (!status)
        {
            gwmItemPtr->info->x_offset = (int)*plocal_offset;
            XFree( (char*)plocal_offset);
        }
    }
    else if ( event->xproperty.atom == gwmItemPtr->info->yoff_atom)
    {
        status = XGetWindowProperty( gwmItemPtr->info->display,
            gwmItemPtr->info->win_id, event->xproperty.atom, 0, 1, False,
            XA_INTEGER, &actual_type, &actual_format,
            &nitems, &bytes_after,
            (unsigned char**)(&plocal_offset));
        if (!status)
        {
            gwmItemPtr->info->y_offset = (int)*plocal_offset;
            XFree( (char*)plocal_offset);
        }
    }
#if 0
    else if ( event->xproperty.atom == gwmItemPtr->info->xov_off_atom)
    {
        status = XGetWindowProperty( gwmItemPtr->info->display,
            gwmItemPtr->info->win_id, event->xproperty.atom, 0, 1, False,
            XA_INTEGER, &actual_type, &actual_format,
            &nitems, &bytes_after,
            (unsigned char**)(&plocal_offset));
        if (!status)
        {
            gwmItemPtr->info->x_ov_offset = (int)*plocal_offset;
            XFree( (char*)plocal_offset);
	    gwmItemPtr->xovoffset = gwmItemPtr->info->x_ov_offset;
        }
    }
    else if ( event->xproperty.atom == gwmItemPtr->info->yov_off_atom)
    {
        status = XGetWindowProperty( gwmItemPtr->info->display,
            gwmItemPtr->info->win_id, event->xproperty.atom, 0, 1, False,
            XA_INTEGER, &actual_type, &actual_format,
            &nitems, &bytes_after,
            (unsigned char**)(&plocal_offset));
        if (!status)
        {
            gwmItemPtr->info->y_ov_offset = (int)*plocal_offset;
            XFree( (char*)plocal_offset);
	    gwmItemPtr->yovoffset = gwmItemPtr->info->y_ov_offset;
        }
    }
#endif

/*
  recalculate the bounding box in the header with the new size, position
  and scroll offsets.
*/
    gwmItemPtr->header.x1 = gwmItemPtr->info->x_offset;
    gwmItemPtr->header.y1 = gwmItemPtr->info->y_offset;
    gwmItemPtr->header.x2 = gwmItemPtr->info->x_offset +
	gwmItemPtr->info->pix_width;
    gwmItemPtr->header.y2 = gwmItemPtr->info->y_offset +
	gwmItemPtr->info->pix_height;
#if 0
    if (gwmItemPtr->xovoffset > 0)
    {
	gwmItemPtr->header.x2 += gwmItemPtr->xovoffset;
    }
    else
    {
	gwmItemPtr->header.x1 += gwmItemPtr->xovoffset;
    }
    if (gwmItemPtr->yovoffset > 0)
    {
	gwmItemPtr->header.y2 += gwmItemPtr->yovoffset;
    }
    else
    {
	gwmItemPtr->header.y1 += gwmItemPtr->yovoffset;
    }
#endif
}

int tkgwmItemPostScript(Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
    int prepass)
{
    GwmItem* gwmItemPtr = (GwmItem*)itemPtr;
    int nc;
    XColor *ct, *ctr, *ctg, *ctb;
    int i, j, k;
    XImage *image;
    int colour;
    char *linebuf;
    char buffer[200];
    int inten;
    long pix, rpix, gpix, bpix;
    long rbase, gbase, bbase;
    XVisualInfo *vinfo, vinfo_template;
    XWindowAttributes winatt;
    int nitems;

    if (prepass) return TCL_OK;

/*
  Get a copy of the colour table
*/
    if (gwmItemPtr->info->visclass == DirectColor ||
          gwmItemPtr->info->visclass == TrueColor ) {
/*
    Decomposed colour model - get each colour component seperately.
*/

/*
    Fill out the visual info structure
*/
        XGetWindowAttributes( gwmItemPtr->info->display,
                Tk_WindowId(gwmItemPtr->tkwin), &winatt);
        vinfo_template.visualid = XVisualIDFromVisual( winatt.visual );
        vinfo = XGetVisualInfo( gwmItemPtr->info->display, VisualIDMask,
                &vinfo_template, &nitems);

/*
    Find first pixel value for each colour.
*/
       for ( rbase = 1; (vinfo->red_mask & rbase) == 0;
             rbase *= 2 );
       for ( gbase = 1; (vinfo->green_mask & gbase) == 0;
             gbase *= 2 );
       for ( bbase = 1; (vinfo->blue_mask & bbase) == 0;
             bbase *= 2 );
/*
    Load the colour table array with the appropriate pixel values and get
    the colour tables.
*/
       for ( nc = 0, pix = 0; pix <= vinfo->red_mask; nc++ ) {
          pix += rbase;
       }
       ctr = (XColor*)malloc( sizeof(XColor) * nc );
       for ( i = 0, pix = 0; pix <= vinfo->red_mask; i++ ) {
          ctr[i].pixel = pix;
          pix += rbase;
       }
       XQueryColors( gwmItemPtr->info->display, gwmItemPtr->info->cmap, ctr, nc );

       for ( nc = 0, pix = 0; pix <= vinfo->green_mask; nc++ ) {
          pix += gbase;
       }
       ctg = (XColor*)malloc( sizeof(XColor) * nc );
       for ( i = 0, pix = 0; pix <= vinfo->green_mask; i++ ) {
          ctg[i].pixel = pix;
          pix += gbase;
       }
       XQueryColors( gwmItemPtr->info->display, gwmItemPtr->info->cmap, ctg, nc );

       for ( nc = 0, pix = 0; pix <= vinfo->blue_mask; nc++ ) {
          pix += bbase;
       }
       ctb = (XColor*)malloc( sizeof(XColor) * nc );
       for ( i = 0, pix = 0; pix <= vinfo->blue_mask; i++ ) {
          ctb[i].pixel = pix;
          pix += bbase;
       }
       XQueryColors( gwmItemPtr->info->display, gwmItemPtr->info->cmap, ctb, nc );

    } else {

/*
   Undecomposed colour model - get colours for all possible pixel values.
*/
       nc =  DisplayCells( gwmItemPtr->info->display,
	   DefaultScreen( gwmItemPtr->info->display ) );
       ct = (XColor*)malloc( sizeof(XColor) * nc );
       for ( i = 0; i < nc; i++ ) ct[i].pixel = i;
       XQueryColors( gwmItemPtr->info->display, gwmItemPtr->info->cmap, ct,
          nc );
    }

/*
  Get a copy of the pixmap.
*/
    image = XGetImage( gwmItemPtr->info->display, gwmItemPtr->info->pix_id,
	0, 0, gwmItemPtr->info->pix_width, gwmItemPtr->info->pix_height,
	AllPlanes, XYPixmap);


/*
  See if display has colour or not.
*/
    if ( gwmItemPtr->info->visclass == StaticColor ||
	gwmItemPtr->info->visclass == TrueColor ||
	gwmItemPtr->info->visclass == PseudoColor ||
	gwmItemPtr->info->visclass == DirectColor )
    {
        colour = 1;
    }
    else
    {
	colour = 0;
    }


/*
  Allocate a buffer with enough space for a line of postscript.
*/
    if ( colour )
	linebuf = (char*)malloc( gwmItemPtr->info->pix_width * 6  + 1);
    else
	linebuf = (char*)malloc( gwmItemPtr->info->pix_width * 2  + 1);

/*
  Write colorimage command.
*/
    sprintf(buffer, "%d %d 8 [ 1 0 0 1 %d %d]",
	gwmItemPtr->info->pix_width, gwmItemPtr->info->pix_height,
	-gwmItemPtr->header.x1, gwmItemPtr->header.y1);
    Tcl_AppendResult(interp, buffer, (char*) NULL);
    sprintf(buffer,
	"{currentfile %d string readhexstring pop}false %d colorimage\n",
	gwmItemPtr->info->pix_width, colour ? 3 : 1);
    Tcl_AppendResult(interp, buffer, (char*) NULL);

/*
  Write image data.
*/
    for (j = gwmItemPtr->info->pix_height - 1; j >= 0; j--)
    {

/*
      For each pixel in the line
*/
        for ( i = 0, k = 0; i < gwmItemPtr->info->pix_width; i++ )
        {
            pix = XGetPixel( image, i, j );
            if (colour)
            {
                if (gwmItemPtr->info->visclass == DirectColor ||
                      gwmItemPtr->info->visclass == TrueColor ) {
                   rpix = (pix & vinfo->red_mask) / rbase;
                   gpix = (pix & vinfo->green_mask) / gbase;
                   bpix = (pix & vinfo->blue_mask) / bbase;
                   linebuf[k++] =
                       "0123456789ABCDEF"[(ctr[rpix].red >> 12) & 0xf];
                   linebuf[k++] =
                       "0123456789ABCDEF"[(ctr[rpix].red >> 8) & 0xf];
                   linebuf[k++] =
                        "0123456789ABCDEF"[(ctg[gpix].green >> 12) & 0xf];
                   linebuf[k++] =
                        "0123456789ABCDEF"[(ctg[gpix].green >> 8) & 0xf];
                   linebuf[k++] =
                       "0123456789ABCDEF"[(ctb[bpix].blue >> 12) & 0xf];
                   linebuf[k++] =
                       "0123456789ABCDEF"[(ctb[bpix].blue >> 8) & 0xf];

                } else {
                   linebuf[k++] =
                       "0123456789ABCDEF"[(ct[pix].red >> 12) & 0xf];
                   linebuf[k++] =
                       "0123456789ABCDEF"[(ct[pix].red >> 8) & 0xf];
                   linebuf[k++] =
                        "0123456789ABCDEF"[(ct[pix].green >> 12) & 0xf];
                   linebuf[k++] =
                        "0123456789ABCDEF"[(ct[pix].green >> 8) & 0xf];
                   linebuf[k++] =
                       "0123456789ABCDEF"[(ct[pix].blue >> 12) & 0xf];
                   linebuf[k++] =
                       "0123456789ABCDEF"[(ct[pix].blue >> 8) & 0xf];
               }
            }
            else
            {
/*
          Convert the pixel value to a B/W intensity.
*/
                if (gwmItemPtr->info->visclass == DirectColor ||
                      gwmItemPtr->info->visclass == TrueColor ) {
                   rpix = (pix & vinfo->red_mask) / rbase;
                   gpix = (pix & vinfo->green_mask) / gbase;
                   bpix = (pix & vinfo->blue_mask) / bbase;
                   inten = (int)( (float)(ctr[rpix].red   >> 8) +
                       (float)(ctg[gpix].green >> 8) +
                       (float)(ctb[bpix].blue  >> 8) ) /3.0;
                } else {
                   inten = (int)( (float)(ct[pix].red   >> 8) +
                       (float)(ct[pix].green >> 8) +
                       (float)(ct[pix].blue  >> 8) ) /3.0;
                }

/*
          Write it to the buffer in Hex
*/
                linebuf[k++] = "0123456789ABCDEF"[(inten >> 4) & 0xf];
                linebuf[k++] = "0123456789ABCDEF"[inten & 0xf];
            }
        }
	linebuf[k] = '\0';
	Tcl_AppendResult(interp, linebuf, " \n", (char*)NULL);
    }

/*
  Free the line buffer etc.
*/
    free( linebuf );
    if (gwmItemPtr->info->visclass == DirectColor ||
        gwmItemPtr->info->visclass == TrueColor ) {
       XFree( vinfo );
       free( ctr );
       free( ctg );
       free( ctb );
    } else {
       free( ct );
    }

    return TCL_OK;
}
