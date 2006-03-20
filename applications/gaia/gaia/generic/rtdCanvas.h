#if !defined( RTDCANVAS_INCLUDED ) /* Include this file only once */
#define RTDCANVAS_INCLUDED

/*
 * Public interfaces for local canvas items.
 */

#include <tk.h>

/* rtd_polyline */
int RtdLineCreate( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item **itemPtr,
                   int objc, Tcl_Obj *CONST objv[] );

int RtdLineDelete( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display );

void RtdLineDisplay( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display,
                      Drawable dst, int x, int y, int width, int height );

void RtdLineSetLastCoords( Tcl_Interp *interp, const double *x,
                           const double *y, int numPoints );

void RtdLineQuickSetCoords( Tcl_Interp *interp, Tk_Canvas canvas,
                            Tk_Item *itemPtr, const double *x,
                            const double *y, int numPoints );

void RtdLineQuickSetCoords( Tcl_Interp *interp, Tk_Canvas canvas,
                            Tk_Item *itemPtr, const double *x,
                            const double *y, int numPoints );

void RtdLineSetColour( Display *display, Tk_Item *itemPtr, XColor *colour );

void RtdLineSetWidth( Display *display, Tk_Item *itemPtr, int width );

int RtdLineToPostscript( Tcl_Interp *interp, Tk_Canvas canvas,
                         Tk_Item *itemPtr, int prepass );

/* rtd_word */
void RtdWordLastBBox( double *xb, double *yp );

/* rtd_segment */
void RtdSegmentSetCoords( Tcl_Interp *interp, int append,
                          const double *x, const double *y,
                          int numPoints );
#endif
