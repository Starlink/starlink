#if !defined( RTDCANVAS_INCLUDED ) /* Include this file only once */
#define RTDCANVAS_INCLUDED

/* 
 * Public interfaces for local canvas items.
 */

#include <tk.h>

int RtdLineCreate( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item **itemPtr, 
                   int objc, Tcl_Obj *CONST objv[] );

int RtdLineDelete( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display );

void RtdSetLastLineCoords( Tcl_Interp *interp, const double *x, 
                           const double *y, int numPoints );

void RtdQuickSetLineCoords( Tcl_Interp *interp, Tk_Canvas canvas, 
                            Tk_Item *itemPtr, const double *x, 
                            const double *y, int numPoints );

void RtdWordLastBBox( double *xb, double *yp );

void RtdSegmentSetCoords( Tcl_Interp *interp, int append,
                          const double *x, const double *y,
                          int numPoints );
#endif
