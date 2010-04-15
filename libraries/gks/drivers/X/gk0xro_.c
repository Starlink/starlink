/*
 * RAL GKS SYSTEM
 *
 * gk0xro_:  Raster output device external for the Xlib workstation driver.
 *
 * Type of Routine:  Part of WS DRIVER
 * Author:           TAW
 *
 * Copyright (C) SERC 1988
 *
 * Maintenance Log:
 *
 *  20/09/88  TAW   Dummy routine, needs implementing in Xlib
 *  18/01/89  TAW   Included XLib routines to draw a raster rectangle.
 *  26/10/89  RMK   Removed setting of error 93, and instead ensured
 *                  that index is inside workstation colour table (S350).
 */

#include <wwinfo.h>				/* For ww */
#include "../../system/include/f77_type.h"	/* For FORTRAN type matching */
#include "../../system/include/fa.h"
#include "../../system/include/gkerr.h"		/* For error reporting */
#include "../../system/include/gkdt.h"		/* Needed by ... */
#include "../../system/include/gkwkd.h"		/* WS derived data area */
#include "../../system/include/gkwca.h"		/* WS communication area */
#include "../../system/include/gkwdt.h"		/* WS description table area */
#include "gk0xwd.h"
#include <X11/Xlib.h>
/*
 * Errors:
 *
 *     304 Input/Output error has occurred while sending data to a workstation
 *
 * Comments:
 *
 *  Renders the raster on the ww backing Pixmap using ??.
 *
 *  The raster data is stored in column major order,  but each line of the
 *  raster is stored as a column of the array and is therefore contiguous.
 */

f77_integer gk0xro_(x, y, nxpix, nypix, xdim, raster)
  f77_real    *x,		/* Raster origin X coordinate (In) */
	      *y;		/* Raster origin Y coordinate (In) */
  f77_integer *nxpix,		/* Number of pixels per raster line (In) */
              *nypix,		/* Number of lines in raster (In) */
	      *xdim,		/* X dimension of raster data array (In) */
              *raster;		/* Raster array (In) */
{
  extern GC XDefaultGC();	/* for getting default GC */
  extern int gk0xunportask();

  f77_integer
    b_left,			/* Bounding box left limit */
    b_right,			/* Bounding box right limit */
    b_top,			/* Bounding box top limit */
    b_bottom;			/* Bounding box bottom limit */

  Display
    *display =			/* ww display */
      (Display *)gk0xunportask((void *)ddbm, 040);

  GC LGC;			/* Default GC for ww display */
  int
    wsid = gkywca_.kwkix - 1,	/* Workstation index for C use */
    x0 = (int)*x,		/* X origin in pixrect coordinates */
    y0 =			/* Y origin in pixrect coordinates */
      (int)(gkywdt_.kdsry[wsid] - 1) - (int)*y,
    value,			/* Pixel value */
    line_offset,		/* Offset of raster line from start of array */
    xr, yr;			/* Loop indices */

  /* Set up LGC and the parts with common values for all fill areas */
   if(LGC == (GC)0) LGC = XDefaultGC(display,gk0xunportask((void *)ddbm, 050));
   XSetFunction(display,LGC,GXcopy);
  /*
   * For each line in the raster,  scan along the raster data array column
   * containing the raster line and set the pixels in the ww backing pixmap
   */

  for (line_offset = 0, yr = 0; yr < *nypix; line_offset += *xdim, yr++)
    for (xr = 0; xr < *nxpix; xr++)
    {
      /* Set pixel value, ensuring that it is inside the wkstn colour table */
      value = (int)raster[line_offset+xr] % gkywdt_.kpci[wsid];

      XSetForeground(display, LGC, (unsigned long)gk0xcc_(&value));
      XDrawPoint(display, (Drawable *)gk0xunportask(ddbm,ASKBMMEMORY),LGC,
                 x0 + xr, y0 + yr);
    }

  /* Set up bounding box for screen updating */

  if((b_left = x0) < gkywkd_.kwkdat[wsid][ILEFT])
    gkywkd_.kwkdat[wsid][ILEFT] = (f77_integer)b_left;
  if((b_right = x0 + *nxpix - 1) > gkywkd_.kwkdat[wsid][IRIGHT])
    gkywkd_.kwkdat[wsid][IRIGHT] = (f77_integer)b_right;
  if((b_top = y0) < gkywkd_.kwkdat[wsid][ITOP])
    gkywkd_.kwkdat[wsid][ITOP] = (f77_integer)b_top;
  if((b_bottom = y0 + *nypix - 1) > gkywkd_.kwkdat[wsid][IBOTT])
    gkywkd_.kwkdat[wsid][IBOTT] = (f77_integer)b_bottom;

  return((f77_integer)0);
}
