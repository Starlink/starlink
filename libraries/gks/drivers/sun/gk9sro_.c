/*
 * RAL GKS SYSTEM
 *
 * gk9sro_:  Raster output device external for the Sun workstation driver.
 *
 * Type of Routine:  Part of WS DRIVER
 * Author:           TAW
 *
 * Copyright (C) SERC 1987
 *
 * Maintenance Log:
 *
 *  ??/??/87  TAW   Created.
 *  06/05/87  PJWR  Corrected to use GKS drawing area of bitmap rather than
 *                  the entire bitmap.
 *  14/07/87  PJWR  IS: Changed error numbers.
 *  04/11/87  PJWR  Converted to leave area to update in workstation workspace
 *                  area and reformatted to usual layout.
 *  05/11/87  PJWR  Release version stabilised.
 *  30/08/89  RMK   Removed test on colour index and setting of error 93 -
 *                  gk9scc now ensures that value is within colour table (S350).
 */

#include <pixrect/pixrect_hs.h>			/* For pixrect routines */
#include "../../drivers/sun/varinc/wwinfo.h"				/* For ww */
#include "../../system/include/f77_type.h"	/* For FORTRAN type matching */
#include "../../system/include/fa.h"
#include "../../system/include/gkerr.h"		/* For error reporting */
#include "../../system/include/gkdt.h"		/* Needed by ... */
#include "../../system/include/gkwkd.h"		/* WS derived data area */
#include "../../system/include/gkwca.h"		/* WS communication area */
#include "../../system/include/gkwdt.h"		/* WS description table area */
#include "../../drivers/sun/gk9swd.h"

/*
 * Errors:
 *
 *     304 Input/Output error has occurred while sending data to a workstation
 *
 * Comments:
 *
 *  Renders the raster on the ww backing pixrect using pr_put.
 *
 *  The raster data is stored in column major order,  but each line of the
 *  raster is stored as a column of the array and is therefore contiguous.
 */

f77_integer gk9sro_(x, y, nxpix, nypix, xdim, raster)
  f77_real    *x,		/* Raster origin X coordinate (In) */
	      *y;		/* Raster origin Y coordinate (In) */
  f77_integer *nxpix,		/* Number of pixels per raster line (In) */
              *nypix,		/* Number of lines in raster (In) */
	      *xdim,		/* X dimension of raster data array (In) */
              *raster;		/* Raster array (In) */
{
  f77_integer
    b_left,			/* Bounding box left limit */
    b_right,			/* Bounding box right limit */
    b_top,			/* Bounding box top limit */
    b_bottom;			/* Bounding box bottom limit */

  struct pixrect
    *display =			/* ww backing pixrect */
      (struct pixrect *)unportask((void *)ddbm, ASKBMMEMORY);

  int
    wsid = gkywca_.kwkix - 1,	/* Workstation index for C use */
    x0 = (int)*x,		/* X origin in pixrect coordinates */
    y0 =			/* Y origin in pixrect coordinates */
      (int)(gkywdt_.kdsry[wsid] - 1) - (int)*y,
    value,			/* Pixel value */
    line_offset,		/* Offset of raster line from start of array */
    xr, yr;			/* Loop indices */

  /*
   * For each line in the raster,  scan along the raster data array column
   * containing the raster line and set the pixels in the ww backing pixrect
   */

  for (line_offset = 0, yr = 0; yr < *nypix; line_offset += *xdim, yr++)
    for (xr = 0; xr < *nxpix; xr++)
    {
      value = (int)gk9scc_(&raster[line_offset+xr]);
      if(pr_put(display, x0 + xr, y0 + yr, value) == PIX_ERR)
      {
        /* Couldn't access pixel */

        gkyerr_.kerror = 304;
        return((f77_integer)0);
      }
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
