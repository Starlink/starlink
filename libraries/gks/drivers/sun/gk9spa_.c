/*
 * RAL GKS SYSTEM
 *
 * gk9spa_: Performs INQUIRE PIXEL ARRAY for the Sun WS driver.
 *
 * Type of Routine:  Part of WS DRIVER
 * Author:           PJWR
 *
 * Copyright (C) SERC 1987
 *
 * Maintenance Log:
 *
 *  20/01/87  PJWR  Created.
 *  13/02/87  PJWR  Modified to correct improper positioning of the valid pixel
 *                  data in the returned array.
 *  18/02/87  PJWR  Improved efficiency of test for invalid regions when
 *                  returning data.
 *  02/04/87  PJWR  Created version 1.0.
 *  06/05/87  PJWR  Corrected to use GKS drawing area of bitmap rather than
 *                  the entire bitmap.
 *  16/07/87  PJWR  IS:  Changed workstation communication data used for pixel
 *                  array size, output array details and invalid values flag.
 */

#include "./varinc/wwinfo.h"
#include <pixrect/pixrect_hs.h>
#include "../../system/include/f77_type.h"
#include "../../system/include/fa.h"
#include "../../system/include/gks.h"
#include "../../system/include/gkdt.h"
#include "../../system/include/gkerr.h"
#include "../../system/include/gkwca.h"
#include "../../system/include/gkwdt.h"
#include "../../system/include/gkwkd.h"
#include "../../system/include/gkwsl.h"

/*
 * Comments:
 *
 *  This routine will require modification when NeWS appears,  as it uses
 *  pixrects.
 */

f77_integer gk9spa_(x0, y0, data)
  f77_integer *x0;		/* X coord. of TLC of pixel array (Input) */
  f77_integer *y0;		/* Y coord. of TLC of pixel array (Input) */
  f77_integer **data;		/* Pixel array (Output) */
{
  box
    inquired,			/* The box corresponding to the pixel array */
    given;			/* The portion of 'inquired' that's legal */

  struct pixrect
    *surface;			/* The display surface pixrect */

  int
    i, j,			/* Indices for 'data' */
    x, y;			/* indices for '*surface' */

  /* Construct box defining inquired pixel array. */

  inquired.b_left = *x0;
  inquired.b_top = gkywdt_.kdsry[gkywca_.kwkix - 1] - 1 - *y0;
  inquired.b_right = inquired.b_left + gkywca_.kwi5 - 1;
  inquired.b_bottom = inquired.b_top + gkywca_.kwi6 - 1;

  /* Obtain intersection of inquired pixel array and display surface. */

  given = boxop(inquired, ddbm->bm_box, BOXINTERSECT);

  /* Intersection != inquired area => invalid values */

  if(given.b_left != inquired.b_left
  || given.b_top != inquired.b_top
  || given.b_right != inquired.b_right
  || given.b_bottom != inquired.b_bottom
  )
    gkywca_.kwi7 = GPRSNT;
  else
    gkywca_.kwi7 = GABSNT;

  /* Get pointer to pixrect representing display surface */

  surface = (struct pixrect *)unportask((void *)ddbm, ASKBMMEMORY);

  /* Transfer values from pixrect to 'data',  filling invalid regions -1 */

  i = gkywca_.kwi3 - 1;
  j = gkywca_.kwi4 - 1;
  for(x = inquired.b_left; x <= inquired.b_right; x++)
    for(y = inquired.b_top; y <= inquired.b_bottom; y++)
    {
      if((gkywca_.kwi7 == GPRSNT) &&  (x < given.b_left
				    || x > given.b_right
				    || y < given.b_top
				    || y > given.b_bottom)
      )
	((f77_integer *)data)[i + (j * gkywca_.kwi1)] = -1;
      else
	((f77_integer *)data)[i + (j * gkywca_.kwi1)] = pr_get(surface, x, y);
      i += 1;
      j += 1;
    }

  return((f77_integer)0);
}
