/*
 * RAL GKS SYSTEM
 *
 * gk9sln_:  Outputs a polyline with width and style.
 *
 * Type of Routine:  Part of WORKSTATION DRIVER
 * Author:           TAW
 *
 * Copyright (C) SERC 1987
 *
 * Maintenance Log:
 *
 *  11/08/87  TAW   Version 1 stabilised.
 *  14/10/87  PJWR  Added solid linestyle and extended documentation.
 *  16/10/87  PJWR  Modified to pick up line width and style from
 *                  workstation workspace area rather than derived data area.
 *                  Changed name from gk9spl_ to gk9sln_.
 *  03/11/87  PJWR  Display updates now done by main driver.  Code changed to
 *                  leave boundaries in WS workspace area rather than calling
 *                  notescreen().
*  03/02/88  TAW   Changed PIX_SET to PIX_SRC  in pr_polyline call.


 */

#include "./varinc/wwinfo.h"				/* For ww */
#include <pixrect/pixrect_hs.h>			/* For pixrect routines */
#include "../../system/include/f77_type.h"	/* For type matching */
#include "../../system/include/gkdt.h"		/* Needed by following ... */
#include "../../system/include/gkwkd.h"		/* For WS derived data */
#include "../../system/include/gkwca.h"		/* For WS communication area */
#include "../../system/include/gkerr.h"		/* For error reporting */
#include "../../system/include/gkwdt.h"		/* For WDT access */
#include "gk9swd.h"				/* For WS workspace offsets */

/*
 * Errors:
 *
 *    300  Storage overflow has occurred in GKS
 *    304  Input/Output error has occurred while sending data to a workstation
 *
 * Comments:
 *
 *  Uses pr_polyline to render the polyline on the pixrect ww maintains to
 *  back the window.
 *
 *  The brush and texture structures for pr_polyine are cached and only
 *  updated when the line width or line style are changed.
 *
 *  The array of shorts used to hold the cached line pattern ('cached_pattern')
 *  must be large enough to hold the largest pattern defined.
 */
Pr_texture      *style = (Pr_texture *)0;	/* Which style is cached  */
Pr_brush 	brush = {0};
int cached_style = 1;

f77_integer gk9sln_(npts, x, y)
  f77_integer *npts;		/* Number of coordinate pairs (In) */
  f77_real    *x,		/* X coordinates (In) */
	      *y;		/* Y coordinates (In) */
{
static short
    dashed[] = 			/* Broken line patterns,  similar to Sigmex */
      {4,4,0},
    dotted[] =
      {1,3,0},
    dash_dot[] =
      {4,3,1,3,0},
    dash_dot_dot[] =
      {4,3,1,3,1,3,0};

  static short
    *patterns[] =		/* Array of references to the patterns above */
    {
      dashed, dotted, dash_dot, dash_dot_dot
    };

  static short			/* Cached line pattern (see header comments)*/
    cached_pattern[7];

  static Pr_texture
    texture =			/* Texture cache for broken line styles */
    {
	cached_pattern, 0,
	1, 1, 0, 0,   0, 0, 0, 0, 0,
	0, 0, 0
    };

  extern char
    *malloc();			/* For allocating pixrect coordinate array */

  struct pr_pos
    *points;			/* Pointer to pixrect coordinates */

  f77_integer
    b_left,			/* Changed display area left bound */
    b_right,			/* Changed display area right bound */
    b_top,			/* Changed display area top bound */
    b_bottom;			/* Changed display area bottom bound */

  int
    wkix = gkywca_.kwkix - 1,	/* Local copy of workstation index */
    y_max =			/* Maximum Y coordinate */
      gkywdt_.kdsry[wkix] - 1,
    new_style =			/* Line style to be used */
      gkywkd_.kwkdat[wkix][ILNTY],
    new_width =			/* Line width to be used */
      gkywkd_.kwkdat[wkix][ILNWD],
    status,			/* Return value of pr_polyline() */
    i;				/* Loop index */

  /* Allocate memory for pixrect coordinates */

  points = (struct pr_pos *)malloc((unsigned)(*npts * sizeof(struct pr_pos)));

  /* Continue if successful,  otherwise return error 300 */

  if(points != (struct pr_pos *)0)
  {
    /*
     * If either the line width or line style have changed,  update the brush
     * and texture structures
     */

    if(new_style != cached_style || new_width != brush.width)
    {
      /* First update the texture pattern */

      if(new_style == 1)

	/* Set the style pointer to NULL for solid lines */

	style = (Pr_texture *)0;
      else
      {
	/* Load the style and multiply segment lengths by the line width */

	for(i = 0; patterns[new_style - 2][i] != 0; i++)
	  texture.pattern[i] = patterns[new_style - 2][i] * new_width;
	texture.pattern[i] = 0;

	/* Set the style pointer to the cache address */

	style = &texture;
      }
      cached_style = new_style;
      /* Now update the brush */

      brush.width = new_width;
    }

    /*
     * Truncate GKS DC coordinates into pixrect raster coordinates and
     * calculate bounding box of polyline for screen updating
     */

    b_top  = b_bottom = points[0].y = y_max - (int)y[0];
    b_left = b_right  = points[0].x = (int)x[0];

    for (i = 1; i < *npts; i++)
    {
      if((points[i].x = (int)x[i]) < b_left)
	b_left = points[i].x;
      else if(points[i].x > b_right)
	b_right = points[i].x;
      if((points[i].y = y_max - (int)y[i]) < b_top)
	b_top = points[i].y;
      else if(points[i].y > b_bottom)
	b_bottom = points[i].y;
    }

    /* The bounding box must be enlarged if thick lines are used */

    if(brush.width > 1)
    {
      b_top    -= brush.width / 2;
      b_bottom += brush.width / 2;
      b_left   -= brush.width / 2;
      b_right  += brush.width / 2;
    }

    /* Render the polyine */

    status = pr_polyline((struct pixrect *)unportask((void *)ddbm, ASKBMMEMORY),
			  0,0, (int)*npts, points, POLY_DONTCLOSE, &brush,
			  style, PIX_SRC  | PIX_COLOR(dd->d_fore)
			 );
    if(status != PIX_ERR)
    {
      /* Set up the update area in the workstation workspace */

      if(b_left < gkywkd_.kwkdat[wkix][ILEFT])
	gkywkd_.kwkdat[wkix][ILEFT] = (f77_integer)b_left;
      if(b_right > gkywkd_.kwkdat[wkix][IRIGHT])
	gkywkd_.kwkdat[wkix][IRIGHT] = (f77_integer)b_right;
      if(b_top < gkywkd_.kwkdat[wkix][ITOP])
	gkywkd_.kwkdat[wkix][ITOP] = (f77_integer)b_top;
      if(b_bottom > gkywkd_.kwkdat[wkix][IBOTT])
	gkywkd_.kwkdat[wkix][IBOTT] = (f77_integer)b_bottom;
    }
    else
      gkyerr_.kerror = 304;

    /* Free the pixrect coordinate memory */

    (void)free((char *)points);
  }
  else
    gkyerr_.kerror = 300;

    return((f77_integer)0);
}
