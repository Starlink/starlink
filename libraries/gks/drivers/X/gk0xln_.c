/*
 * RAL GKS SYSTEM
 *
 * gk0xln_:  Outputs a polyline with width and style.
 *
 * Type of Routine:  Part of WORKSTATION DRIVER
 * Author:           TAW
 *
 * Copyright (C) SERC 1988
 *
 * Maintenance Log:
 *
 *  	20/09/88	TAW	Copied from ../sun/gk9sln_.c and modified for XLib.
 *      13/01/88	TAW	Added XSetFillStyle = Fill Solid as it should be for all lines.
 */

#include <wwinfo.h>				/* For ww */
#include "../../system/include/f77_type.h"	/* For type matching */
#include "../../system/include/gkdt.h"		/* Needed by following ... */
#include "../../system/include/gkwkd.h"		/* For WS derived data */
#include "../../system/include/gkwca.h"		/* For WS communication area */
#include "../../system/include/gkerr.h"		/* For error reporting */
#include "../../system/include/gkwdt.h"		/* For WDT access */
#include "gk0xwd.h"				/* For WS workspace offsets */
#include <X11/Xlib.h>

/*
 * Errors:
 *
 *    300  Storage overflow has occurred in GKS
 *    304  Input/Output error has occurred while sending data to a workstation
 *
 * Comments:
 *
 *  Uses XDrawLine to render the polyline on the Pixmap ww maintains to
 *  back the window.
 *
 *  The brush and texture structures for the GC LGC are cached and only
 *  updated when the line width or line style are changed.
 *
 *  The array of shorts used to hold the cached line pattern ('cached_pattern')
 *  must be large enough to hold the largest pattern defined.
 */

f77_integer gk0xln_(npts, x, y)
  f77_integer *npts;		/* Number of coordinate pairs (In) */
  f77_real    *x,		/* X coordinates (In) */
	      *y;		/* Y coordinates (In) */
{
  static int
    cached_style = 1;		/* Which style is cached (initially SOLID) */

  struct pattern {
     int NumOnOff;
     char OnOff[6];
     };
  static struct pattern
    dashed = 			/* Broken line patterns,  similar to Sigmex */
      {2, {4,4,0,0,0,0} },
    dotted =
      {2, {1,3,0,0,0,0} },
    dash_dot =
      {4, {4,3,1,3,0,0} },
    dash_dot_dot =
      {6, {4,3,1,3,1,3} };

  static struct pattern
    *patterns[] =		/* Array of references to the patterns above */
    {
      &dashed, &dotted, &dash_dot, &dash_dot_dot
    };

  static struct pattern			/* Cached line pattern (see header comments)*/
    cached_pattern;


  static unsigned int
    brush = 1;		/* Width cache */

  static Display *display;	/* Current default display to draw on */
  static GC LGC;		/* The GC to setup for drawing */

  extern char
    *malloc();			/* For allocating pixrect coordinate array */

  extern GC XDefaultGC();	/* for getting default GC */

  XPoint *points;		/* Pointer to X coordinates */

  static int style = LineSolid;	/* X Line style */

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
    i;				/* Loop index */

  /* Get default display from ddbm */
  display = (Display *)gk0xunportask(ddbm,040);

  /* Allocate memory for X coordinates */
  points = (XPoint *)malloc((unsigned)(*npts * sizeof(XPoint)));

  /* Continue if successful,  otherwise return error 300 */

  if(points != (XPoint *)0)
  {
    /*
     * If either the line width or line style have changed,  update the brush
     * and cached_pattern structures
     */

    if(new_style != cached_style || new_width != brush)
    {
      /* First update the texture pattern */

      if(new_style == 1)

	/* Set the style for solid lines */

	style = LineSolid;
      else
      {
	/* Set style and multiply segment lengths by the line width */

	style = LineOnOffDash;

	cached_pattern.NumOnOff = patterns[new_style - 2]->NumOnOff;
	for(i = 0; patterns[new_style - 2]->NumOnOff != i; i++)
	  cached_pattern.OnOff[i] = patterns[new_style - 2]->OnOff[i] * new_width;
      }

      /* Update the cached_style */

      cached_style = new_style;

      /* Now update the brush */

      if (new_width == 1) brush = 0;
      else brush = new_width;
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

    if(brush > 1)
    {
      b_top    -= brush / 2;
      b_bottom += brush / 2;
      b_left   -= brush / 2;
      b_right  += brush / 2;
    }

    /* Set up the GC for the polyline */
    if(LGC == (GC)0) LGC = XDefaultGC(display,gk0xunportask(ddbm,050));
    if(DefaultVisual(display,DefaultScreen(display))->class==PseudoColor)
      XSetFunction(display,LGC,GXcopy);
    else
      XSetFunction(display,LGC,GXset);
    XSetForeground(display,LGC,dd->d_fore);
    XSetLineAttributes(display,LGC,brush,style,CapButt,JoinMiter);
    XSetFillStyle(display, LGC, FillSolid);
    if(cached_style != 1) XSetDashes(display,LGC,1,cached_pattern.OnOff,cached_pattern.NumOnOff);

    /* Render the polyine */

    XDrawLines(display, (Drawable *)gk0xunportask(ddbm,ASKBMMEMORY), LGC,
               points, *npts, CoordModeOrigin);

    /* Set up the update area in the workstation workspace */

    if(b_left < gkywkd_.kwkdat[wkix][ILEFT])
	gkywkd_.kwkdat[wkix][ILEFT] = (f77_integer)b_left;
    if(b_right > gkywkd_.kwkdat[wkix][IRIGHT])
	gkywkd_.kwkdat[wkix][IRIGHT] = (f77_integer)b_right;
    if(b_top < gkywkd_.kwkdat[wkix][ITOP])
	gkywkd_.kwkdat[wkix][ITOP] = (f77_integer)b_top;
    if(b_bottom > gkywkd_.kwkdat[wkix][IBOTT])
	gkywkd_.kwkdat[wkix][IBOTT] = (f77_integer)b_bottom;

    /* Free the X coordinate memory */

    (void)free((char *)points);
  }
  else
    gkyerr_.kerror = 300;

    return((f77_integer)0);
}
