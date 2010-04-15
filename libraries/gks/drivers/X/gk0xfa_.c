/*
 * RAL GKS SYSTEM
 *
 * gk0xfa_:  Performs fill area for the Xlib workstation driver.
 *
 * Type of Routine:  Part of WORKSTATION DRIVER
 * Author:           TAW
 *
 * Copyright (C) SERC 1989
 *
 * Maintenance Log:
 *
 *  10/08/88  TAW   Copied from Sun driver (../sun/gk0xfa_.c) but not yet
 *		    modified.
 *  11/01/89  TAW   Modified for XLib.
 *  26/01/89  TAW   Worked round a bug in colour patterns.
 *  16/02/89  TAW   Changed XSetFunction from GXor or GXset to GXCopy
 */

#include <wwinfo.h>				/* For ww */
#include "../../system/include/f77_type.h"	/* For FORTRAN type matching */
#include "../../system/include/gks.h"		/* For style types */
#include "../../system/include/gkerr.h"		/* For error reporting */
#include "../../system/include/gkhp.h"		/* For GKS heap access */
#include "../../system/include/gkstk.h"		/* For GKS stack access */
#include "../../system/include/gkdt.h"		/* Needed by ... */
#include "../../system/include/gkwca.h"		/* For WS communication area */
#include "../../system/include/gkwdt.h"		/* For WS description area */
#include "../../system/include/gkwkd.h"		/* For WS derived data area */
#include "../../system/include/gkwsl.h"		/* For WS state list area */
#include "gk0xwd.h"				/* For WS workspace offsets */
#include <X11/Xlib.h>
#include <math.h>

/*
 * Types:
 *
 * Hatch_desc - holds the description of a hatch element
 */

typedef struct
{
  int
    width,		/* Width of hatch element in pixels */
    height;		/* Height of hatch element in pixels */
  Pixmap
    image;		/* Image data  */
  unsigned int numsegs; /* number of line segments in hatch */
  XSegment *ends;	/* Line segment end points */
}
Hatch_desc;

/*
 * Algorithm:
 *
 *   [1] Transform boundary of area to be filled from WC to DC.
 *   [2] Clip boundary of area to be filled.  This will result in one  or  more
 *       potentially  self  intersecting  boundaries  describing  the  disjoint
 *       area(s) to be filled.
 *   [3] Translate the boundary coordinates from DC to pixrect coordinates  and
 *       calculate the bounding box of the primitive.
 *   [4] Render the area(s) to be filled according to  the  interior  style  as
 *       follows:
 *
 *       HOLLOW areas have their boundaries drawn as 1 pixel  wide  vectors  in
 *         the current fill area colour using XDrawLine().
 *       SOLID areas are  filled  with  the  current  fill  area  colour  using
 *         XPolygonFill().
 *       PATTERN areas are filled using a texture pixrect and ???.
 *         The pattern pixrect is  generated  by  creating  a  pattern  element
 *         pixrect from the pattern description stored  on  the  GKS  heap  and
 *         replicating this onto a pixrect the size of the  primitive  bounding
 *         box.
 *       HATCH areas are filled using an hatches image Pixmap. The
 *         hatch Pixmap is created by forcing the  current  fill  area  colour
 *         through a stencil formed from a replicated hatch element onto a copy
 *         of the current screen contents of the primitive bounding box.
 *
 * Errors:
 *
 *  -1016  Internal error detected within GKS
 *    304  Input/Output error while sending data to a workstation
 *
 * Comments:
 *
 *  This routine uses the RAL GKS stack allocation routines for FORTRAN routine
 *  workspace and malloc() for C routine workspace.
 *
 *  The ww foreground colour,  dd->d_fore,  must be set to the fill area colour
 *  index and the ww default bitmap pointer ddbm must be set to the appropriate
 *  workstation bitmap before this routine is called.
 */

int rem(value, divisor)
int value, divisor;
{
int tmp;
tmp = value;
while(tmp>=divisor) tmp-=divisor;
return(tmp);
}

f77_integer gk0xfa_(nc, xwc, ywc)
  f77_integer *nc;		/* Number of vertices (In) */
  f77_real    *xwc,		/* Vertex X world coordinates (In) */
	      *ywc;		/* Vertex Y world coordinates (In) */
{

#include "hatches.h"		/* Hatch element image data declarations */

  static Hatch_desc
    hatches[] = 		/* Hatch description lookup table */
    {
      { 64, 64, 0,  8, hatch_1 }, { 64, 64, 0,  8, hatch_2 },
      { 60, 60, 0, 10, hatch_3 }, { 60, 60, 0, 10, hatch_4 },
      { 64, 64, 0, 16, hatch_5 }, { 60, 60, 0, 20, hatch_6 },
      { 48, 56, 0, 14, hatch_7 }, { 48, 48, 0, 12, hatch_8 },
      { 44, 44, 0, 16, hatch_9 }, { 48, 56, 0, 14, hatch_10 }
    };

  extern f77_integer
    gkstal_(),			/* For allocating GKS stack */
    gkstda_(),			/* For releasing GKS stack */
    gkdrge_(),			/* For getting pattern directory */
    gkhpgi_(),			/* For getting pattern data */
    gkpclp_();			/* For clipping the area to be filled */

  extern char
    *malloc();			/* For allocating C workspace */
  extern GC XDefaultGC();	/* for getting default GC */
  extern int gk0xunportask();
  f77_integer
    nbnds,			/* Number of boundaries to/from gkpclp_() */
    type,			/* Data type for GKS stack routines */
    size,			/* Allocation size for GKS routines */
    three = 3,			/* Data count for GKS heap access */
    zero = 0,			/* Data count for GKS heap access */
    istack,			/* GKS stack integer workspace offset */
    rstack,			/* GKS stack real workspace offset */
    *lastvertex,		/* Last vertex index list address */
    pat_desc[3],		/* Pattern description */
    *pat_data;			/* Pattern data address */

  f77_real
    dummy[1],			/* Dummy array for gkdrge_() */
    *xdc_unclipped,		/* Address of unclipped vertex X coordinates */
    *ydc_unclipped,		/* Address of unclipped vertex Y coordinates */
    *xdc_clipped,		/* Address of clipped vertex X coordinates */
    *ydc_clipped;		/* Address of clipped vertex Y coordinates */

  XPoint
    *vlist;			/* Pixmap coordinates of area to be filled */

  static GC LGC;		/* The GC to setup for drawing */
  static GC HatchGC;		/* The GC to setup for drawing on
  				   the hatch element*/
  Display
    *display = 			/* ww backing display */
      (Display *)gk0xunportask((void *)ddbm, 040);
   Pixmap element;			/* Hatch element */

  int
    b_left,			/* Primitive bounding box left coordinate */
    b_right,			/* Primitive bounding box right coordinate */
    b_top,			/* Primitive bounding box top coordinate */
    b_bottom,			/* Primitive bounding box bottom coordinate */
    p_width,			/* Pattern width */
    p_height,			/* Pattern height */
    ws =			/* C version of workstation index */
      (int)gkywca_.kwkix - 1,
    y_max = 			/* Display height for coordinate inversion */
      (int)gkywdt_.kdsry[ws] - 1,
    status,			/* Return status of pixrect routines */
    *npts,			/* List of boundary vertex counts */
    i, j,			/* Loop indices / temporary array indices */
    tileheight;			/* Vertical height of tile in pixels */

  char
    *memory;			/* Address of C workspace */

  /*
   * Allocate FORTRAN workspace for transformation and clipping.  This is
   * done as one real allocation and one integer allocation,  with subarray
   * addresses calculated after allocation.  The total workspace required is:
   *   nc * 2 reals for transformations
   *   nc * 4 reals for clipped vertices
   *   nc / 2 integers for last vertex list created by clipping
   */

  type = KREALS; size = *nc * 6;
  (void)gkstal_(&type, &size, &rstack);
  if(gkyerr_.kerror != 0)
    return((f77_integer)0);
  xdc_unclipped = &gkystk_.stk.qstack[rstack-1];
  ydc_unclipped = xdc_unclipped + *nc;
  xdc_clipped = ydc_unclipped + *nc;
  ydc_clipped = xdc_clipped + (*nc * 2);

  type = KINTGS; size = *nc / 2;
  (void)gkstal_(&type, &size, &istack);
  if(gkyerr_.kerror != 0)
    return((f77_integer)0);
  lastvertex = &gkystk_.stk.kstack[istack-1];

  /* Transform vertices to DC */

  (void)gktwd_(nc, xwc, ywc, xdc_unclipped, ydc_unclipped);
  if(gkyerr_.kerror != 0) return((f77_integer)0);

  /*
   * Clip the area to be filled.  We start with one boundary with 'nc' vertices
   * and finish with 'nbnds' boundaries with the index of the last vertex of
   * each boundary 'i' (0 <= i < nbnds) in lastvertex[i].
   */

  nbnds = 1;
  lastvertex[0] = *nc;
  (void)gkpclp_(&nbnds, nc, lastvertex, xdc_unclipped, ydc_unclipped,
		&gkywkd_.qwclxl[ws], &gkywkd_.qwclxr[ws],
		&gkywkd_.qwclyb[ws], &gkywkd_.qwclyt[ws],
		xdc_clipped, ydc_clipped
	       );
  if(gkyerr_.kerror != 0) return((f77_integer)0);

  /* Allocate C workspace for boundary vertex counts and pixmap coordinates */

  memory = malloc((unsigned)(nbnds * sizeof(int) +
			     lastvertex[nbnds-1] * sizeof(XPoint)
			    )
		 );

  npts = (int *)memory;
  vlist = (XPoint *)(memory + nbnds * sizeof(int));

  /* Translate last vertex list into a list of boundary vertex counts */

  for(i = nbnds - 1; i > 0; i--)
    npts[i] = lastvertex[i] - lastvertex[i-1];
  npts[0] = lastvertex[0];

  /*
   * Truncate the vertex coordinates and put them into Xpoint form,
   * calculating the bounding box of the primitive in the process.
   */

  b_top  = b_bottom = vlist[0].y = y_max - (int)ydc_clipped[0];
  b_left = b_right  = vlist[0].x = (int)xdc_clipped[0];

  for (i = 1; i < lastvertex[nbnds-1]; i++)
  {
    if((vlist[i].x = (int)xdc_clipped[i]) < b_left)
      b_left = vlist[i].x;
    else if(vlist[i].x > b_right)
      b_right = vlist[i].x;
    if((vlist[i].y = y_max - (int)ydc_clipped[i]) < b_top)
      b_top = vlist[i].y;
    else if(vlist[i].y > b_bottom)
      b_bottom = vlist[i].y;
  }

  /* Set up LGC and the parts with common values for all fill areas */
    if(LGC == (GC)0) LGC = XDefaultGC(display,gk0xunportask(ddbm,050));
      XSetFunction(display,LGC,GXcopy);

  /* Render the polygon as specified by the interior style */

  if(gkywkd_.kwfais[ws] == GHOLLO)
   {
    /*
     * Draw the boundary of each disjoint area in the set returned by the
     * clipping routine
     */

    XSetForeground(display,LGC,dd->d_fore);
    XSetLineAttributes(display,LGC,1,LineSolid,CapButt,JoinMiter);
    XSetFillStyle(display,LGC,FillSolid);

    for(i = 0; i < nbnds; i++)
    {
      XDrawLines(display, (Drawable *)gk0xunportask(ddbm,ASKBMMEMORY),
                          LGC, vlist, npts[i], CoordModeOrigin);
      XDrawLine(display, (Drawable *)gk0xunportask(ddbm,ASKBMMEMORY),
                LGC, vlist->x, vlist->y,
                (vlist+npts[i]-1)->x, (vlist+npts[i]-1)->y);
      vlist += npts[i];
    }
   }
  else
  {
   if(gkywkd_.kwfais[ws] == GSOLID)
    {
      /*
       * Set up rest of LGC with fill_style = FillSolid, foreground =
       * fill area colour index for XFillPolygon.
       */

    XSetForeground(display,LGC,dd->d_fore);
    XSetFillStyle(display,LGC,FillSolid);
    }
    else
    {
      /*
       * Pattern or hatch interior style.
       */

      /* Deal with each style */

      if(gkywkd_.kwfais[ws] == GPATTR)
      {
	/*
	 * Obtain the pattern description from the pattern directory. The
	 * pattern width and height will be in pat_desc[0] and pat_desc[1]
	 * and the heap index for the pattern element data in pat_desc[2]
	 */

	(void)gkdrge_(&gkywsl_.kpabpt[ws], &gkywkd_.kwfasi[ws], &three, &zero,
		      pat_desc, dummy
		     );
	if(gkyerr_.kerror != 0) return((f77_integer)0);

	/*
	 * Free current GKS integer stack allocation and allocate space for
	 * pattern data.
	 */

	p_width = (int)pat_desc[0];
	p_height = (int)pat_desc[1];
	type = KINTGS; size = p_width * p_height;
	(void)gkstda_(&type,&istack);
	if(gkyerr_.kerror != 0) return((f77_integer)0);
	(void)gkstal_(&type,&size,&istack);
	if(gkyerr_.kerror != 0) return((f77_integer)0);
	pat_data = &gkystk_.stk.kstack[istack-1];

	/* Get pattern data from GKS heap */

	(void)gkhpgi_(&pat_desc[2], &zero, &size, pat_data);

	/*
	 * Create a Pixmap representing the pattern element. Note that
	 * pattern data is in row major memory order
	 */

	element = XCreatePixmap(display, (Drawable *)gk0xunportask(ddbm,ASKBMMEMORY),
	                        p_width, p_height,
			        (gkywdt_.kpci[ws] > 2) ? 8 : 1
			        );
	if(element == ( Pixmap)0)
	{
	  gkyerr_.kerror = -1016;
	  return((f77_integer)0);
	}

        XSetFunction(display, LGC, GXcopy);

         /* This is what should happen for both colour and monochrome patterns but tiles set up in colour
            seem to get the first 4 elements of each row shifted up on row so we have to compensate for
            this by shifting them back down. There must be a good reason for this but I cant find it.
            Patterns for colour workstations need to be multiples of 4 wide I think. TAW
         *
         *for(i = 0; i < p_width; i++)
	 * for(j = 0; j < p_height; j++)
	 * {
	 *   XSetForeground(display, LGC,
	 *                  (unsigned long)gk0xcc_(&pat_data[j*p_width+i]));
         *   XDrawPoint(display, element, LGC, i, j);
	 * }
	 */
	if((gkywdt_.kpci[ws] > 2) && (display->release==11)
	   && (display->vnumber<3))
	 /* Pattern on a colour workstation  before XV11R3 had a bug */
	 for(i = 0; i < p_width; i++)
	  for(j = 0; j < p_height; j++)
	  {
	    XSetForeground(display, LGC,
	                   (unsigned long)gk0xcc_(&pat_data[j*p_width+i])
	                  );
	    if(i<4) XDrawPoint(display, element, LGC, i, rem(j+1,p_width));
	    else XDrawPoint(display, element, LGC, i, j);
	  }
	else /* Pattern on a monochrome workstation */
	  for(i = 0; i < p_width; i++)
	  for(j = 0; j < p_height; j++)
	  {
	    XSetForeground(display, LGC,
	                   (unsigned long)gk0xcc_(&pat_data[j*p_width+i])
	                  );
	    XDrawPoint(display, element, LGC, i, j);
	  }

	/*
	 * Set up element as tile in LGC for XFillPolygon and set up
	 * Tiling origin from GKS pattern origin.
	 */

        XSetTile(display, LGC, element);
	XSetFillStyle(display, LGC, FillTiled);

	XSetTSOrigin(display, LGC,
	            rem( (int)gkywkd_.qwpax[ws], p_width) - p_width,
	            rem(y_max - (int)gkywkd_.qwpay[ws] - p_height, p_height)
	            );
     }
      else if(gkywkd_.kwfais[ws] == GHATCH)
      {
	/*
	 * Create a hatch element Pixmap for the hatch description specified
	 * by the style index if not already created
	 */

	i = abs((int)gkywkd_.kwfasi[ws]) - 1;
	if (hatches[i].image ==0)
	{
	  /* Make the hatch pattern in image */
	  hatches[i].image = XCreatePixmap(display,
	                        (Drawable *)gk0xunportask(ddbm,ASKBMMEMORY),
	                        hatches[i].width,  hatches[i].height, 1);
          if (hatches[i].image==0)
          {
	    gkyerr_.kerror = -1016;
	      return((f77_integer)0);
          }
	  /* Get GC for element Pixmap has to be separate as it is always
	     one pixel deep */
	  if(HatchGC==(GC)0)
	   HatchGC = XCreateGC(display, hatches[i].image, 0, 0);

          XSetForeground(display,HatchGC,BlackPixel(display,DefaultScreen(display)));
          XSetLineAttributes(display,HatchGC,0,LineSolid,CapButt,JoinMiter);
          XDrawSegments(display,hatches[i].image,
                        HatchGC, hatches[i].ends, hatches[i].numsegs);

        }
	/*
	 * Set hatch to be the stipple, set function, and foreground to
	 * be the fill area colour index
	 */

	XSetForeground(display,LGC,dd->d_fore);
	XSetFillStyle(display, LGC, FillStippled);
        XSetStipple(display, LGC, hatches[i].image);
	XSetTSOrigin(display, LGC, b_left, b_top);
    }

   }
    for(i = 0; i < nbnds; i++)
     XFillPolygon(display, (Drawable *)gk0xunportask(ddbm,ASKBMMEMORY),
     	                   LGC, vlist, npts[i] , Complex, CoordModeOrigin);
  }

  /* Set up workstation workspace parameters for screen update */

  if(b_left < gkywkd_.kwkdat[ws][ILEFT])
    gkywkd_.kwkdat[ws][ILEFT] = (f77_integer)b_left;
  if(b_right > gkywkd_.kwkdat[ws][IRIGHT])
    gkywkd_.kwkdat[ws][IRIGHT] = (f77_integer)b_right;
  if(b_top < gkywkd_.kwkdat[ws][ITOP])
    gkywkd_.kwkdat[ws][ITOP] = (f77_integer)b_top;
  if(b_bottom > gkywkd_.kwkdat[ws][IBOTT])
    gkywkd_.kwkdat[ws][IBOTT] = (f77_integer)b_bottom;

  /* Destroy the element Pixmap if used */

  if(gkywkd_.kwfais[ws]==GPATTR)
      (void)XFreePixmap(display, element);

  /* Free FORTRAN workspace. */

  type = KINTGS;
  (void)gkstda_(&type, &istack);
  if(gkyerr_.kerror != 0) return((f77_integer)0);
  type = KREALS;
  (void)gkstda_(&type, &rstack);
  if(gkyerr_.kerror != 0) return((f77_integer)0);

  /* Deallocate C workspace */

  (void)free(memory);

  return((f77_integer)0);
}
