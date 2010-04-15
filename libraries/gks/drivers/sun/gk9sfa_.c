/*
 * RAL GKS SYSTEM
 *
 * gk9sfa_:  Performs fill area for the Sun workstation driver.
 *
 * Type of Routine:  Part of WORKSTATION DRIVER
 * Author:           PJWR
 *
 * Copyright (C) SERC 1987
 *
 * Maintenance Log:
 *
 *  28/10/87  PJWR Created.
 *  30/10/87  PJWR Initial version stabilised.
 *  03/11/87  PJWR Release version stabilised.
 *  12/01/89  TAW  Changed pixel value for pr_put to be a Sun colour index
 *  		   looked up by gk9scc_ rather than using a GKS colour index
 *		   directly.
 *  16/01/89  TAW  Included support for pattern reference point.
 *  12/02/90  PLP  Sorted out the non-square pattern problem (S364).
 *  24/02/91  PLP  Corrected pattern reference code.
 */

#include "./varinc/wwinfo.h"				/* For ww */
#include <pixrect/pixrect_hs.h>			/* For pixrect information */
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
#include "gk9swd.h"				/* For WS workspace offsets */

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
  short
    *image;		/* Image data in memory pixrect format */
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
 *         the current fill area colour using pr_polyline().
 *       SOLID areas are  filled  with  the  current  fill  area  colour  using
 *         pr_polygon_2().
 *       PATTERN areas are filled using a texture pixrect and pr_polygon_2().
 *         The pattern pixrect is  generated  by  creating  a  pattern  element
 *         pixrect from the pattern description stored  on  the  GKS  heap  and
 *         replicating this onto a pixrect the size of the  primitive  bounding
 *         box.
 *       HATCH areas are filled using a texture pixrect and pr_polygon_2(). The
 *         hatch pixrect is created by forcing the  current  fill  area  colour
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


f77_integer gk9sfa_(nc, xwc, ywc)
  f77_integer *nc;		/* Number of vertices (In) */
  f77_real    *xwc,		/* Vertex X world coordinates (In) */
	      *ywc;		/* Vertex Y world coordinates (In) */
{

#include "./varinc/hatches.h"		/* Hatch element image data declarations */

  static Hatch_desc
    hatches[] = 		/* Hatch description lookup table */
    {
      { 64, 64,  hatch_1 }, { 64, 64,  hatch_2 }, { 60, 60,  hatch_3 },
      { 60, 60,  hatch_4 }, { 64, 64,  hatch_5 }, { 60, 60,  hatch_6 },
      { 48, 56,  hatch_7 }, { 48, 48,  hatch_8 }, { 44, 44,  hatch_9 },
      { 48, 56, hatch_10 }
    };

  extern f77_integer
    gkstal_(),			/* For allocating GKS stack */
    gkstda_(),			/* For releasing GKS stack */
    gkdrge_(),			/* For getting pattern directory */
    gkhpgi_(),			/* For getting pattern data */
    gkpclp_();			/* For clipping the area to be filled */

  extern char
    *malloc();			/* For allocating C workspace */

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

  struct pr_pos
    *vlist;			/* Pixrect coordinates of area to be filled */

  struct pixrect
    *display = 			/* ww backing pixrect */
      (struct pixrect *)unportask((void *)ddbm, ASKBMMEMORY),
    *element,			/* Pattern or hatch element */
    *stencil,			/* Hatch stencil */
    *texture;			/* Texture for pr_polygon_2() */

  int
    b_left,			/* Primitive bounding box left coordinate */
    b_right,			/* Primitive bounding box right coordinate */
    b_top,			/* Primitive bounding box top coordinate */
    b_bottom,			/* Primitive bounding box bottom coordinate */
    b_width,			/* Primitive bounding box width */
    b_height,			/* Primitive bounding box height */
    ws =			/* C version of workstation index */
      (int)gkywca_.kwkix - 1,
    y_max = 			/* Display height for coordinate inversion */
      (int)gkywdt_.kdsry[ws] - 1,
    status,			/* Return status of pixrect routines */
    rasterop,			/* The rasterop for pr_polygon_2() */
    *npts,			/* List of boundary vertex counts */
    i, j;			/* Loop indices / temporary array indices */

  char
    *memory;			/* Address of C workspace */

   double
    fmod(),
    fabs();
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

  /* Allocate C workspace for boundary vertex counts and pixrect coordinates */

  memory = malloc((unsigned)(nbnds * sizeof(int) +
			     lastvertex[nbnds-1] * sizeof(struct pr_pos)
			    )
		 );

  npts = (int *)memory;
  vlist = (struct pr_pos *)(memory + nbnds * sizeof(int));

  /* Translate last vertex list into a list of boundary vertex counts */

  for(i = nbnds - 1; i > 0; i--)
    npts[i] = lastvertex[i] - lastvertex[i-1];
  npts[0] = lastvertex[0];

  /*
   * Truncate the vertex coordinates and put them into pixrect form,
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

  /* Render the polygon as specified by the interior style */

  if(gkywkd_.kwfais[ws] == GHOLLO)

    /*
     * Draw the boundary of each disjoint area in the set returned by the
     * clipping routine
     */

    for(i = 0; i < nbnds; i++)
    {
      status = pr_polyline(display, 0, 0, npts[i], vlist, POLY_CLOSE,
			   (Pr_brush *)0, (Pr_texture *)0,
			   PIX_SRC | PIX_COLOR(dd->d_fore)
			  );
      if(status == PIX_ERR)
      {
	gkyerr_.kerror = 304;
	return((f77_integer)0);
      }
      vlist += npts[i];
    }
  else
  {
    if(gkywkd_.kwfais[ws] == GSOLID)
    {
      /*
       * Set up a null texture pixrect and a source rasterop including the
       * fill area colour index for pr_polygon_2
       */

      texture = (struct pixrect *)0;
      rasterop = PIX_SRC|PIX_COLOR(dd->d_fore);
    }
    else
    {
      /*
       * Pattern or hatch interior style.  Both require the bounding box
       * dimensions and a texture pixrect the same depth as the backing
       * pixrect.
       */

      b_width = b_right - b_left + 1;
      b_height = b_bottom - b_top + 1;
      texture = mem_create(b_width, b_height, (gkywdt_.kpci[ws] > 2) ? 8 : 1);
      if(texture == (struct pixrect *)0)
      {
	gkyerr_.kerror = -1016;
	return((f77_integer)0);
      }

      /* Now deal with each style */

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

	type = KINTGS; size = pat_desc[0] * pat_desc[1];
	(void)gkstda_(&type,&istack);
	if(gkyerr_.kerror != 0) return((f77_integer)0);
	(void)gkstal_(&type,&size,&istack);
	if(gkyerr_.kerror != 0) return((f77_integer)0);
	pat_data = &gkystk_.stk.kstack[istack-1];

	/* Get pattern data from GKS heap */

	(void)gkhpgi_(&pat_desc[2], &zero, &size, pat_data);

	/*
	 * Create a pixrect representing the pattern element. Note that
	 * pattern data is in row major memory order
	 */

	element = mem_create((int)pat_desc[0], (int)pat_desc[1],
			     (gkywdt_.kpci[ws] > 2) ? 8 : 1
			    );
	if(element == (struct pixrect *)0)
	{
	  gkyerr_.kerror = -1016;
	  return((f77_integer)0);
	}
	for(i = 0; i < pat_desc[1]; i++)
	  for(j = 0; j < pat_desc[0]; j++)
	  {
	    status = pr_put(element, j, i,
	                    (int)gk9scc_(&pat_data[i*pat_desc[0]+j]));
	    if(status == PIX_ERR)
	    {
	      gkyerr_.kerror = -1016;
	      return((f77_integer)0);
	    }
	  }

	/*
	 * Replicate the element over the texture pixrect and set up a copy
	 * rasterop for pr_polygon_2
	 */
        i = pat_desc[0] -
            nint(fmod(fabs((double)(gkywkd_.qwpax[ws] - b_left)),
                           (double) pat_desc[0]));
        j = (int)(fmod(fabs((double)(gkywkd_.qwpay[ws] - b_bottom)),
                            (double) pat_desc[1])) - 1;
	status = pr_replrop(texture, 0, 0, b_width, b_height, PIX_SRC, element,
			    i,
			    j
			   );
	if(status == PIX_ERR)
	{
	  gkyerr_.kerror = -1016;
	  return((f77_integer)0);
	}
	rasterop = PIX_SRC;
      }
      else if(gkywkd_.kwfais[ws] == GHATCH)
      {
	/*
	 * Create a hatch element pixrect from the hatch description specified
	 * by the style index
	 */

	i = abs((int)gkywkd_.kwfasi[ws]) - 1;
	element = mem_point(hatches[i].width, hatches[i].height, 1,
			    hatches[i].image
			   );
	if(element == (struct pixrect *)0)
	{
	  gkyerr_.kerror = -1016;
	  return((f77_integer)0);
	}

	/* Create a stencil pixrect and replicate the hatch element over it */

	stencil = mem_create(b_width, b_height, 1);
	if(stencil == (struct pixrect *)0)
	{
	  gkyerr_.kerror = -1016;
	  return((f77_integer)0);
	}
	status = pr_replrop(stencil, 0, 0, b_width, b_height, PIX_SRC,
			    element, 0, 0
			   );
	if(status == PIX_ERR)
	{
	  gkyerr_.kerror = -1016;
	  return((f77_integer)0);
	}

	/*
	 * Copy the contents of the primitives bounding box from the ww
	 * backing pixrect into the texture pixrect
	 */

	status = pr_rop(texture, 0, 0, b_width, b_height, PIX_SRC,
			display, b_left, b_top
		       );
	if(status == PIX_ERR)
	{
	  gkyerr_.kerror = -1016;
	  return((f77_integer)0);
	}

	/*
	 * Draw the hatch onto the copied background using the stencil and a
	 * source rasterop including the fill area colour index and set up a
	 * copy rasterop for pr_polygon_2
	 */

	status = pr_stencil(texture, 0, 0, b_width, b_height,
			    PIX_SRC|PIX_COLOR(dd->d_fore),
			    stencil, 0, 0, (struct pixrect *)0, 0, 0
			   );
	if(status == PIX_ERR)
	{
	  gkyerr_.kerror = -1016;
	  return((f77_integer)0);
	}
	rasterop = PIX_SRC;
      }
    }

    /*
     * Fill the set of boundaries using pr_polygon_2 with the textures and
     * rasterops derived above.
     */

    status = pr_polygon_2(display, 0, 0, (int)nbnds, npts, vlist, rasterop,
			  texture, -b_left, -b_top
			 );
    if(status == PIX_ERR)
    {
      gkyerr_.kerror = 304;
      return((f77_integer)0);
    }
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

  /* Destroy the element, stencil and texture pixrects if used */

  switch(gkywkd_.kwfais[ws])
  {
    case GHATCH:
      (void)pr_destroy(stencil);
    case GPATTR:
      (void)pr_destroy(element);
      (void)pr_destroy(texture);
      break;
    default:
      break;
  }

  /* Free FORTRAN workspace. */

  type = KINTGS;
  (void)gkstda_(&type, &istack);
  if(gkyerr_.kerror != 0) return((f77_integer)0);
  type = KREALS;
  (void)gkstda_(&type, &rstack);
  if(gkyerr_.kerror != 0) return((f77_integer)0);

  /* Dealloacate C workspace */

  (void)free(memory);

  return((f77_integer)0);
}
