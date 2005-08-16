#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	histdist.c (Histogram Equalize Distribute)
 * Subroutine:	distribute_levels()			returns: int
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  30 May 1989
 *		{1} MVH fixed bug in range_zgroup		   7 Dec 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>
#include "histeq.h"		/* define SubrangeLink */

/* Local prototypes */
static int excess_zgroup();
static int excess_nzgroup();
static int range_zgroup();

/*
 * Subroutine:	distribute_levels
 * Purpose:	Distribute the levels among histogram sub-groups
 * Returns:	number of groups with no assigned color levels
 */
int distribute_levels ( linklist, pixel_area, color_levels,
		        pmin, pmax, ncolor )
     SubrangeLink *linklist;
     int pixel_area, color_levels;
     int pmin, pmax, ncolor;
{
  int average;
  int threshold;
  int excess;
  int levels, zeroes;
  int max_z_excess, max_nz_excess, max_z_range;
  SubrangeLink *subrange;
#ifdef DEBUG
  int census = 0;
#endif

  /* if all one group (no strong peaks), allocation is simple */
  if( linklist->next == 0 ) {
    linklist->color_levels = ncolor;
    return( 0 );
  }
  /* average is rounded strongly upward to be stingy with levels */
  average = (pixel_area / color_levels) + 1;
  subrange = linklist;
  zeroes = 0;
  max_z_excess = max_nz_excess = 0;
  max_z_range = 0;
  /* first pass, simple assignment and some note taking */
  while( subrange != 0 ) {
    if( subrange->range > 0 ) {
      levels = subrange->pixel_area / average;
      excess = subrange->pixel_area - (levels * average);
      if( levels >= subrange->range ) {
	levels = subrange->range;
	subrange->range = -subrange->range;
      } else {
	if( levels == 0 ) {
	  zeroes++;
	  if( excess > max_z_excess )
	    max_z_excess = excess;
	  if( subrange->range > max_z_range )
	    max_z_range = subrange->range;
	} else {
	  if( excess > max_nz_excess )
	    max_nz_excess = excess;
	}
      }
      subrange->color_levels = levels;
      subrange->excess_pixels = excess;
      color_levels -= levels;
#ifndef DEBUG
    }
#else
      census += levels;
    } else
      ++census;
#endif
    subrange = subrange->next;
  }
  /* second pass groups with no levels and vals or ranges above limits */
  if( zeroes > 0 ) {
    /* groups with counts above 1/4 of average */
    threshold = average / 4;
    while( (zeroes > 0) && (color_levels > 0) && (max_z_excess > threshold) ) {
      if( excess_zgroup(linklist, &max_z_excess, &max_z_range, average) ) {
	zeroes--;
	color_levels--;
#ifndef DEBUG
      }
#else
	census++;
      } else
        (void)fprintf(stderr, "Failed to find excess zero.\n");
#endif
    }
    /* groups with range above 2 * unequalized distribution */
    threshold = MAX(((pmax - pmin) / 8) , 4);
    while( (zeroes > 0) && (color_levels > 0) && (max_z_range > threshold) ) {
      if( range_zgroup(linklist, &max_z_excess, &max_z_range, average) ) {
	zeroes--;
	color_levels--;
#ifndef DEBUG
      }
#else
	census++;
      } else
        (void)fprintf(stderr, "Failed to find range zero.\n");
#endif
    }
  }
  /* third pass, give away any remaining levels by highest excess */
#ifdef DEBUG
  if( color_levels != (ncolor - census) ) {
    (void)fprintf(stderr, "Allocation waste: %d level(s)\n",
	  	  (ncolor - census) - color_levels);
  }
#endif
  while( color_levels > 0 ) {
    if( (zeroes > 0) && (max_z_excess > max_nz_excess) ) {
      if( excess_zgroup(linklist, &max_z_excess, &max_z_range, average) ) {
	zeroes--;
	color_levels--;
      } else {
#ifdef DEBUG
        (void)fprintf(stderr, "Failed to find excess zero.\n");
#endif
	break;  /* allan: break endless loop */
      }
    } else {
      if( excess_nzgroup(linklist, &max_nz_excess, average) )
	color_levels--;
      else {
#ifdef DEBUG
        (void)fprintf(stderr, "Failed to find excess.\n");
#endif
        break;  /* allan: break endless loop */
      }
    }
  }
  return( zeroes );
}

/*
 * Subroutine:	excess_zgroup
 * Purpose:	Find subrange with zero allotted levels and specified excess.
 *		Assign it one level
 */
static int excess_zgroup ( subrange, excess, range, average )
     SubrangeLink *subrange;
     int *excess, *range;
     int average;
{
  int max_excess, looking;

  max_excess = -32700;
  looking = 1;
  while( subrange != 0 ) {
    if( (subrange->color_levels == 0) && (subrange->range > 0) ) {
      if( looking && (subrange->excess_pixels == *excess) ) {
	if( subrange->range > 1 ) {
	  subrange->color_levels = 1;
	} else {
	  subrange->color_levels = 1;
	  subrange->range = -1;
	}
	subrange->excess_pixels -= average;
	looking = 0;
      } else {
	if( subrange->excess_pixels > max_excess )
	  max_excess = subrange->excess_pixels;
	if( subrange->range > *range )
	  *range = subrange->range;
      }
    }
    subrange = subrange->next;
  }
  *excess = max_excess;
  return( !looking );
}

/*
 * Subroutine:	range_zgroup
 * Purpose:	Find group with zero allotted levels and specified range.
 *		Assign it one level.
 */
static int range_zgroup (subrange, excess, range, average)
     SubrangeLink *subrange;
     int *excess, *range;
     int average;
{
  int max_range, looking;

  max_range = 0;
  looking = 1;
  while( subrange != 0 ) {
    if( (subrange->color_levels == 0) && (subrange->range > 0) ) {
      if( looking && (subrange->range == *range) ) {
	if( subrange->range > 1 ) {
	  subrange->color_levels = 1;
	} else {
	  subrange->color_levels = 1;
	  subrange->range = -1;
	}
	subrange->excess_pixels -= average;
	looking = 0;
      } else {
	if( subrange->excess_pixels > *excess )
	  *excess = subrange->excess_pixels;
	if( subrange->range > max_range )
	  max_range = subrange->range;
      }
    }
    subrange = subrange->next;
  }
  *range = max_range;
  return( !looking );
}

/*
 * Subroutine:	excess_nzgroup
 * Purpose:	Find group with non-zero allotted levels and specified
 *		excess value.  Assign it one additional level.
 */
static int excess_nzgroup ( subrange, excess, average )
     SubrangeLink *subrange;
     int *excess;
     int average;
{
  int looking, max_excess;

  looking = 1;
  max_excess = -32767;
  while( subrange != 0 ) {
    if( (subrange->color_levels > 0) && (subrange->range > 1) ) {
      if( looking && (subrange->excess_pixels == *excess) &&
	  (subrange->color_levels < subrange->range) ) {
	subrange->color_levels += 1;
	subrange->excess_pixels -= average;
	if( subrange->color_levels == subrange->range ) {
	  subrange->color_levels = subrange->range;
	  subrange->range = -subrange->range;
	} else {
	  if( subrange->excess_pixels > max_excess )
	    max_excess = subrange->excess_pixels;
	}
	looking = 0;
      } else {
	if( subrange->excess_pixels > max_excess )
	  max_excess = subrange->excess_pixels;
      }
    }
    subrange = subrange->next;
  }
  *excess = max_excess;
  return( !looking );
}
