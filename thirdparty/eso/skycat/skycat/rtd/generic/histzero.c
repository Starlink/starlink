#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	histzero.c (Histogram Zero)
 * Subroutine:	resolve_zeroes()			returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  30 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>
#include "histeq.h"		/* define SubrangeLink */

/*
 * Subroutine:	resolve_zeroes
 * Purpose:	Combine groups with zero alloted levels with adjoining groups
 * Note:	Adjoining groups are large count single level groups
 * Called by:	histrogram_equalize() above
 */
void resolve_zeroes ( PriorLink, zeroes )
     SubrangeLink *PriorLink;
     int zeroes;
{
  SubrangeLink *ThisLink, *NextLink;
  int a_count, b_count, z1count, z2count;
  static void merge_links();

  /* if very first entry is a zero allocated link */
  if( PriorLink->color_levels == 0 ) {
    /* merge this and next */
    merge_links(PriorLink);
    zeroes--;
  }
  /* scan for the zero allocated links */
  while( zeroes > 0 ) {
    ThisLink = PriorLink->next;
#ifdef DEBUG
    /* if reached the end of the list, we had an error */
    if( ThisLink == 0 ) {
      (void)fprintf(stderr,"Zero error\n");
      return;
    }
#endif
    /* if we are about to hit a zero */
    if( ThisLink->color_levels == 0 ) {
      /* if it is the last zero merge with prior */
      NextLink = ThisLink->next;
      if( NextLink == 0 ) {
	merge_links(PriorLink);
	return;
      }
      a_count = PriorLink->pixel_area;
      b_count = NextLink->pixel_area;
      /* if the preceding link is smaller than the trailing link */
      if( a_count > b_count ) {
	merge_links(PriorLink);
	zeroes--;
      } else {
	/* probe beyond */
	if( NextLink->next != 0 ) {
	  if( NextLink->next->color_levels != 0 ) {
	    /* if new competition, merge with next link */
	    merge_links(ThisLink);
	    zeroes--;
	  } else {
	    z1count = ThisLink->pixel_area;
	    z2count = NextLink->next->pixel_area;
	    /* where would the next one go? */
	    if( (NextLink->next->next == 0) ||
		(NextLink->next->next->pixel_area > (b_count + z2count)) ) {
	      if( (b_count + z2count) > (a_count + z1count) ) {
		merge_links(PriorLink);
	      } else {
		merge_links(ThisLink);
	      }
	    } else
	      merge_links(ThisLink);
	    zeroes--;
	  }
	} else {
	  merge_links(PriorLink);
	  zeroes--;
	}
      }
    }
    PriorLink = ThisLink;
  }
}

/*
 * Subroutine:	merge_links
 * Purpose:	Combine two links of histogram group list
 */
static void merge_links ( subrange )
     SubrangeLink *subrange;
{
  SubrangeLink *lostlink;

  lostlink = subrange->next;
  subrange->next = lostlink->next;
  subrange->max_entry = MAX(subrange->max_entry, lostlink->max_entry);
  subrange->pixel_area += lostlink->pixel_area;
  subrange->high = lostlink->high;
  subrange->range += lostlink->range;
  subrange->nz_entries += lostlink->nz_entries;
  subrange->excess_pixels += lostlink->excess_pixels;
  subrange->color_levels += lostlink->color_levels;
  free( (char *)lostlink );
}
