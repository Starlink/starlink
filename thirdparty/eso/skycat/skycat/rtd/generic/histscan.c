#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	histscan.c (Histogram Scan)
 * Subroutine:	scan_histogram_for_peaks()	returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  30 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include "histeq.h"

/*
 * Subroutine:	scan_histogram_for_peaks
 * Purpose:	Scan the image histogram picking out large cell count values
 *		make sub-groups of the histogram between the large count levels
 */
void scan_histogram_for_peaks ( subrange, histogram,
			        pixel_area, map_levels, average )
     SubrangeLink *subrange;	/* i/o: link (initially covers range) */
     int *histogram;		/* i: histogram (for signed index) */
     int *pixel_area;		/* i/o: number of pixels to account for */
     int *map_levels;		/* i/o: number of levels left to map */
     int *average;		/* i/o: average pixels per color map level */
{
  int i;
  int scan_end;			/* l: end of subrange in histogram */
  int scan_start;		/* l: histogram entry after last peak */
  int pixel_count;		/* l: number of pixels at histogram entry */
  int sr_nzentries;		/* l: number of non-zero entries in subrange */
  int sr_pixel_area;		/* l: number of pixels in current subrange */
  int sr_max_peak;		/* l: highest peak within current  subrange */
  static SubrangeLink *get_new_subrange_record();
  static void fill_subrange_record();

  /* set initial pixel_count values */
  sr_pixel_area = 0;
  sr_nzentries = 0;
  sr_max_peak = 0;
  scan_start = subrange->low;
  scan_end = subrange->high;
  /* run through values in histogram mapping excessive entries */
  for( i = scan_start; i <= scan_end; i++ ) {
    pixel_count = histogram[(ushort)i]; /* allan: added ushort cast */
    /* if this pixel value alone is enough for one level, mark it */
    if( pixel_count >= *average ) {
      /* take this count out of equalization distribution */
      *pixel_area -= pixel_count;
      *map_levels -= 1;
      /* compute new average, (peaks in prior range will be rechecked later */
      if( *map_levels > 0 )
	*average = (*pixel_area / *map_levels) + 1;
      /* make a subrange between peaks if there was a valley & get new link */
      if( i > scan_start ) {
	fill_subrange_record(subrange, scan_start, i - 1, i - scan_start,
			     sr_nzentries, sr_pixel_area, sr_max_peak);
	subrange = get_new_subrange_record(subrange);
      }
      /* make a subrange of one for this peak */
      fill_subrange_record(subrange, i, i, -1, 1, pixel_count, pixel_count);
      subrange->color_levels = 1;
      /* if entries remain, put them in a subrange */
      if (i < scan_end) {
	subrange = get_new_subrange_record(subrange);
	fill_subrange_record(subrange, i + 1, scan_end, scan_end - i, 0, 0, 0);
      }
      /* reset scan values */
      sr_pixel_area = 0;
      sr_nzentries = 0;
      sr_max_peak = 0;
      scan_start = i + 1;
    } else {
      /* update scan values */
      if( pixel_count > 0 ) {
	sr_pixel_area += pixel_count;
	++sr_nzentries;
	if( pixel_count > sr_max_peak )
	  sr_max_peak = pixel_count;
      }
    }
  }
  /* mark the final group */
  if( scan_start < scan_end ) {
    fill_subrange_record(subrange, scan_start, scan_end,
			 (scan_end - scan_start) + 1,
			 sr_nzentries, sr_pixel_area, sr_max_peak);
  }
}

/*
 * Subroutine:	get_new_subrange_record
 * Purpose:	Create a new link in histogram link list, after one given
 * Returns:	Pointer to new subrange link
 */
static SubrangeLink *get_new_subrange_record ( old_link )
     SubrangeLink *old_link;
{
  SubrangeLink *new_link;
  char *calloc_errchk();

  /* create new record for histogram link list */
  new_link = (SubrangeLink *)
    calloc_errchk(1, sizeof(SubrangeLink), "histeq link");
  new_link->next = old_link->next;
  old_link->next = new_link;
  new_link->color_levels = 0;
  new_link->excess_pixels = 0;
  return( new_link );
}

/*
 * Subroutine:	fill_subrange_record
 * Purpose:	Set parameters in subrange link list record
 */
static void fill_subrange_record ( link, low, high, range, nz_entries,
				   pixel_area, max_entry )
     SubrangeLink *link;
     int low, high;		/* i: first and last index in histogram */
     int range;			/* i: span of histogram entries */
     int nz_entries;		/* i: non-zero entries in range */
     int pixel_area;		/* i: sum of histogram entry values (pixels) */
     int max_entry;		/* i: highest histogram entry value */
{
  link->low = low;
  link->high = high;
  link->range = range;
  link->nz_entries = nz_entries;
  link->pixel_area = pixel_area;
  link->max_entry = max_entry;
}
