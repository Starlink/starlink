#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	histeql.c (Histogram Equalize)
 * Purpose:	Fill the scalemap by histogram equalization
 * Subroutine:	histogram_equalize()		returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  30 May 1989
 *              {1} Peter W. Draper     converted to use unsigned 20 Jan 1999
 *                                      long for scalemap
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>
#include "histeq.h"		/* define SubrangeLink */

/*
 * Subroutine:	histogram_equalize
 * Purpose:	Create a scaling map which equalizes image cells per
 *		output level optimization accounts for large cell counts
 *		for single levels (e.g. half of all pixels with value 0)
 */
void histogram_equalize ( scalemap, histogram, area, pmin, pmax,
			  color_levels, pixels )
     unsigned long *scalemap;	/* i/o: scalemap (for signed indexing) */
     int *histogram;		/* i: histogram (for signed indexing) */
     int area;			/* i: area in pixels when histogram was made */
     int pmin, pmax;		/* i: min and max values in histogram */
     int color_levels;		/* i: number of levels in color map */
     unsigned long *pixels;	/* i: map to hardware entries */
{
  SubrangeLink *linklist;	/* l: beginning of subrange linklist */
  int average_area;		/* l: average area covered by each level */
  int pixel_area;
  int map_levels;
  int nz_entries;		/* l: non-zero entries in histogram */
  int empties;			/* l: subranges with no alloted levels */
  char *calloc_errchk();
  int distribute_levels();
  void generate_scalemap(), scan_histogram_for_peaks(), resolve_zeroes();
  static int count_nonzero_histogram_entries(), rescan_histogram();
  static void unmark_peak_links();

  /* initialize link list */
  linklist = (SubrangeLink *)calloc_errchk(10, sizeof(int), "HElink");
  linklist->next = 0;
  linklist->low = pmin;
  linklist->high = pmax;
  linklist->range = (pmax - pmin) + 1;
  linklist->pixel_area = area;
  linklist->max_entry = 0;
  /* if not enough non-zero entries to make distribution, bypass work */
  nz_entries = count_nonzero_histogram_entries(histogram, pmin, pmax);
  if( nz_entries <= color_levels ) {
    linklist->color_levels = color_levels;
    linklist->nz_entries = nz_entries;
    generate_scalemap(histogram, linklist, scalemap, pixels);
    return;
  }
  /* initialize count and level variables */
  pixel_area = area;
  map_levels = color_levels;
  /* desired number of pixels at each level */
  /* average is rounded strongly upward to be stingy with levels */
  average_area = (pixel_area / map_levels) + 1;
  /* go through histogram seeking histogram entries above the map mean */
  scan_histogram_for_peaks(linklist, histogram,
			   &pixel_area, &map_levels, &average_area);
  /* repeat scans until no more treetops emerge */
  while( rescan_histogram(linklist, histogram,
			  &pixel_area, &map_levels, &average_area) != 0 );
  /* allocate levels */
  empties =
    distribute_levels(linklist, pixel_area, map_levels,
		      pmin, pmax, color_levels);
  /* make peak and valley links look the same (peaks have range 1) */
  unmark_peak_links(linklist, color_levels);
  if( empties > 0 ) {
    resolve_zeroes(linklist, empties);
  }
  /* make map and free memory */
  generate_scalemap(histogram, linklist, scalemap, pixels);
}

/*
 * Subroutine:	rescan_histogram
 * Purpose:	Repeat scanning for large count levels as saturation level
 *		is modified.  Repeats until all large count levels are
 *		marked and residual is stable
 * Returns:	1 if a range was modified, 0 if nothing was changed
 */
static int rescan_histogram ( subrange, histogram,
			      pixel_area, map_levels, average_area )
     SubrangeLink *subrange;
     int *histogram;
     int *pixel_area;
     int *map_levels;
     int *average_area;
{
  int process;
  void scan_histogram_for_peaks();

  process = 0;
  while( subrange != 0 ) {
    /* if subrange has values that exceed current average */
    if( (subrange->range > 1) && (subrange->max_entry >= *average_area) ) {
      scan_histogram_for_peaks(subrange, histogram,
			       pixel_area, map_levels, average_area);
      /* indicate that additional processing has taken place */
      process = 1;
    }
    subrange = subrange->next;
  }
  return( process );
}

/*
 * Subroutine:	unmark_peak_links
 * Purpose:	Make singularity links non_uniquely marked (range > 0) and
 *		check count against reference
 * Called by:	histogram_equalize() above
 */
static void unmark_peak_links ( subrange, nlevels )
     SubrangeLink *subrange;
     int nlevels;
{
#ifdef DEBUG
  int levels = 0;
#endif

  while( subrange != 0 ) {
    if( subrange->range <0 ) {
      subrange->range = -subrange->range;
    }
#ifdef DEBUG
    if( (subrange->next != 0) &&
	(subrange->next->low != (subrange->high + 1)) )
      (void)fprintf(stderr, "Missing Link in list.\n");
    if( subrange->color_levels > subrange->range )
      (void)fprintf(stderr, "Excess levels in a Link.\n");
    levels += subrange->color_levels;
#endif
    subrange = subrange->next;
  }
#ifdef DEBUG
  if( levels != nlevels ) {
    (void)fprintf(stderr, "Levels = %d\n",levels);
  }
#endif
}

/*
 * Subroutine:	count_nonzero_histogram_entries
 * Called by:	histogram_equalize() in HistEqual.c
 */
static int count_nonzero_histogram_entries ( histogram, pmin, pmax )
     register int *histogram;
     int pmin;
     register int pmax;
{
  register int i, npix;

  npix = 0;
  for( i = pmin; i <= pmax; i++ ) {
      if( histogram[(ushort)i] > 0 ) /* allan: added ushort cast */
      ++npix;
  }
  return( npix );
}
