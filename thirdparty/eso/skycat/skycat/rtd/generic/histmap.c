#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	histmap.c (Histogram Map)
 * Subroutine:	make_HE_scalemap()		returns: int
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
#include "histeq.h"		/* define SubrangeLink, List */

static void make_subrange_scalemap();
static void make_gapped_list(), list_to_map();
static int first_shortlist_pass();
static void add_level_to_short_list();


/*
 * Subroutine:	generate_scalemap
 * Purpose:	Make scalemap, applying standard histgoram equalization
 *		one subrange group at a time
 * Note:	Value range was broken into groups with an assigned number
 *		of levels for each group.  Each group is either a single
 *		value or a range of values with no excessive peaks, such that
 *		standard (non-optimizing) histogram equaliztion algorithm can
 *		safely be applied.
 * Note:	The original link-list of groups is freed.
 */
void generate_scalemap ( hist, subrange, scalemap, pixels )
     int *hist;				/* i: histogram (for signed offsets) */
     SubrangeLink *subrange;	/* i: linklist of subranges */
     unsigned long *scalemap;		/* i: scalemap (for signed indexing) */
     unsigned long *pixels;		/* i: map to hardware entries */
{
  int baselevel;
  SubrangeLink *trash;

  baselevel = 0;
  while( subrange != 0 ) {
    make_subrange_scalemap(hist, subrange, scalemap, baselevel, pixels);
    if( subrange->color_levels > 0 )
      baselevel += subrange->color_levels;
    trash = subrange;
    subrange = subrange->next;
    free((char *)trash);
  }
}

/*
 * Subroutine:	make_subrange_scalemap
 * Purpose:	Make a section of scale map using histgroup link as guide 
 * Called by:	make_HE_scalemap() in HistEqual.c
 */
static void
  make_subrange_scalemap ( histogram, subrange, scalemap, baselevel, pixels )
     int *histogram;
     SubrangeLink *subrange;
     unsigned long *scalemap;		/* scalemap (for signed indexing) */
     int baselevel;
     unsigned long *pixels;		/* i: map to hardware entries */
{
  int i, color_levels;
  SubrangeList *list;
  unsigned long dispval;
  char *calloc_errchk();
  void make_equalized_list();

  /* if only one level, make map section */
  if( subrange->color_levels <= 1 ) {
    dispval = pixels[baselevel];
    for( i = subrange->low; i <= subrange->high; i++ ) {
	scalemap[(ushort)i] = dispval; /* allan: added ushort cast */
    }
    return;
  }
  color_levels = subrange->color_levels;
  /* allocate excess space as initial efforts may overshoot number of levels */
  list = (SubrangeList *)
    calloc_errchk(2 * color_levels, sizeof(SubrangeList), "HistList");
  /* if normal processing will not work, choose special */
  if( color_levels < subrange->nz_entries ) {
    make_equalized_list(histogram, list, subrange->low, subrange->high,
			subrange->pixel_area, color_levels);
  } else {
    make_gapped_list(histogram, list, subrange->low, subrange->high,
		     color_levels);
  }
#ifdef DEBUG
  /* check work done */
  if( list[color_levels - 1].last != subrange->high ) {
    (void)fprintf(stderr, "ERROR: histogram list not right\n");
    (void)fprintf(stderr, "levels: %d, list: %d, link: %d\n",
		  color_levels, list[color_levels - 1].last, subrange->high);
  }
#endif
  /* make section of map as defined by list */
  list_to_map(scalemap, list, baselevel, color_levels, pixels);
  /* free the list space */
  free( (char *)list );
}

/*
 * Subroutine:	list_to_map
 * Purpose:	Make section of map as defined by list
 * Called by:	make_subrange_scalemap() above
 */
static void list_to_map ( scalemap, histlist, baselevel, levels, pixels )
     unsigned long *scalemap;		/* scalemap (for signed indexing) */
     SubrangeList *histlist;
     int baselevel, levels;
     unsigned long *pixels;		/* i: map to hardware entries */
{
  int i, level;
  int first, last, imageval;
  unsigned long dispval;

  level = baselevel;
  for( i = 0; i < levels; i++ ) {
    first = histlist[i].first;
    last = histlist[i].last;
    dispval = pixels[level];
    for( imageval = first; imageval <= last; imageval++ ) {
      scalemap[(ushort)imageval] = dispval;  /* allan: added ushort cast */
    }
    level++;
  }
}

/*
 * Subroutine:	make_gapped_list
 * Purpose:	Allocate levels for a histogram subrange.  Special process
 * 		for situation when more levels than actually used values.
 */
static void make_gapped_list ( histogram, list, low, high, levels )
     int *histogram;
     SubrangeList *list;
     int low, high, levels;
{
  int range_j, max_range;
  int levels_used;

  levels_used =
    first_shortlist_pass(histogram, list, low, high, levels,
			 &max_range, &range_j);
  while( levels_used < levels ) {
    add_level_to_short_list(list, levels_used - 1, &max_range, &range_j);
    ++levels_used;
  }
}

/*
 * Subroutine:	first_shortlist_pass
 * Purpose:	Make a list to describe map allocation using special
 *		allocation method.  Fill the list with each entry ending
 *		at the next actually used value.
 */
static int
  first_shortlist_pass ( histogram, list, low_entry, high_entry, levels,
			 max_range, range_j )
     int *histogram;
     SubrangeList *list;
     int low_entry, high_entry, levels;
     int *range_j, *max_range;
{
  int i, area, level;

  /* initialize parameters (index starts at 0) */
  level = 0;
  area = 0;
  *max_range = -1;
  list[level].first = low_entry;
  /* first pass, assign levels by simple method */
  for( i = low_entry; i <= high_entry; i++ ) {
    area += histogram[(ushort)i]; /* allan: added ushort cast */
    /* while levels last, scan till value which is used */
    if( (area > 0) || (i == high_entry) ) {
      list[level].last = i;
      list[level].pixel_area = area;
      list[level].shrink_entry = (i - list[level].first) + 1;
      /* find the lowest entry with the highest range */
      if( list[level].shrink_entry > *max_range ) {
	*max_range = list[level].shrink_entry;
	*range_j = level;
      }
      if( i < high_entry ) {
	/* start for next group */
	list[++level].first = i + 1;
#ifdef DEBUG
	if( level > levels ) {
	  (void)fprintf(stderr, "Actual exceeds levels\n");
	  level--;
	}
#endif
      } else if (level >= levels) {
	list[level - 1].last = i;
      }
      area = 0;
    }
  }
  return( level+1 );
}

/*
 * Subroutine:	add_level_to_short_list
 */
static void add_level_to_short_list ( list, top, max_range, range_j )
     SubrangeList *list;
     int top;
     int *max_range, *range_j;
{
  int i, j, mark;

  mark = *range_j;
  *max_range = -1;
  for( i = top, j = top + 1; j > mark; i--, j-- ) {
    list[j].first = list[i].first;
    list[j].last = list[i].last;
    list[j].pixel_area = list[i].pixel_area;
    list[j].shrink_entry = list[i].shrink_entry;
    /* find the lowest entry with the highest range */
    if( list[j].shrink_entry >= *max_range ) {
      *max_range = list[j].shrink_entry;
      *range_j = j;
    }
  }
  i++;
  j++;
  list[i].last = list[i].first + ((list[i].shrink_entry / 2) - 1);
  list[j].first = list[i].last + 1;
  list[i].pixel_area = 0;
  list[i].shrink_entry = (list[i].last - list[i].first) + 1; 
  list[j].shrink_entry = (list[j].last - list[j].first) + 1; 
  for( ; j >= 0; j-- ) {
    /* find the lowest entry with the highest range */
    if( list[j].shrink_entry >= *max_range ) {
      *max_range = list[j].shrink_entry;
      *range_j = j;
    }
  }
}
