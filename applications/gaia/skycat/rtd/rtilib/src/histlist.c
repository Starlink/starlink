#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	histlist.c (Histogram List)
 * Subroutine:	make_equalized_list()	returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  30 May 1989
 *		{1} MVH constraints based on best history	  15 Dec 1989
 *		{2} JRW limit to get out of infinite loop	  19 Jun 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>
#include "histeq.h"		/* define SubrangeList */

#define MAXITERATIONS 1000 /* --jrw */

#ifdef DEBUG
#define HDEBUG
#endif

/*
 * Subroutine:	make_equalized_list
 * Purpose:	Distributing levels for a subrange section of the histogram
 */
void make_equalized_list ( histogram, list, low_entry, high_entry,
			   pixel_area, color_levels )
     int *histogram;
     SubrangeList *list;
     int low_entry, high_entry;
     int pixel_area, color_levels;
{
  int shrink_level, stretch_level;
  int max_shrink, min_stretch;
  int average_area, end_area, min_area, max_area;
  int levels_given;		/* levels which equalize_simply used */
  int correction;		/* correction for asymptotic convergence */
  int iterations;		/* avoid infinite loop --jrw */
  int end_error;		/* deviation from ideal of last level */
  int best_levels_over;		/* levels from an equalize worth remembering */
  int best_levels_under;
  int best_average_over = 0;	/* average_area used to get best_levels */
  int best_average_under = 0;
  static int equalize_simply();
#ifdef JIGGLE
  static void adjust_list();
#endif

  /* else allocation distribution must first be determined */
  /* run through histgram section making basic allocation and taking notes */
  average_area = pixel_area / color_levels;
  max_area = max_shrink = 0;
  min_area = min_stretch = pixel_area + 1;
  /* use 2*color_levels to allow allocation to exceed limit */
  levels_given =
    equalize_simply(histogram, list, 0, average_area,
		    low_entry, high_entry, color_levels + color_levels,
		    &shrink_level, &stretch_level,
		    &end_area, &min_area, &max_area,
		    &min_stretch, &max_shrink);
  if( levels_given != color_levels ) {
    /* make approximation for large errors */
    correction = ((color_levels - levels_given) * average_area) / -100;
    /* default correction if error was relatively small */
    if( (correction < 2) && (correction > -2) ) {
      if( color_levels > levels_given )
	correction = -2;
      else
	correction = 2;
    }
    /* record what has been learned so far */
    if( levels_given > color_levels ) {
      best_levels_over = levels_given;
      best_average_over = average_area;
    } else {
      best_levels_under = levels_given;
      best_average_under = average_area;
    }
  } else
    /* if first try was good enough, go with it */
    correction = 0;
  end_error = 0;
  iterations = 0; /* --jrw */
  while( (correction != 0) && (iterations++ < MAXITERATIONS) ) { /* --jrw */
    average_area += correction;
    levels_given =
      equalize_simply(histogram, list, 0, average_area,
		      low_entry, high_entry, color_levels + color_levels,
		      &shrink_level, &stretch_level,
		      &end_area, &min_area, &max_area,
		      &min_stretch, &max_shrink);
    if( levels_given != color_levels ) {
      if( levels_given > color_levels ) {
	/* is this the closest overshoot */
	if( (best_average_over == 0) || (levels_given < best_levels_over) ) {
	  best_levels_over = levels_given;
	  best_average_over = average_area;
	}
	if( correction < 0 ) {
	  /* went too far, turn back if that is an option */
	  if( (correction == -1) && (end_error == 0) )
	    /* done if we never got to a true result */
	    correction = 0;
	  else
	    /* asymtotic correction to converge on solution */
	    correction = (correction - 1) / -2;
	} else if( (best_average_under != 0) &&
		   (best_average_under <= (average_area + correction)) )
	  /* if continue in same direction, don't pass previous best */
	  correction = (best_average_under - average_area) - 1;
      } else {
	/* is this the closest undershoot */
	if( (best_average_under == 0) || (levels_given < best_levels_under) ) {
	  best_levels_under = levels_given;
	  best_average_under = average_area;
	}
	if( correction > 0 ) {
	  if( (correction == 1) && (end_error == 0) )
	    /* done if we never got to a true result */
	    correction = 0;
	  else
	    /* asymtotic correction to converge on solution */
	    correction = (correction + 1) / -2;
	} else if( (best_average_over != 0) &&
		   (best_average_over <= (average_area + correction)) )
	  /* if continue in same direction, don't pass previous best */
	  correction = (best_average_over - average_area) + 1;
      }
    } else if( (end_area > max_area) || (end_area < min_area) ) {
      if( correction < -16 )
	correction = -16;
      else if( correction > 16 )
	correction = 16;
      if( end_area > average_area ) {
	if( correction < 0 ) {
	  /* this is an overshoot */
	  correction = correction / -2;
	  if( (end_error == 0) || ((end_area - average_area) > end_error) ) {
	    /* go back if we might do better */
	    if( correction == 0 )
	      correction = 1;
	  }
	  end_error = end_area - average_area;
	}
      } else {
	if( correction > 0 ) {
	  /* this is an overshoot */
	  correction = correction / -2;
	  if( (end_error == 0) || ((average_area - end_area) > end_error) ) {
	    /* go back if we might do better */
	    if( correction == 0 )
	      correction = -1;
	  }
	  end_error = average_area - end_area;
	}
      }
    } else
      correction = 0;
  }

#if 0
  if (correction != 0) {
    (void)printf("SAOimage: histeq could not converge on best distribution,");
    (void)printf(" proceeding anyway\n");
    (void)fflush(stdout);
  }
#endif

  /* if no fit yet, force one by making last level stretch */
  if( (levels_given != color_levels) && (best_average_over != 0) ) {
    levels_given =
      equalize_simply(histogram, list, 0, best_average_over,
		      low_entry, high_entry, color_levels,
		      &shrink_level, &stretch_level,
		      &end_area, &min_area, &max_area,
		      &min_stretch, &max_shrink);
  }
#ifdef JIGGLE
  /* This section to directly adjust level boundaries for a better fit
   * is experimental. It is not currently used, because the improvement
   * is not important relative to the processing time that can occur
   * in non-stable situations.  In some situations, such as when
   * levels != color_levels coming out of the previous loop, a few
   * passes could yield worthwhile improvements.
   */
#ifdef HDEBUG
  (void)printf("Before adjustment: %d (between: %d %d), end: %d\n",
	       max_area - min_area, min_area, max_area, end_area);
  (void)fflush(stdout);
#endif
  /* jiggle the allocations for a better fit (reduce maximum error) */
  if( (end_area > max_area) && (end_area > min_stretch) ) {
    /* bunch toward the end to make it smaller */
    while( (end_area > max_area) && (end_area > min_stretch) ) {
      average_area = min_stretch;
      adjust_list(1, histogram, list, color_levels,
		  low_entry, high_entry, average_area,
		  &stretch_level, &shrink_level, &end_area,
		  &min_area, &max_area, &min_stretch, &max_shrink);
    }
  } else if( (end_area < min_area) && (end_area < max_shrink) ) {
    /* bunch away from the end to make it bigger */
    while( (end_area < min_area) && (end_area < max_shrink) ) {
      average_area = max_shrink;
      adjust_list(0, histogram, list, color_levels, low_entry, high_entry,
		  average_area, &stretch_level, &shrink_level, &end_area,
		  &min_area, &max_area, &min_stretch, &max_shrink);
    }
  }
#ifdef HDEBUG
  (void)printf("After adjustment: %d (between: %d %d), end: %d\n",
	       max_area - min_area, min_area, max_area, end_area);
  (void)fflush(stdout);
#endif
#endif
}

/*
 * Subroutine:	equalize_simply
 * Purpose:	Make a list to describe map using basic allocation method
 * Note:	Allocate levels from "level" to "color_levels"
 */
static int equalize_simply ( histogram, histlist,
			     level, average_area, low_entry, high_entry,
			     color_levels, shrink_level, stretch_level,
			     end_area, min_area, max_area,
			     min_stretch, max_shrink )
     int *histogram;
     SubrangeList *histlist;
     int level, average_area;
     int low_entry, high_entry;
     int color_levels;
     int *shrink_level, *stretch_level;
     int *end_area, *min_area, *max_area;
     int *min_stretch, *max_shrink;
{
  int entry, neighbor_entry;
  int area, old_area;
  int err_low, err_high;
  int init_next;

  /* initialize parameters */
  init_next = 0;
  area = 0;
  histlist[level].first = low_entry;
  /* reduce by one for indexing to end on last list level */
  color_levels--;
  /* in this pass, assign levels by simple method */
  for( entry = low_entry; entry <= high_entry; entry++ ) {
    if( init_next ) {
      ++level;
      histlist[level].first = entry;
      area = 0;
      init_next = 0;
    }
    old_area = area;
    area += histogram[(ushort)entry]; /* allan: added ushort cast */
    /* while levels last, scan till exceed average area */
    if( (level < color_levels) && (area >= average_area) ) {
      err_low = average_area - old_area;
      err_high = area - average_area;
      if( err_high < err_low ) {
	/* err to excess */
	histlist[level].last = entry;
	histlist[level].pixel_area = area;
	/* shrink option would be to exclude this entry */
	histlist[level].shrink_area = old_area;
	histlist[level].shrink_entry = entry - 1;
	/* look for next non-zero entry to include for stretch option */
	neighbor_entry = entry;
	do {
	  ++neighbor_entry;
	} while( (neighbor_entry <= high_entry) && 
	       (histogram[(ushort)neighbor_entry] == 0) ); /* allan: added ushort cast */
	if( neighbor_entry > high_entry ) {
	  /* if at high end, exagerate to preclude this option */
	  histlist[level].stretch_area = 10 * area;
	  histlist[level].stretch_entry = high_entry;
	} else {
	  histlist[level].stretch_area = area + histogram[(ushort)neighbor_entry]; /* allan: added ushort cast */
	  histlist[level].stretch_entry = neighbor_entry;
	}
      } else {
	/* err short */
	/* exclude this entry */
	neighbor_entry = entry - 1;
	histlist[level].last = neighbor_entry;
	histlist[level].pixel_area = old_area;
	/* including this entry is the stretch option */
	histlist[level].stretch_area = area;
	histlist[level].stretch_entry = entry;
	/* look for previous non-zero entry for shrink option (check limit) */
	while( (neighbor_entry >= low_entry) &&
	       (histogram[(ushort)neighbor_entry] == 0) ) /* allan: added ushort cast */
	  --neighbor_entry;
	if( neighbor_entry < low_entry ) {
	  /* if at low end, preclude this option */
	  histlist[level].shrink_area = 0;
	  histlist[level].shrink_entry = low_entry;
	} else {
	  histlist[level].shrink_area = old_area - histogram[(ushort)neighbor_entry]; /* allan: added ushort cast */
	  histlist[level].shrink_entry = neighbor_entry - 1;
	}
	/* leave this entry for the next level */
	--entry;
      }
      /* check for new champions of excess */
      if( histlist[level].pixel_area < *min_area )
	*min_area = histlist[level].pixel_area;
      if( histlist[level].pixel_area > *max_area )
	*max_area = histlist[level].pixel_area;
      if( histlist[level].stretch_area <= *min_stretch ) {
	*min_stretch = histlist[level].stretch_area;
	*stretch_level = level;
      }
      if( histlist[level].shrink_area >= *max_shrink ) {
	*max_shrink = histlist[level].shrink_area;
	*shrink_level = level;
      }
      init_next = 1;
    }
  }
  /* mark end of list (level should be last one (color_levels - 1)) */
  histlist[level].pixel_area = area;
  *end_area = area;
  histlist[level].last = entry - 1;
  return( level + 1 );
}

#ifdef JIGGLE
/*
 * Subroutine:	adjust_list
 * Purpose:	Alter a level of the map and remake section above altered level
 */
static void adjust_list ( stretch_this_entry, histogram, histlist,
			  color_levels, low_entry, high_entry,
			  average_area, stretch_level, shrink_level, end_area,
			  min_area, max_area, min_stretch, max_shrink )
     int stretch_this_entry;	/* i: stretch an entry, else shrink one */
     int *histogram;
     SubrangeList *histlist;
     int color_levels;
     int low_entry, high_entry, average_area;
     int *shrink_level, *stretch_level;
     int *end_area, *min_area, *max_area;
     int *min_stretch, *max_shrink;
{
  int level, j, neighbor_entry;
  int equalize_simply();

  /* make an adjustment */
  if( stretch_this_entry ) {
    /* stretch an entry */
    level = *stretch_level;
    /* shrink returns to what it was */
    histlist[level].shrink_area = histlist[level].pixel_area;
    histlist[level].shrink_entry = histlist[level].last;
    /* current was stretch option before */
    histlist[level].pixel_area = histlist[level].stretch_area;
    histlist[level].last = histlist[level].stretch_entry;
    /* find the next non-zero entry for the new stretch option */
    neighbor_entry = histlist[level].last;
    do {
      ++neighbor_entry;
    } while( (neighbor_entry <= high_entry) &&
	     (histogram[(ushort)neighbor_entry] == 0) ); /* allan: added ushort cast */
    if( neighbor_entry > high_entry ) {
      /* don't go off the edge */
      histlist[level].stretch_area *= 10;
      histlist[level].stretch_entry = high_entry;
    } else {
      histlist[level].stretch_area += histogram[(ushort)neighbor_entry]; /* allan: added ushort cast */
      histlist[level].stretch_entry = neighbor_entry;
    }
  } else {
    /* shrink an entry */
    level = *shrink_level;
    /* stretch option becomes what it was */
    histlist[level].stretch_area = histlist[level].pixel_area;
    histlist[level].stretch_entry = histlist[level].last;
    /* current was shrink option before */
    histlist[level].last = histlist[level].shrink_entry;
    histlist[level].pixel_area = histlist[level].shrink_area;
    /* look for last non-zero entry to exclude for new shrink option */
    neighbor_entry = histlist[level].last;
    while( (neighbor_entry >= low_entry) &&
	   (histogram[(ushort)neighbor_entry] == 0) ) /* allan: added ushort cast */
      --neighbor_entry;
    if( neighbor_entry < low_entry ) {
      /* don't go off the edge */
      histlist[level].shrink_area = 0;
      histlist[level].shrink_entry = low_entry;
    } else {
      histlist[level].shrink_area -= histogram[(ushort)neighbor_entry]; /* allan: added ushort cast */
      histlist[level].shrink_entry = neighbor_entry - 1;
    }
  }
  /* remake list with this change */
  *min_area = *min_stretch = average_area * 3;
  *max_shrink = *max_area = 0;
  /* retake notes up to point of change */
  for( j = 0; j <= level; j++ ) {
    if( histlist[j].pixel_area < *min_area )
      *min_area = histlist[j].pixel_area;
    if( histlist[j].pixel_area > *max_area )
      *max_area = histlist[j].pixel_area;
    if( histlist[j].stretch_area <= *min_stretch ) {
      *min_stretch = histlist[j].stretch_area;
      *stretch_level = j;
    }
    if( histlist[j].shrink_area >= *max_shrink ) {
      *max_shrink = histlist[j].shrink_area;
      *shrink_level = j;
    }
  }
  if( (level < color_levels) && (histlist[level].last < high_entry) )
    /* make rest of list in normal way */
    (void)equalize_simply(histogram, histlist, j, average_area,
			  histlist[level].last + 1, high_entry, color_levels,
			  shrink_level, stretch_level,
			  end_area, min_area, max_area,
			  min_stretch, max_shrink);
}
#endif

