/* Part of dvi2bitmap.
 * Copyright 1999, Particle Physics and Astronomy Research Council.
 * See file LICENCE for conditions.
 */
// $Id$

#ifndef PAGERANGE_HEADER_READ
#define PAGERANGE_HEADER_READ 1

// gcc iostream may somehow omit NULL
#ifndef NULL
#define NULL 0
#endif

#include <iostream>
#include <map>
#include "verbosity.h"

class PageRange
{
  public:
    PageRange();

    bool addSpec(const char *, const char *);
    bool isSelected (const int, const int*);
    static void verbosity (const verbosities level) { verbosity_ = level; }

  private:
    bool useCounts_;
    int useCountNo_;
    int first_, last_;
    enum { oneRange, ranges, unset } rangeType_;
    static verbosities verbosity_;
    map<int,bool> setPages_;
};

#endif
