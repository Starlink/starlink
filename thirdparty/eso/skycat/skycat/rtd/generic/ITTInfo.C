/*
 * E.S.O. - VLT project 
 * "@(#) $Id: ITTInfo.C,v 1.5 2005/02/02 01:43:03 brighton Exp $"
 *
 * ITTInfo.C - member routines for class ITTInfo
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * Peter W. Draper 14/07/98  Modified interpolate to use last value 
 *                           (makes last colour pure).
 * pbiereic        17/02/03  Added 'using namespace std'. Removed ::std specs.
 */
static const char* const rcsId="@(#) $Id: ITTInfo.C,v 1.5 2005/02/02 01:43:03 brighton Exp $";


using namespace std;
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cstring>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "ITTInfo.h"
#include "error.h"
#include "util.h"
#include "define.h"

// list of itts, used for cache
ITTInfo* itts_ = NULL;


/*
 * constructor - arguments are the name of the itt
 * and an array of itt values. Both are assumed to
 * have been allocated.
 */
ITTInfo::ITTInfo(char* name, double* value)
    : value_(value),
      name_(name), 
      next_(itts_)
{
    // make this the head of the list
    itts_ = this;
}


/*
 * destructor
 */
ITTInfo::~ITTInfo()
{
    // remove from list
    if (this == itts_) {
	itts_ = next_;
    } 
    else {
	for (ITTInfo* m = itts_; m; m = m->next_) {
	    if (m->next_ == this) {
		m->next_ = next_;
		break;
	    }
	}
    }
}



/* 
 * read a ITT from a file (if needed) and return a new instance for it
 */
ITTInfo* ITTInfo::get(char* filename) 
{
    // just use the basename to identify the colormap
    char* name = strdup(fileBasename(filename));
    
    // see if we read this one already
    ITTInfo* m;
    for (m = itts_; m; m = m->next()) 
	if (strcmp(m->name(), name) == 0)
	    break;
    if (m) {
      free( name );
      return m;
    }

    // have to read file
    ifstream f(filename);
    if (! f) {
      free( name );
      error("could not open ITT file: ", filename);
      return (ITTInfo*) NULL;
    }

    double* value = new double[MAX_ITT];
    if (! value) {
      free( name );
      error("could not allocate ITT color table");
      return (ITTInfo*) NULL;
    }
    for (int i = 0; i < MAX_ITT; i++) {
	f >> value[i];
    }
    if (! f) {
      free( name );
      error("error reading ITT file: ", filename);
      return (ITTInfo*) NULL;
    }

    m = new ITTInfo(name, value);
    if (! m) {
      error("could not create ITT");
    }
    free( name );
    return m;
}


/* 
 * write a list of loaded itt files to the given stream
 * separated by spaces
 */
void ITTInfo::list(ostream& os) 
{
    ITTInfo* m;
    for (m = itts_; m; m = m->next()) 
	os << m->name() << " ";
}


/* 
 * Copy the rgb color values from src to dest and interpolate based 
 * on the ITT table and the count of available colors
 */
void ITTInfo::interpolate(XColor* src, XColor* dest, int colorCount) 
{
    int c = colorCount - 1;
    int index, value;
    
    for (int i=0; i<colorCount; i++) {
	index = (i * (MAX_ITT - 1))/c;
        // PWD: modify to pick up last value.
	value = (unsigned char)((value_[index]*c)+0.5);
	dest[i].red = src[value].red;
	dest[i].green = src[value].green;
	dest[i].blue = src[value].blue;
    }
}


/* 
 * Copy the rgb color values from src to dest as above,
 * and also scale the ITT values by the given amount
 */
void ITTInfo::scale(int amount, XColor* src, XColor* dest, int colorCount) 
{
    int c = colorCount - 1;
    int n = MAX_ITT-1;
    int index, value;
    
    int start = min(amount, colorCount/2);
    int end = colorCount - start;
    if (end <= start)
	end = start+1;
    int dist = end - start + 1;
    
    for (int i=0; i<colorCount; i++) {
        if (i >= start && i <= end) {
            index = ((i-start)*n)/dist;
            if (index < 0)
                index = 0;
            else if (index > n)
                index = n;
	} 
	else if (i < start) {
            index = 0;
	} 
	else {
	    index = n;
	}
	value = (unsigned char)(value_[index]*c);
	dest[i].red = src[value].red;
	dest[i].green = src[value].green;
	dest[i].blue = src[value].blue;
    }
}

