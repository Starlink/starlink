/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id$" 
 *
 * WCS.C - method definitions for class WCS, a reference counting
 *         wrapper class for managing world coordinates for an image.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  30 Sep 96  Created
 *                 17 Mar 98  Made WCSRep an abstract base class to allow 
 *                            new implementations of WCS. Renamed old WCSRep
 *                            to SAOWCS (based on Doug Mink's saoimage version).
 * abrighto        02/01/06   Renamed WCS.C to WCSRep.C to avoid name conflict
 *                            with wcs.c on file systems that ignore case
 */
static const char* const rcsId="@(#) $Id$";


#include <cstdlib>
#include <cstring>
#include "error.h"
#include "WCSRep.h"


/*
 * constructor for internal representation 
 */
WCSRep::WCSRep() 
    : status_(0),
      refcnt_(1) 
{
}


/*
 * destructor for internal representation 
 */
WCSRep::~WCSRep() 
{
}


/*
 * copy constructor - increment the reference count...
 */
WCS::WCS(const WCS& wcs) 
    : rep_(wcs.rep_)
{
    if (rep_) 
	rep_->refcnt_++;
}


/*
 * destructor - delete if there are no more references.
 */
WCS::~WCS() 
{
    if (rep_ && --rep_->refcnt_ <= 0) 
	delete rep_;
}


/*
 * assignment operator
 */
WCS& WCS::operator=(const WCS& wcs)
{
    if (wcs.rep_)
	wcs.rep_->refcnt_++;		// protect against "wcs = wcs"
    if (rep_ && --rep_->refcnt_ <= 0)
	delete rep_;
    rep_ = wcs.rep_;
    return *this;
}

