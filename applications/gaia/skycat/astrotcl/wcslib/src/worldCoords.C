/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: worldCoords.C,v 1.2 2005/02/02 01:43:03 brighton Exp $
 *
 * worldCoords.C - C interface implementation for C++ class WorldCoords
 * 
 * See the man page for a complete description.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  13 Oct 95  Created
 */
static const char* const rcsId="@(#) $Id: worldCoords.C,v 1.2 2005/02/02 01:43:03 brighton Exp $";



// include the C++ and C interfaces
#include "WorldCoords.hxx"
extern "C" {
#include "worldCoords.h"
}


/* 
 * copy the C++ WorldCords class object to the C WC struct
 * and return a pointer to the C WC struct.
 */
static WC* wcCopy(const WorldCoords& wcs, WC* wc)
{
    if (!wc)
	return NULL;

    wc->ra.hours = wcs.ra().hours();
    wc->ra.min = wcs.ra().min();
    wc->ra.sec = wcs.ra().sec();
    wc->ra.val = wcs.ra().val();

    wc->dec.hours = wcs.dec().hours();
    wc->dec.min = wcs.dec().min();
    wc->dec.sec = wcs.dec().sec();
    wc->dec.val = wcs.dec().val();

    return wc;
}


/* 
 * initialize null world coordinates 
 * and return a pointer to the WC struct.
 */
extern "C" 
WC* wcInitNull(WC* wc)
{
    return wcCopy(WorldCoords(), wc);
}


/* 
 * return true if the given coords are null
 */
extern "C" 
int wcIsNull(WC* wc)
{
    return (wc->ra.val == WCS_NULL 
	    || wc->dec.val == WCS_NULL);
}


/* 
 * initialize from RA, DEC in H:M:S D:M:S format 
 * and return a pointer to the WC struct.
 */
extern "C" 
WC* wcInitFromHMS(WC* wc, 
		  int rh, int rm, double rs, 
		  int dd, int dm, double ds, 
		  double equinox)
{
    return wcCopy(WorldCoords(rh, rm, rs, dd, dm, ds, equinox), wc);
}


/* 
 * initialize from RA, DEC in degrees in floating pt format 
 * and return a pointer to the WC struct.
 */
extern "C" 
WC* wcInitFromDeg(WC* wc, double ra, double dec, double equinox)
{
    return wcCopy(WorldCoords(ra, dec, equinox), wc);
}


/* 
 * initialize world coords from RA and DEC in string format "H:M:S", "D:M:S" 
 */
extern "C" 
WC* wcInitFromStrings(WC* wc, char* ra, char* dec, double equinox)
{
    return wcCopy(WorldCoords(ra, dec, equinox), wc);
}


/* 
 * print RA and DEC to the given buffers in the given equinox 
 */
void wcPrint(WC* wc, char* ra_buf, char* dec_buf, double equinox)
{
    if (wc) {
	WorldCoords tmp(wc->ra.val*15, wc->dec.val);
	tmp.print(ra_buf, dec_buf, equinox);
    }
}

