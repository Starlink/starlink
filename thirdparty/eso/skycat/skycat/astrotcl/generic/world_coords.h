#ifndef _worldCoords_h_
#define _worldCoords_h_

/*
 * E.S.O. - VLT project 
 * $Id: world_coords.h,v 1.1.1.1 2006/01/12 16:43:52 abrighto Exp $
 *
 * world_coords.h - C interface to C++ class WorldCoords
 *
 * (Note: C applications must have at least a dummy C++ main and link
 * with C++)
 *
 * See the man page for a complete description.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 *                 02/01/06  Renamed worldCoords.h to world_coords.h to
 *                           avoid name conflict on file systems that ignore case
 */


/* struct representing H:M:S.sss (or D:M:S) value */
typedef struct { 
    int hours;	 
    int min;
    double sec;
    double val;			/* value calculated in degrees */
} WC_HMS;


/* struct representing world coordinates */
typedef struct { 
    WC_HMS ra, dec;
} WC;


/* initialize null world coordinates */
WC* wcInitNull(WC*);

/* return true if the given coords are null */
int wcIsNull(WC* wc);

/* initialize world coords from RA and DEC in string format "H:M:S", "D:M:S" */
WC* wcInitFromStrings(WC*, char* ra, char* dec, double equinox);

/* initialize from RA, DEC in H:M:S D:M:S format */
WC* wcInitFromHMS(WC*, int rh, int rm, double rs, int dd, int dm, double ds, double equinox);

/* initialize from RA, DEC in degrees in floating pt format */
WC* wcInitFromDeg(WC*, double ra, double dec, double equinox);

/* print RA and DEC to the given buffers in the given equinox */
void wcPrint(WC* wc, char* ra_buf, char* dec_buf, double equinox);

#endif /* _worldCoords_h_ */
