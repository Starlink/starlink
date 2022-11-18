/*
*+
*  Name:
*     smf_fcf

*  Purpose:
*     Find the nominal beam FCF at the date of the supplied header.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     double smf_fcf( const smfHead hdr, int *status )

*  Arguments:
*     hdr = const smfHdr* (Given)
*        Header struct. Assumed to contain a FitsChan in the hdr slot
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned value:
*     The nominal beam FCF at the date given by the UTDATE header (Jy/beam/pW).

*  Description:
*     Returns the  nominal beam FCF at the date given by the UTDATE header.
*     If UTDATE is not present, use DATE-OBS. A value of VAL__BADD is
*     returned (without error) if the FCF canot be determined. A value of
*     VAL__BADD is also returned if an error occurs.

*  Authors:
*     David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     19-JAN-2022 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2022 East Asian Observatory
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "sae_par.h"
#include "prm_par.h"
#include "star/pal.h"
#include "mers.h"

/* SMURF includes */
#include "libsmf/smf.h"

double smf_fcf( const smfHead * hdr, int *status ){

/* Local Variables: */
   char filter[81];
   double dateobs;
   double fd;
   double result;
   int id;
   int im;
   int iy;
   int j;
   int there;
   int utdate;

/* Initialise */
   result = VAL__BADD;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Validate the header. */
   if( !smf_validate_smfHead(hdr, 1, 0, status) ) return result;

/* If the UTDATE keyword is present in header and has a defined value,
   get it as an integer value. */
   utdate = 0;
   if( astTestFits( hdr->fitshdr, "UTDATE", &there ) ) {
      (void) astGetFitsI( hdr->fitshdr, "UTDATE", &utdate );

/* Otherwise, get the value of DATE-OBS as a UTC MJD value. */
   } else {
      smf_find_dateobs( hdr, &dateobs, NULL, status );

/* Extract the UTDATE integer from it. */
      if( *status == SAI__OK ) {
         palDjcl( dateobs, &iy, &im, &id, &fd, &j );
         if( j == 0 ) utdate = id + 100*im + 10000*iy;
      }
   }

/* If the UTDATE was found, return the appropriate FCF. */
   if( utdate > 0 ){

/* Get the filter (450 or 850). */
      smf_fits_getS( hdr, "FILTER", filter, sizeof(filter), status);
      if( *status == SAI__OK ) {

/* Choose the FCF based on date and filter. */
         if( !strcmp( filter, "450" ) ){
            if( utdate < 20180630 ) {
               result = 531.0;
            } else {
               result = 472.0;
            }
         } else if( !strcmp( filter, "850" ) ){
            if( utdate < 20161101 ){
               result = 525.0;
            } else if( utdate < 20180630 ){
               result = 516.0;
            } else {
               result = 495.0;
            }
         } else {
            *status = SAI__ERROR;
            errRepf( " ", "smf_fcf: unexpected FILTER value '%s' found in "
                     "FITS header - expected '450' or '850'.", status, filter );
         }
      }
   }

/* Return the result. */
   return result;
}

