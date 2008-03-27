/*
*+
*  Name:
*     smf_getobsidss

*  Purpose:
*     Extract the OBSIDSS value from a FITS header.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     result = smf_getobsidss( AstFitsChan *hdr, int *status );

*  Arguments:
*     hdr = AstFitsChan * (Given)
*        The FITS header to be read.
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Returned Value:
*     A pointer to a static buffer containing the OBSIDSS value. Note,
*     the contents of this buffer will change each time this function is
*     invoked, so a copy of the returned string should be taken if the
*     string is needed beyond the next invocation of this function. A
*     pointer to a null string is returned if an error occurs.

*  Description:
*     This function returns a pointer to a static buffer containing the
*     OBSIDSS value read from the supplied FitsChan. If the FitsChan does
*     not contain an OBSIDSS header, the returned value is formed by
*     concatenating the OBSID and SUBSYSNR headers (an error is reported
*     if either of these are not present in the FitsChan).

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     27-MAR-2008 (DSB)
*        Initial version. Refactored from code by Tim Jenness in
*        smf_fits_outhdr.c).
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}

*-
*/
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include <string.h>

const char *smf_getobsidss( AstFitsChan *hdr, int *status ) {

/* Local Variables: */
   static char result[81];  /* Static buffer for returned string */
   char *value = NULL;      /* Pointer to static buffer containing header value */
   int nc;                  /* Number of characters written */

/* Initialise the returned string to a null string. */
   result[ 0 ] = 0;

/* Check the inherited status */
   if ( *status != SAI__OK ) return result;

/* If we have a OBSIDSS header, return it. */
   if( astGetFitsS( hdr, "OBSIDSS", &value ) ) {
      strcpy( result, value );

/* Otherwise, try to form the equivalent OBSIDSS value by concatenating
   OBSID and SUBSYSNR. Note, SCUBA-2 will not have SUBSYSNR but it will
   always have OBSIDS, so we should never end up in this branch. */
   } else if( astGetFitsS( hdr, "OBSID", &value ) ) {

/* We need to copy the string returned by astGetFitsS immediately since
   the pointer points to a static buffer that will be changed by the next 
   call to astGetFitsS. */
      nc = sprintf( result, "%s_", value );

      if( astGetFitsS( hdr, "SUBSYSNR", &value ) ) {
         strcpy( result + nc, value );

      } else if( *status == SAI__OK) {
         *status = SAI__ERROR;
         errRep( "", "Could not determine OBSIDSS value since OBSIDSS and "
                 "SUBSYSNR FITS headers are both missing.", status );
      }

   } else if( *status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( "", "Could not determine OBSIDSS value since OBSIDSS and "
              "OBSID FITS headers are both missing.", status );
   }

/* Return the result, or a null string iof an error has occurred. */
   return ( *status == SAI__OK ) ? result : "";
}

