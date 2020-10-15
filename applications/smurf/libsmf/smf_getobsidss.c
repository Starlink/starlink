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
*     result = smf_getobsidss( AstFitsChan *hdr, char * obsid,
*              dim_t szobs, char * obsidss, dim_t szss, int *status );

*  Arguments:
*     hdr = AstFitsChan * (Given)
*        The FITS header to be read.
*     obsid = char * (Given)
*        Buffer to receive Observation ID. Can be NULL.
*     szobs = dim_t (Given)
*        Size of "obsid" buffer.
*     obsidss = char * (Given)
*        Buffer to receive subsystem observation ID. Can be NULL.
*     szss = dim_t (Given)
*        Size of "obsidss" buffer.
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Returned Value:
*     Returns the pointer supplied for "obsidss". This allows the routine
*     to be used inline. Will return NULL if "obsidss" is NULL. Returns
*     NULL on error.

*  Description:
*     Obtain the observation ID and the subsystem observation ID. If either
*     argument is NULL they will not be returned. If both are NULL, there
*     is an error. If the FitsChan does not contain an OBSIDSS header, the
*     returned value is formed by concatenating the OBSID and SUBSYSNR
*     headers (an error is reported if either of these are not present in
*     the FitsChan).

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     27-MAR-2008 (DSB)
*        Initial version. Refactored from code by Tim Jenness in
*        smf_fits_outhdr.c).
*     31-JUL-2008 (TIMJ):
*        No longer use a static buffer. Also give option of retrieving
*        OBSID.
*     27-AUG-2008 (DSB):
*        Correct "sizeof(obsidss)" to "szss" when calling one_strcat and
*        one_strlcpy.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}

*-
*/
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "star/one.h"

#include "smf.h"
#include <string.h>

#define FUNC_NAME "smf_getobsidss"

char *smf_getobsidss( AstFitsChan *hdr, char * obsid, dim_t szobs,
                      char * obsidss, dim_t szss, int *status ) {

/* Local Variables: */
  char lobsid[SZFITSTR];  /* somewhere to put the obsid */
  char *obspnt = NULL;    /* Local buffer or supplied buffer */
  dim_t size;            /* Size of obsid buffer */
  char *value = NULL;     /* Pointer to static buffer containing header value */

   /* Check the inherited status */
   if ( *status != SAI__OK ) return NULL;

   /* Check for NULL hdr */

   if( !hdr ) {
     *status = SAI__ERROR;
     errRep( " ", FUNC_NAME ": Must have non-NULL input hdr pointer"
             " (possible programming error)", status);
     return NULL;
  }


   /* Get the OBSID header, use either local buffer or supplied buffer */
   if (obsid && szobs > 0) {
     obspnt = obsid;
     size = szobs;
   } else {
     obspnt = lobsid;
     size = sizeof(lobsid);
   }
   if (astGetFitsS( hdr, "OBSID", &value)) {
     one_strlcpy( obspnt, value, size, status);
   } else {
     /* OBSID is important if we have been given the external buffer */
     if (obsid && *status == SAI__OK) {
       *status = SAI__ERROR;
       errRep( " ", "Could not find OBSID FITS header", status);
     }
     obspnt[0] = '\0';
   }

   /* Now find OBSIDSS if we need it */
   if (*status == SAI__OK && obsidss) {

     obsidss[0] = '\0';

     /* If we have a OBSIDSS header, return it. */
     if( astGetFitsS( hdr, "OBSIDSS", &value ) ) {
       one_strlcpy( obsidss, value, szss, status );

/* Otherwise, try to form the equivalent OBSIDSS value by concatenating
   OBSID and SUBSYSNR. Note, SCUBA-2 will not have SUBSYSNR but it will
   always have OBSIDSS, so we should never end up in this branch. */
     } else if( strlen(obspnt) ) {

       /* copy the string into the SS buffer */
       one_strlcpy( obsidss, obspnt, szss, status );
       one_strlcat( obsidss, "_", szss, status );

       if( astGetFitsS( hdr, "SUBSYSNR", &value ) ) {
         one_strlcat(obsidss, value, szss, status );

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

   }

   /* Return the supplied obsidss buffer */
   if (*status != SAI__OK) {
     return NULL;
   } else {
     return obsidss;
   }
}

