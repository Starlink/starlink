/*
*+
*  Name:
*     smf_updateprov

*  Purpose:
*     Update the output NDF provenance to include a new input NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_updateprov( int ondf, const smfData *data, int indf, 
*                          const char *creator, int *status )

*  Arguments:
*     ondf = int (Given)
*        The output NDF identifier.
*     data = const smfData * (Given)
*        Pointer to the structure describing the current input NDF. If
*        NULL, then the "indf" is used instead.
*     indf = int (Given)
*        NDF identifier for the current input NDF. Only accessed if "data"
*        is NULL.
*     creator = const char * (Given)
*        A string such as "SMURF:MAKECUBE" indicating the calling app.
*     status = int * (Given and Returned)
*        Inherited status value. 

*  Description:
*     This function records the current input NDF as a parent of the
*     output NDF. It includes the input OBSIDSS value in the output
*     provenance information.

*  Authors:
*     David S Berry (JAC, UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-NOV-2007 (DSB):
*        Initial version.
*     15-APR-2008 (DSB):
*        Use smf_getobsidss to get the OBSIDSS value from the FitsChan.
*        This means that an OBSIDSS value can be formed from OBSID and 
*        SUBSYSNR headers if OBSIDSS is not present in the FitsChan.
*     25-APR-2008 (DSB):
*        Added "indf" and "creator" arguments. Changed to retain
*        information about ancestors if one of the ancestors refers to 
*        the OBSIDSS value of the input NDF.
*     31-JUL-2008 (TIMJ):
*        Use thread-safe obsidss API.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2008 Science & Technology Facilities Council.
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

/* Starlink includes */
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"

/* SMURF includes */
#include "libsmf/smf.h"

#include <string.h>

void smf_updateprov( int ondf, const smfData *data, int indf, 
                     const char *creator, int *status ){

/* Local Variables */
   AstFitsChan *fc = NULL;      /* AST FitsChan holding input FITS header */
   HDSLoc *cloc = NULL;         /* Locator for HDS component */
   HDSLoc *prov = NULL;         /* Locator for ancestor provenance info */
   HDSLoc *tloc = NULL;         /* Locator for temp HDS storage */
   char value[ 256 ];           /* Buffer for ancestor's OBSIDSS value */
   char obsidssbuf[SZFITSCARD]; /* OBSIDSS value in input file */
   char *obsidss = NULL;        /* Pointer to OBSIDSS buffer */
   int found;                   /* Was OBSIDSS value found in an input ancestor? */
   int ianc;                    /* Ancestor index */
   int isroot;                  /* Ignore any ancestors in the input NDF? */
   int there;                   /* Was the required component found? */

/* Check the inherited status */
   if( *status != SAI__OK ) return;

/* Get a FitsChan holding the contents of the input NDF FITS extension. */
   if( !data ) {
      kpgGtfts( indf, &fc, status );
   } else {
      fc = data->hdr->fitshdr;
   }

/* Get the input NDF identifier. */
   if( data && data->file && 
       data->file->ndfid != NDF__NOID) indf = data->file->ndfid;

/* Initially, assume that we should include details of ancestor NDFs. */
   isroot = 0;

/* Get the OBSIDSS keyword value from the input FITS header. */
   obsidss = smf_getobsidss( fc, NULL, 0, obsidssbuf,
                             sizeof(obsidssbuf), status );
   if( obsidss ) {

/* Search through all the ancestors of the input NDF (including the input
   NDF itself) to see if any of them refer to the same OBSIDSS value. */
      found = 0;
      ianc = 0; 
      ndgGtprv( indf, ianc, &prov, status );
      while( prov ) {
         datThere( prov, "MORE", &there, status );
         if( there ) {
            datFind( prov, "MORE", &tloc, status );
            datThere( tloc, "OBSIDSS", &there, status );
            if( there ) {
               datFind( tloc, "OBSIDSS", &cloc, status );
               datGet0C( cloc, value, 255, status );
               found = !strcmp( obsidss, value );
               datAnnul( &cloc, status );
            }
            datAnnul( &tloc, status );
         }
         datAnnul( &prov, status );
         if( ! found ) ndgGtprv( indf, ++ianc, &prov, status );
      }

/* If the OBSIDSS value was not found in any ancestor, then we add it
   now. So put the OBSIDSS keyword value in an HDS structure that will be 
   stored with the output provenance information. */
      if( ! found ) {
         datTemp( "MORE", 0, NULL, &tloc, status );
         datNew0C( tloc, "OBSIDSS", strlen( obsidss ), status );
         datFind( tloc, "OBSIDSS", &cloc, status );
         datPut0C( cloc, obsidss, status );
         datAnnul( &cloc, status );

/* Ignore ancestor NDFs if none of them referred to the correct OBSIDSS. */
         isroot = 1;
      } 
   }

/* Update the provenance for the output NDF to include the input NDF as
   an ancestor. Indicate that each input NDF is a root NDF (i.e. has no 
   parents). */
   ndgPtprv( ondf, indf, tloc, isroot, creator, status );

/* Free resources. */
   if( tloc ) datAnnul( &tloc, status );
   if( !data ) fc = astAnnul( fc );
}

