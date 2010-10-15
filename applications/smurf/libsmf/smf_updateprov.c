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
*     modprov = NdgProvenance ** (Given & Returned)
*        If NULL the provenance will be read by this routine and the
*        file will be immediately updated. The provenance will then
*        be freed. If a pointer is provided and points to a NULL
*        provenance the provenance will be read and modified but the
*        file will not be updated. If the pointer is provided and
*        points to a provenance then that provenance will be modified
*        without reading the output NDF.
*     status = int * (Given and Returned)
*        Inherited status value.

*  Description:
*     This function records the current input NDF as a parent of the
*     output NDF. It includes the input OBSIDSS value in the output
*     provenance information.

*  Notes:
*     - Always propagates provenance if we have OBSIDSS available or if
*     we have ancestors in the input provenance. Does not propagate if
*     we have no ancestors and no OBSIDSS.
*     - If an external provenance pointer is provided the caller
*     is responsible for writing the provenance to the output file.

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
*     29-JUN-2009 (DSB):
*        Use new NDG provenance API.
*     2009-11-27 (TIMJ):
*        Only store provenance if we are a root (OBSIDSS is available)
*        or we have provenance to propagate. Allow there to be no
*        fits header at all.
*     2010-10-15 (TIMJ):
*        Allow provenance structure to be retained between calls.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2009 Science & Technology Facilities Council.
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
#include "star/ndg.h"

/* SMURF includes */
#include "libsmf/smf.h"

#include <string.h>

void smf_updateprov( int ondf, const smfData *data, int indf,
                     const char *creator, NdgProvenance **modprov, int *status ){

/* Local Variables */
   AstFitsChan *fc = NULL;      /* AST FitsChan holding input FITS header */
   AstKeyMap *anc = NULL;       /* KeyMap holding ancestor info */
   AstKeyMap *tkm = NULL;       /* KeyMap holding contents of MORE */
   NdgProvenance *prov = NULL;  /* Pointer to input provenance structure */
   char *obsidss = NULL;        /* Pointer to OBSIDSS buffer */
   char obsidssbuf[SZFITSCARD]; /* OBSIDSS value in input file */
   const char *vptr = NULL;     /* Pointer to OBSIDSS string */
   int found;                   /* Was OBSIDSS value found in an input ancestor? */
   int ianc;                    /* Ancestor index */
   int isroot;                  /* Ignore any ancestors in the input NDF? */

/* Check the inherited status */
   if( *status != SAI__OK ) return;

/* Get a FitsChan holding the contents of the input NDF FITS extension. */
   if( !data ) {
      kpgGtfts( indf, &fc, status );
   } else if (data->hdr) {
      fc = data->hdr->fitshdr;
   }

/* Get the input NDF identifier. */
   if( data && data->file &&
       data->file->ndfid != NDF__NOID) indf = data->file->ndfid;

/* Get a structure holding provenance information from indf. */
   prov = ndgReadProv( indf, " ", status );

/* Initially, assume that we should include details of ancestor NDFs. */
   isroot = 0;

/* Get the OBSIDSS keyword value from the input FITS header if we have a FITS
   header. There can be cases (eg in STACKFRAMES) where we do not have a
   FITS header so handle that gracefully. */
   if (fc) obsidss = smf_getobsidss( fc, NULL, 0, obsidssbuf,
                                     sizeof(obsidssbuf), status );
   if( obsidss ) {

/* Search through all the ancestors of the input NDF (including the input
   NDF itself) to see if any of them refer to the same OBSIDSS value. */
      found = 0;
      ianc = 0;
      anc = ndgGetProv( prov, ianc, NULL, status );
      while( anc ) {
         if( astMapGet0A( anc, "MORE", &tkm ) ) {
            if( astMapGet0C( tkm, "OBSIDSS", &vptr ) ) {
               found = !strcmp( obsidss, vptr );
            }
            tkm = astAnnul( tkm );
         }
         anc = astAnnul( anc );
         if( ! found ) anc = ndgGetProv( prov, ++ianc, NULL, status );
      }

/* If the OBSIDSS value was not found in any ancestor, then we add it
   now. So put the OBSIDSS keyword value in an AST KeyMap that will be
   stored with the output provenance information. */
      if( ! found ) {
         tkm = astKeyMap( " " );
         astMapPut0C( tkm, "OBSIDSS", obsidss, NULL );

/* Ignore ancestor NDFs if none of them referred to the correct OBSIDSS. */
         isroot = 1;
      }
   }

/* Update the provenance for the output NDF to include the input NDF as
   an ancestor. Indicate that each input NDF is a root NDF (i.e. has no
   parents). Do nothing if we have no provenance to propagate, unless
   we have root information and are propagating that. */
   if ( ndgCountProv( prov, status ) > 0 || tkm) {
     NdgProvenance *oprov = NULL;
     if (modprov && *modprov) {
       oprov = *modprov;
     } else {
       oprov = ndgReadProv( ondf, creator, status );
       if (modprov) *modprov = oprov;
     }
     ndgPutProv( oprov, indf, NULL, tkm, isroot, status );
     /* do not update the file or free provenance if we
        are using an external provenance struct or returning
        it to the caller */
     if (!modprov) {
       ndgWriteProv( oprov, ondf, 1, status );
       oprov = ndgFreeProv( oprov, status );
     }
   }

/* Free resources. */
   if( prov ) prov = ndgFreeProv( prov, status );
   if( tkm ) tkm = astAnnul( tkm );
   if( !data && fc ) fc = astAnnul( fc );
}

