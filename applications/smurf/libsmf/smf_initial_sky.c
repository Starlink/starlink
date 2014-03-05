/*
*+
*  Name:
*     smf_initial_sky

*  Purpose:
*     Subtract the initial sky estimate from the cleaned data.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_initial_sky( ThrWorkForce *wf, AstKeyMap *keymap,
*                          smfDIMMData *dat, int *iters, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     keymap = AstKeyMap * (Given)
*        Configuration parameters that control the map-maker.
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation.
*     iters = int * (Returned)
*        If the initial sky NDF was created by a previous run of makemap
*        that was interupted using control-C, "*iters" will be returned
*        holding the number of iterations that were completed before the
*        map was created. Otherwise, -1 is returned in "*iters".
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     Non-zero if an initial sky estimate was subtracted from the data.
*     Zero otherwise.

*  Description:
*     If the IMPORTSKY configuration parameter is set, this function
*     reads a sky map in using the ADAM parameter specified by the
*     IMPORTSKY. It then stores this map as the current map in the
*     iterative map-making process. It then samples the map at each
*     bolometer position and subtracts the bolometer value from the
*     current residuals.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     22-OCT-2012 (DSB):
*        Original version.
*     3-JUL-2013 (DSB):
*        - Added argument iters.
*        - Ensure the bad bits mask is set so that the mapped NDF data
*        array is masked by the quality array.
*        - Import variance from supplied NDF if available.
*     10-DEC-2013 (DSB):
*        - Re-structured to avoid setting map pixels bad if they are
*        flagged only by FLT or COM masking. That is, map pixels should
*        be bad if and only if they are flagged by AST masking.
*        - Report an error if the NDF contains unexpected quality names.
*        - Clear and RING or COM flags in the quality array of the time series data.
*     23-JAN-2014 (DSB):
*        Mask the AST map in smf_calcmodel_ast as normal, rather than masking it here.
*     4-MAR-2014 (DSB):
*        Do not clear and RING or COM flags in the quality array of the time
*        series data (it will never be present anyway).
*     5-MAR-2014 (DSB):
*        Ignore the quality in the supplied map.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012-2013 Science & Technology Facilities Council.
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
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "dat_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"

int smf_initial_sky( ThrWorkForce *wf, AstKeyMap *keymap, smfDIMMData *dat,
                     int *iters, int *status ) {

/* Local Variables: */
   char refparam[ DAT__SZNAM ];/* Name for reference NDF parameter */
   const char *cval;          /* The IMPORTSKY string value */
   double *ptr;               /* Pointer to NDF Data array */
   double *vptr;              /* Pointer to NDF Variance array */
   int indf1;                 /* Id. for supplied reference NDF */
   int indf2;                 /* Id. for used section of reference NDF */
   int nel;                   /* Number of mapped NDF pixels */
   int result;                /* Returned flag */
   int there;                 /* Is there a smurf extension in the NDF? */
   size_t size;               /* Size of mapped array */

/* Initialise the returned value to indicate no sky has been subtractred. */
   result = 0;

/* Assume the sky map was not created by an interupted previous run of
   makemap. */
   *iters = -1;

/* Check inherited status. */
   if( *status != SAI__OK ) return result;

/* Begin an AST context. */
   astBegin;

/* The IMPORTSKY config parameter should have the name of the ADAM
   parameter to use for acquiring the NDF that contains the initial sky
   estimate. If IMPORTSKY is "1", use REF. */
   cval = NULL;
   astMapGet0C( keymap, "IMPORTSKY", &cval );
   if( cval ) {
      if( !astChrMatch( cval, "REF" ) &&
          !astChrMatch( cval, "MASK2" ) &&
          !astChrMatch( cval, "MASK3" ) ) {
         astMapGet0I( keymap, "IMPORTSKY", &result );
         cval = ( result > 0 ) ? "REF" : NULL;
      }
      if( cval ) {
         result = 1;
         strcpy( refparam, cval );
         astChrCase( NULL, refparam, 1, 0 );
      }
   }

/* Do nothing more if we are not subtracting an initial sky from the data. */
   if( result ) {

/* Begin an NDF context. */
      ndfBegin();

/* Get an identifier for the NDF using the associated ADAM parameter. */
      ndfAssoc( refparam, "READ", &indf1, status );

/* Tell the user what we are doing. */
      ndfMsg( "N", indf1 );
      msgOut( "", "Using ^N as the initial guess at the sky", status );

/* Get a section from this NDF that matches the bounds of the map. */
      ndfSect( indf1, 2, dat->lbnd_out, dat->ubnd_out, &indf2, status );

/* Ensure masked values are not set bad in the mapped data array. */
      ndfSbb( 0, indf2, status );

/* Map the data array section, and copy it into the map buffer. */
      ndfMap( indf2, "DATA", "_DOUBLE", "READ", (void **) &ptr, &nel, status );
      if( *status == SAI__OK ) {
         memcpy( dat->map, ptr, dat->msize*sizeof(*ptr));
      }

/* Map the variance array section, and copy it into the map buffer. */
      ndfState( indf2, "VARIANCE", &there, status );
      if( there ) {
         ndfMap( indf2, "VARIANCE", "_DOUBLE", "READ", (void **) &vptr, &nel, status );
         if( *status == SAI__OK ) {
            memcpy( dat->mapvar, vptr, dat->msize*sizeof(*vptr));
         }
      }

/* If the NDF was created by a previous run of makemap that was interupted
   using control-C, it will contain a NUMITER item in the smurf extension,
   which gives the number of iterations that were completed before the
   map was created. Obtain and return this value, if it exists. */
      ndfXstat( indf1, SMURF__EXTNAME, &there, status );
      if( there ) ndfXgt0i( indf1, SMURF__EXTNAME, "NUMITER", iters,
                            status );

/* End the NDF context. */
      ndfEnd( status );

/* Indicate the map arrays within the supplied smfDIMMData structure now
   contain usable values. We need to do this before calling
   smf_calcmodel_ast below so that the right mask gets used in
   smf_calcmodel_ast. */
      dat->mapok = 1;

/* Apply any existinction correction to the cleaned bolometer data. */
      if( dat->ext ) smf_calcmodel_ext( wf, dat, 0, keymap, dat->ext, 0,
                                        status);

/* Sample the above map at the position of each bolometer sample and
   subtract the sampled value from the cleaned bolometer value. */
      smf_calcmodel_ast( wf, dat, 0, keymap, NULL, SMF__DIMM_PREITER, status);

/* Remove any existinction correction to the modifed bolometer data. */
      if( dat->ext ) smf_calcmodel_ext( wf, dat, 0, keymap, dat->ext,
                                        SMF__DIMM_INVERT, status);
   }

/* End the AST context. */
   astEnd;

/* Return the pointer to the boolean mask. */
   return result;
}

