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
*                          smfDIMMData *dat, double chunkfactor, int *iters,
*                          int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     keymap = AstKeyMap * (Given)
*        Configuration parameters that control the map-maker.
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation.
*     chunkfactor = double (Given)
*        The calibration correction factor to use for the current chunk.
*        The values sampled from the supplied initial sky map are divided
*        by this factor before being subtracted from the time-series data.
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
*     10-MAR-2014 (DSB):
*        Update the Quality component of the supplied NDF to match the
*        mask created by smf_calcmodel_ast.
*     8-SEP-2014 (DSB):
*        Changed to support freezing of masks within SKYLOOP. Initial
*        masks are now made from the Quality array in the supplied NDF.
*        These will be immediately over-written (within smf_calcmodel_ast)
*        with new masks unless the mask is frozen.
*     10-APR-2018 (DSB):
*        Added parameter "chunkfactor".
*     13-FEB-2020 (DSB):
*        Annull the error if the supplied map contains a quality component
*        but does not contain any quality name information to define the use
*        of each quality bit. In such cases, the quality array is probably full
*        of zeros and so can be ignored.
*     10-JUN-2020 (DSB):
*        - Ignore quality values that do not correspond to a good data
*        value. This is because they can be spurious (e.g. created by
*        mosaicing a set of other maps using picard) but will still trigger
*        the creating of a mask, which may mask out the bulk of the map.
*        - Use sections from the WEIGHTS and EXP_TIME NDFs that match the
*        main NDF.
*     11-DEC-2020 (DSB):
*        Cater for cases wher the supplied map does not have the expected NDFs
*        in the SMURF extension.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012-2014 Science & Technology Facilities Council.
*     Copyright (C) 2018 East Asian Observatory.
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
#include "par_err.h"
#include "irq_err.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"

int smf_initial_sky( ThrWorkForce *wf, AstKeyMap *keymap, smfDIMMData *dat,
                     double chunkfactor, int *iters, int *status ){

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
   int update;                /* Was NDF opened for UPDATE access? */
   size_t i;                  /* Loop count */
   size_t junk;               /* Unused value */

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
   if( result && *status == SAI__OK ) {

/* Begin an NDF context. */
      ndfBegin();

/* Get an identifier for the NDF using the associated ADAM parameter.
   First try UPDATE access. If this fails try READ access. */
      ndfAssoc( refparam, "UPDATE", &indf1, status );
      if( *status != SAI__OK && *status != PAR__NULL && *status != PAR__ABORT ){
         errAnnul( status );
         ndfAssoc( refparam, "READ", &indf1, status );
         update = 0;
      } else {
         update = 1;
      }

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
      if( there ) {
         double *tptr;
         int tndf;
         int tndf2;
         HDSLoc *xloc = NULL;
         ndfXgt0i( indf1, SMURF__EXTNAME, "NUMITER", iters, status );

/* Get a locator for the SMURF extension. */
         ndfXloc( indf1, SMURF__EXTNAME, "READ", &xloc, status );

/* Copy the WEIGHTS NDF from the SMURF extension to the mapweight buffer in "dat". */
         datThere( xloc, "WEIGHTS", &there, status );
         if( there ) {
            ndfFind( xloc, "WEIGHTS", &tndf, status );
            ndfSect( tndf, 2, dat->lbnd_out, dat->ubnd_out, &tndf2, status );
            ndfMap( tndf2, "DATA", "_DOUBLE", "READ", (void **) &tptr, &nel, status );
            if( *status == SAI__OK ) {
               memcpy( dat->mapweight, tptr, dat->msize*sizeof(*tptr));
            }
            ndfAnnul( &tndf2, status );
            ndfAnnul( &tndf, status );
         }

/* Copy the EXP_TIME NDF from the SMURF extension to the hitsmaps buffer in
   "dat". Use the step time in the supplied smfData to convert from time
   to hits. */
         datThere( xloc, "EXP_TIME", &there, status );
         if( there ) {
            ndfFind( xloc, "EXP_TIME", &tndf, status );
            ndfSect( tndf, 2, dat->lbnd_out, dat->ubnd_out, &tndf2, status );
            ndfMap( tndf2, "DATA", "_DOUBLE", "READ", (void **) &tptr, &nel, status );
            if( *status == SAI__OK ) {
               double steptime = dat->res[0]->sdata[0]->hdr->steptime;
               for( i = 0; i < dat->msize; i++ ) {
                  if( tptr[ i ] != VAL__BADD ) {
                     dat->hitsmap[ i ] = (int) ( tptr[ i ]/steptime + 0.5 );
                  } else {
                     dat->hitsmap[ i ] = 0;
                  }
               }
            }
            ndfAnnul( &tndf2, status );
            ndfAnnul( &tndf, status );
         }

/* Annul the SMURF extension locator. */
         datAnnul( &xloc, status );
      }

/* If the NDF has a Quality component, import it and create initial AST,
   FLT, PCA, SSN and COM masks from it. These will often be over-ridden by
   new masks calculated with smf_calcmodel_ast below, but will not be
   over-written if the masks have been frozen by xxx.zero_freeze. */
      ndfState( indf2, "Quality", &there, status );
      if( there && dat->mapqual ) {
         ptr = dat->map;
         smf_qual_t *qarray = smf_qual_map( wf, indf2, "Read", NULL, &junk,
                                            status );
         if( *status == SAI__OK ) {
            smf_qual_t *pq = qarray;
            for( i = 0; i < dat->msize; i++,pq++,ptr++ ) {
               if( *ptr != VAL__BADD ) {
                  if( *pq & SMF__MAPQ_AST ) {
                     if( !dat->ast_mask ) dat->ast_mask = astCalloc( dat->msize,
                                                     sizeof( *(dat->ast_mask) ) );
                     (dat->ast_mask)[ i ] = 1;
                  }
                  if( *pq & SMF__MAPQ_FLT ) {
                     if( !dat->flt_mask ) dat->flt_mask = astCalloc( dat->msize,
                                                     sizeof( *(dat->flt_mask) ) );
                     (dat->flt_mask)[ i ] = 1;
                  }
                  if( *pq & SMF__MAPQ_COM ) {
                     if( !dat->com_mask ) dat->com_mask = astCalloc( dat->msize,
                                                     sizeof( *(dat->com_mask) ) );
                     (dat->com_mask)[ i ] = 1;
                  }
                  if( *pq & SMF__MAPQ_SSN ) {
                     if( !dat->ssn_mask ) dat->ssn_mask = astCalloc( dat->msize,
                                                     sizeof( *(dat->ssn_mask) ) );
                     (dat->ssn_mask)[ i ] = 1;
                  }
                  if( *pq & SMF__MAPQ_PCA ) {
                     if( !dat->pca_mask ) dat->pca_mask = astCalloc( dat->msize,
                                                     sizeof( *(dat->pca_mask) ) );
                     (dat->pca_mask)[ i ] = 1;
                  }
               }
            }

/* If smf_qual_map failed because there was no quality name information
   in the supplied NDF, then annull the error and proceed (i.e. ignore
   the quality component - it is probably full of zeros). */
         } else if( *status == IRQ__NOQNI ) {
            errAnnul( status );
         }
         qarray = astFree( qarray );
      }

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
      smf_calcmodel_ast( wf, dat, 0, keymap, NULL, SMF__DIMM_PREITER,
                         chunkfactor, status);

/* Remove any existinction correction to the modifed bolometer data. */
      if( dat->ext ) smf_calcmodel_ext( wf, dat, 0, keymap, dat->ext,
                                        SMF__DIMM_INVERT, status);

/* If the NDF was opened with UPDATE access, update the quality array in
   the NDF to reflect the AST mask created by smf_calcmodel_ast above. */
      if( update ) {
         smf_qual_t *qarray = astStore( NULL, dat->mapqual, dat->msize*sizeof(*qarray) );
         qarray = smf_qual_unmap( wf, indf2, SMF__QFAM_MAP, qarray,
                                  SMF__Q_GOOD, status );
      }

/* End the NDF context. */
      ndfEnd( status );
   }

/* End the AST context. */
   astEnd;

/* Return the pointer to the boolean mask. */
   return result;
}

