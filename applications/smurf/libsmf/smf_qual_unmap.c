/*
*+
*  Name:
*     smf_qual_unmap

*  Purpose:
*     Unmap a quality array, copying values if appropriate

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_qual_t * smf_qual_unmap( ThrWorkForce *wf, int indf, smf_qfam_t family,
*                                  smf_qual_t * qual, int * status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     indf = int (Given)
*        NDF identifier
*     family = smf_qfam_t (Given)
*        Indicate which family of quality bits is represented in "qual".
*     qual = const smf_qual_t * (Given)
*        Pointer to quality data to be unmapped or freed. Should not be used
*        after calling this routine.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     Returns a NULL pointer.

*  Description:
*     Unmaps quality values associated with an NDF. If the internal SMURF quality is
*     the same size as NDF quality (unsigned char) then this will simply
*     be a call to ndfUnmap. If the size differs and the NDF has been opened with
*     WRITE access the QUALITY component will be mapped and the values will be
*     copied after first compressing them to fit in 8 bytes.
*
*     Quality names are stored in the file corresponding to which bits were
*     present and which family was being used.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - The pointer to quality must have been obtained by a call to
*     smf_qual_map.
*     - The quality names of the items written by this routine are recorded
*     using IRQ.
*     - We set badbits mask except when the MAP family is being used. This is
*     because currently MAKEMAP handles badbits mask based on config settings. The
*     fix here is to attach a badbits mask to the quality information.
*     - Attempts to free quality even if status is bad.

*  History:
*     2010-06-17 (TIMJ):
*        Initial version
*     2010-06-29 (TIMJ):
*        Always create new IRQ structure when writing out.
*     2010-07-06 (TIMJ):
*        Add SMF__Q_NOISE
*     2010-07-08 (TIMJ):
*        Enable quality in output file using ndfSbb.
*        Except for map family.
*     2010-10-04 (TIMJ):
*        If we do not know the family just copy the quality
*        to the output without using quality names and just
*        assuming a simple cast to unsigned char will be enough.
*     2011-04-15 (TIMJ):
*        Add SMF__Q_EXT
*     2011-04-26 (DSB):
*        Add SMF__Q_LOWAP
*     2011-05-26 (TIMJ):
*        Attempt to free QUAL memory even if status is bad.
*     2011-09-19 (DSB):
*        Add SMF__Q_BADEF
*     2014-1-13 (DSB):
*        Multi-threaded.
*     2014-2-28(DSB):
*        Add SMF__Q_RING
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010-2014 Science and Technology Facilities Council.
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

#include "smf.h"

#include "ndf.h"
#include "sae_par.h"
#include "ast.h"
#include "star/irq.h"
#include "mers.h"
#include "irq_err.h"
#include "star/thr.h"

#define FUNC_NAME "smf_qual_unmap"

/* Prototypes for local static functions. */
static void smf1_qual_unmap( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfQualUnmapData {
   int operation;
   size_t highbit;
   size_t i1;
   size_t i2;
   size_t lowbit;
   size_t nout;
   size_t nqbits;
   size_t qcount[SMF__NQBITS];
   smf_qual_t *qual;
   unsigned char *qmap;
} SmfQualUnmapData;

smf_qual_t * smf_qual_unmap( ThrWorkForce *wf, int indf, smf_qfam_t family,
                             smf_qual_t * qual, int * status ) {
  int canwrite = 0;   /* can we write to the file? */
  size_t nqbits = 0;  /* Number of quality bits in this family */
  SmfQualUnmapData *job_data = NULL;
  SmfQualUnmapData *pdata;
  int nw;
  size_t step;
  int iw;

  if (*status != SAI__OK) goto CLEANUP;

  /* do nothing if there is no quality */
  if (!qual) return NULL;

  /* if we do not have an NDF identifier we just free the memory */
  if (indf == NDF__NOID) goto CLEANUP;

  /* See if we have WRITE access to the file */
  ndfIsacc( indf, "WRITE", &canwrite, status );

  /* if we have WRITE access and the data were not mapped we have
     to copy to the file. Also check we have a non-NULL input pointer.
     If the data were mapped we still have to make sure the quality names
     are stored. */
  if ( canwrite && qual ) {
    int highbit = -1; /* highest bit used */
    size_t i;
    int itemp;
    int lowbit = -1;  /* Lowest bit used */
    size_t nout;
    int nqual = 0;
    void *qpntr[1];
    size_t qcount[SMF__NQBITS]; /* statically allocate the largest array */
    IRQLocs *qlocs;
    unsigned char * qmap;
    int there;

    ndfMsg( "FILE", indf );
    msgOutif( MSG__DEBUG, "", "Finalising quality for file ^FILE", status);

    if (family == SMF__QFAM_TCOMP ) {
      /* note that TCOMP is not an allowed quality because SMURF should not be
         using it anywhere in a permanent way. */
      *status = SAI__ERROR;
      ndfMsg( "NDF", indf );
      errRepf( "", "Unsupported quality family '%s' for quality unmapping of "
               "file ^NDF", status, smf_qfamily_str(family,status) );
      goto CLEANUP;
    } else if (family == SMF__QFAM_NULL) {
      /* In this case we have to assume that we just cast the quality
         to UBYTE and copy it without changing anything or naming the
         entries. Use a simple type conversion. */
      ndfMap( indf, "QUALITY", "_UBYTE", "WRITE", &qpntr[0], &itemp, status );
      qmap = qpntr[0];
      nout = itemp;

      for (i = 0; i<nout; i++) {
        qmap[i] = qual[i];
      }
      ndfUnmap( indf, "QUALITY", status );

      /* Turn on all quality */
      ndfSbb( 255, indf, status );

      /* we are finished so jump to tidy up */
      goto CLEANUP;
    }

    /* work out how many quality items are in this family */
    nqbits = smf_qfamily_count( family, status );

    /* initialize qcount */
    for (i=0; i<SMF__NQBITS; i++) {
      qcount[i] = 0;
    }

    /* how many pixels in NDF (assumed to be number in quality) */
    ndfSize( indf, &itemp, status );
    nout = itemp;

    /* How many threads do we get to play with */
    nw = wf ? wf->nworker : 1;

    /* Find how many elements to process in each worker thread. */
    step = nout/nw;
    if( step == 0 ) step = 1;

    /* Allocate job data for threads, and store common values. Ensure that the
       last thread picks up any left-over elements.  */
    job_data = astCalloc( nw, sizeof(*job_data) );
    if( *status == SAI__OK ) {
      for( iw = 0; iw < nw; iw++ ) {
        pdata = job_data + iw;
        pdata->i1 = iw*step;
        if( iw < nw - 1 ) {
          pdata->i2 = pdata->i1 + step - 1;
        } else {
          pdata->i2 = nout - 1 ;
        }
        pdata->nqbits = nqbits;
        pdata->qual = qual;
        pdata->nout = nout;
      }
    }

    /* Work out which bits are actually used */
    if (*status == SAI__OK) {
      size_t k;
      /* now we try to be a bit clever. It may be a mistake since we have to
         do multiple passes through "qual". First determine how many quality
         bits are actually set. */

      for( iw = 0; iw < nw; iw++ ) {
        pdata = job_data + iw;
        pdata->operation = 1;
        thrAddJob( wf, 0, pdata, smf1_qual_unmap, 0, NULL, status );
      }
      thrWait( wf, status );

      for( iw = 0; iw < nw; iw++ ) {
        pdata = job_data + iw;
        for( k=0; k<nqbits; k++ ) {
          qcount[k] += pdata->qcount[k];
        }
      }

      /* see how many we got */
      for (k=0; k<nqbits; k++) {

        if ( qcount[k] ) {
          nqual++;
          highbit = k;
          if (lowbit < 0) lowbit = k;
        }
      }
    }

    /* for IRQ we need to ensure the SMURF extension exists so open and annul it if it is missing.
       We are completely rewriting any IRQ information so we have to delete any previously existing
       IRQ extension. */
    irqDelet( indf, status );
    ndfXstat( indf, SMURF__EXTNAME, &there, status );
    if (!there) {
      HDSLoc * smurfloc = NULL;
      /* Create SMURF extension if it does not already exist */
      ndfXnew( indf, SMURF__EXTNAME, SMURF__EXTTYPE, 0, NULL, &smurfloc, status );
      if (smurfloc) datAnnul( &smurfloc, status );
    }
    irqNew( indf, SMURF__EXTNAME, &qlocs, status );

    /* malloced so we need to map and copy over the values. IRQ
       names need to be set BEFORE we copy. */

    /* Map the quality component with WRITE access */
    ndfMap( indf, "QUALITY", "_UBYTE", "WRITE", &qpntr[0], &itemp, status );
    qmap = qpntr[0];

    /* we assume the number of elements in "qual" is the same as in "qmap" */
    if (*status == SAI__OK) {
      size_t k;

      /* if we only have 8 or fewer bits active we can just compress
         by mapping them to the lower 8 bits. This will work if we also
         set the IRQ quality names in the NDF. */
      if (nqual == 0 ) {
        /* easy */
        memset( qmap, 0, nout * smf_dtype_sz( SMF__UBYTE, status ) );
      } else if ( nqual <= 8 ) {
        size_t curbit = 0;

        /* and the quality names. Start at lowbit and go to highbit
           knowing that we have shifted them down so that lowbit in qual
           is bit 0 in NDF. */
        for (k=lowbit; k<=(size_t)highbit; k++) {
          if (qcount[k]) {
            int fixed = 0;             /* is bit fixed? */
            const char * qdesc = NULL; /* Description of quality */
            const char * qstr = NULL;  /* Quality string identifier */
            curbit++;
            qstr = smf_qual_str( family, 1, k, &qdesc, status );

            irqAddqn( qlocs, qstr, 0, qdesc, status );
            irqFxbit( qlocs, qstr, curbit, &fixed, status );
          }
        }

        /* shift them down */
        for( iw = 0; iw < nw; iw++ ) {
          pdata = job_data + iw;
          pdata->operation = 2;
          pdata->qmap = qmap;
          pdata->highbit = highbit;
          pdata->lowbit = lowbit;
          for( k=0; k<nqbits; k++ ) {
            pdata->qcount[k] = qcount[k];
          }
          thrAddJob( wf, 0, pdata, smf1_qual_unmap, 0, NULL, status );
        }
        thrWait( wf, status );

      } else {
        size_t curbit = 0;

        /* Quality names are now needed and we have to write them
           all out because we have not compressed the bits in the
           output quality array we've only compressed the input.
           To limit the number of active bits we'd have to copy the
           compressed bits to the output and then set the quality
           names but IRQ does not let you do that so you would need
           to run through the entire array first counting which bits
           were used. */

        for (k=0; k<SMF__NQBITS_TCOMP; k++) {
          int fixed = 0;
          const char * qdesc = NULL; /* Description of quality */
          const char * qstr = NULL;  /* Quality string identifier */
          qstr = smf_qual_str( SMF__QFAM_TCOMP, 1, k, &qdesc, status );

          /* Set the quality name */
          irqAddqn( qlocs, qstr, 0, qdesc, status );
          curbit++;
          irqFxbit( qlocs, qstr, curbit, &fixed, status );
        }

        /* compress them */
        for( iw = 0; iw < nw; iw++ ) {
          pdata = job_data + iw;
          pdata->operation = 3;
          pdata->qmap = qmap;
          thrAddJob( wf, 0, pdata, smf1_qual_unmap, 0, NULL, status );
        }
        thrWait( wf, status );

      }
    }

    /* Unmap quality */
    ndfUnmap( indf, "QUALITY", status );

    /* Set the badbits mask to enable all quality by default.
       Do not do this for MAP quality at the moment. */
    if (family != SMF__QFAM_MAP) ndfSbb( 255, indf, status );

    /* release IRQ resources */
    irqRlse( &qlocs, status );
  }

 CLEANUP:
  /* Tidy up */
  qual = astFree( qual );
  job_data = astFree( job_data );
  return NULL;

}





static void smf1_qual_unmap( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_qual_unmap

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_qual_unmap.

*  Invocation:
*     smf1_qual_unmap( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfQualUnmapData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfQualUnmapData *pdata;
   size_t *qcount;
   size_t i1;
   size_t i2;
   size_t i;
   size_t k;
   size_t nqbits;
   smf_qual_t *p1;
   unsigned char *p2;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfQualUnmapData *) job_data_ptr;

   i1 = pdata->i1;
   i2 = pdata->i2;
   nqbits = pdata->nqbits;
   qcount = pdata->qcount;

   if( pdata->operation == 1 ){
      for( k=0; k<nqbits; k++ ) {
        qcount[k] = 0;
      }

      p1 = pdata->qual + i1;
      for( i = i1; i <= i2; i++,p1++) {
        for( k=0; k<nqbits; k++ ) {
          if( *p1 & BIT_TO_VAL(k) ) {
            qcount[k]++;
          }
        }
      }

   } else if( pdata->operation == 2 ){
      size_t curbit;
      size_t lowbit = pdata->lowbit;
      size_t highbit = pdata->highbit;
      p1 = pdata->qual + i1;
      p2 = pdata->qmap + i1;
      for( i = i1; i <= i2; i++,p1++,p2++) {
         curbit = 0;
         *p2 = 0;
         for( k=lowbit; k<=highbit; k++) {

/* was this bit used by this data array? */
            if( qcount[k] ) {

/* was the bit set for this location? */
              if( *p1 & BIT_TO_VAL(k) ) {
                *p2 |= BIT_TO_VAL(curbit);
              }

              curbit++;
            }
          }
        }

   } else if( pdata->operation == 3 ){

      p1 = pdata->qual + i1;
      p2 = pdata->qmap + i1;
      for( i = i1; i <= i2; i++,p1++,p2++) {

        *p2 = 0;
        if( *p1 ) {
          if ( *p1 & (SMF__Q_BADDA|SMF__Q_BADB|SMF__Q_NOISE) ) {
            *p2 |= SMF__TCOMPQ_BAD;
          }
          if ( *p1 & (SMF__Q_APOD|SMF__Q_PAD) ) {
            *p2 |= SMF__TCOMPQ_ENDS;
          }
          if ( *p1 & (SMF__Q_JUMP|SMF__Q_IP|SMF__Q_SPIKE|SMF__Q_FILT|SMF__Q_EXT|SMF__Q_LOWAP|SMF__Q_BADEF) ) {
            *p2 |= SMF__TCOMPQ_BLIP;
          }
          if ( *p1 & (SMF__Q_COM|SMF__Q_SSN|SMF__Q_PCA) ) {
            *p2 |= SMF__TCOMPQ_MATCH;
          }
          if ( *p1 & (SMF__Q_RING) ) {
            *p2 |= SMF__TCOMPQ_RING;
          }
          if ( *p1 & (SMF__Q_STAT) ) {
            *p2 |= SMF__TCOMPQ_TEL;
          }
          if (*p2 == 0 ) {

/* something went wrong. We missed a quality bit somewhere */
            msgOutiff(MSG__QUIET, "", FUNC_NAME ": Untested quality bit found"
                      " in position %zu with value %u", status,
                      i, (unsigned int)*p1);
          }
        }
      }

   } else {
      *status = SAI__ERROR;
      errRepf( "", "smf1_qual_unmap: Invalid operation (%d) supplied.",
               status, pdata->operation );
   }
}




