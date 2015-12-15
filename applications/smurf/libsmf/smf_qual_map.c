/*
*+
*  Name:
*     smf_qual_map

*  Purpose:
*     Map a quality array and maybe copy to malloced buffer

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_qual_t *smf_qual_map( ThrWorkForce *wf, int indf, const char mode[],
*                               smf_qfam_t *family, size_t *nmap, int * status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     indf = int (Given)
*        NDF identifier
*     mode = const char [] (Given)
*        Access mode to use for mapping
*     family = smf_qfam_t * (Returned)
*        Indicate which family of quality bits is represented in "qual". Can be NULL.
*        Will only have a meaningful value if the file has been opened in READ or
*        UPDATE mode (otherwise the values in the array are fresh).
*     nmap = size_t * (Returned)
*        Number of elements mapped or malloced
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     smf_qual_t *
*        Pointer to quality values. Free using smf_qual_unmap.

*  Description:
*     Map the QUALITY component of an NDF using the supplied access mode.
*     If the internal SMURF quality is not _UBYTE an array will be
*     malloced and the mapped data will be copied over. Additionally
*     the QUALITY itself will then be unmapped. If the internal SMURF
*     quality is unsigned char the pointer to the mapped array will
*     be returned directly.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB:  David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     Unmap the array using smf_qual_unmap to make sure that malloced
*     memory is freed properly.

*  History:
*     2010-06-16 (TIMJ):
*        Initial version
*     2011-04-11 (TIMJ):
*        Simplify initialisation logic. Always initialise.
*     2011-04-26 (DSB):
*        Added SMF__Q_LOWAP.
*     2011-09-19 (DSB):
*        Added SMF__Q_BADEF.
*     2012-11-27 (DSB):
*        Change error handling to avoid segfault if an error is reported
*        in irqNxtqn, causing "bit" to be zero, which then gets
*        decremented to -1, and used as an array index...
*     2012-12-5 (DSB):
*        If the NDF has not Quality component, return an array full of zeros,
*        with "*family" set to SMF__QFAM_NULL.
*     2014-1-10 (DSB):
*        Multi-threaded.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010-2011 Science and Technology Facilities Council.
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
#include "smf_err.h"

#include "star/thr.h"
#include "star/irq.h"
#include "ndf.h"
#include "sae_par.h"
#include "ast.h"
#include "irq_err.h"
#include "mers.h"

/* number of NDF quality bits */
#define NDFBITS 8

/* Prototypes for local static functions. */
static void smf1_qual_map( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfQualMapData {
   int *ndfqtoval;
   int *ndfqval;
   int operation;
   size_t i1;
   size_t i2;
   smf_qfam_t lfamily;
   smf_qual_t *retval;
   unsigned char *qmap;
} SmfQualMapData;

smf_qual_t * smf_qual_map( ThrWorkForce *wf, int indf, const char mode[],
                           smf_qfam_t *family, size_t *nmap, int * status ) {

  size_t i;             /* Loop counter */
  int itemp = 0;        /* temporary int */
  smf_qfam_t lfamily = SMF__QFAM_NULL; /* Local quality family */
  size_t nout;          /* Number of elements mapped */
  size_t numqn = 0;     /* number of quality names */
  IRQLocs *qlocs = NULL;/* IRQ Quality */
  unsigned char *qmap;  /* pointer to mapped unsigned bytes */
  void *qpntr[1];       /* Somewhere to put the mapped pointer */
  smf_qual_t *retval = NULL; /* Returned pointer */
  int there;            /* Does the NDF Have a Quality component? */
  char xname[DAT__SZNAM+1];  /* Name of extension holding quality names */
  SmfQualMapData *job_data = NULL;
  SmfQualMapData *pdata;
  int nw;
  size_t step;
  int iw;


  if (*status != SAI__OK) return retval;

  /* Ensure jobs submitted to the workforce within this function are
     handled separately to any jobs submitted earlier (or later) by any
     other function. */
  thrBeginJobContext( wf, status );

  /* how many elements do we need */
  ndfSize( indf, &itemp, status );
  nout = itemp;
  if (nmap) *nmap = nout;

  /* malloc the QUALITY buffer. Initialise to zero to simplify logic
     below. It is difficult to determine in advance which case can use
     initialisation. */
  retval = astCalloc( nout, sizeof(*retval) );

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
      pdata->retval = retval;

    }
  }

  /* If the NDF has no QUality component, return the buffer filled with
     zeros. */
  ndfState( indf, "QUALITY", &there, status );
  if( there ) {

    /* READ and UPDATE mode require that the QUALITY is processed
       and copied before being returned. WRITE mode means that the
       buffer contains no information to copy yet. WRITE/ZERO
       and WRITE/BAD also require that we do not do any quality
       handling */
    if ( strncmp(mode, "WRITE",5) == 0 ) {
      /* WRITE and WRITE/ZERO are actually treated the same way
         because we always initialise */
      if ( strcmp( mode, "WRITE/BAD") == 0 ) {
        for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->operation = 1;
            thrAddJob( wf, 0, pdata, smf1_qual_map, 0, NULL, status );
        }
        thrWait( wf, status );
      }

      /* unmap the NDF buffer and return the pointer */
      if (family) *family = lfamily;
      return retval;
    }

    /* Map the quality component (we always need to do this) */
    ndfMap( indf, "QUALITY", "_UBYTE", mode, &qpntr[0], &itemp, status );
    qmap = qpntr[0];

    /* Need to find out what quality names are in play so we
       can work out which family to translate them to */
    irqFind( indf, &qlocs, xname, status );
    numqn = irqNumqn( qlocs, status );

    if ( *status == IRQ__NOQNI || numqn == 0) {
      /* do not have any names defined so we have no choice
         in copying the values directly out the file */
      if (*status != SAI__OK) errAnnul( status );

      /* simple copy with type conversion */

      for( iw = 0; iw < nw; iw++ ) {
        pdata = job_data + iw;
        pdata->qmap = qmap;
        pdata->operation = 2;
        thrAddJob( wf, 0, pdata, smf1_qual_map, 0, NULL, status );
      }
      thrWait( wf, status );

    } else {
      IRQcntxt contxt = 0;
      int ndfqtosmf[NDFBITS];        /* NDF bit (arr index) and SMURF alternative */
      int ndfqtoval[NDFBITS];        /* NDF bit (arr index) and corresponding Qual value */
      int ndfqval[NDFBITS];          /* Bit values for NDF quality */
      int identity = 1;        /* Is this a simple identity map? */

      /* prefill the mapping with bit to bit mapping */
      for (i=0; i<NDFBITS; i++) {
        ndfqtosmf[i] = i;
        ndfqtoval[i] = BIT_TO_VAL(i);
        ndfqval[i] = ndfqtoval[i];
      }

      /* Now translate each name to a bit */
      for (i = 0; i < numqn && *status == SAI__OK; i++) {
        char qname[IRQ__SZQNM+1];
        char commnt[IRQ__SZCOM+1];
        int fixed;
        int value;
        int bit;
        int done;
        smf_qual_t qval;
        smf_qfam_t tmpfam = 0;

        irqNxtqn( qlocs, &contxt, qname, &fixed, &value, &bit,
                  commnt,sizeof(commnt), &done, status );
        bit--;    /* IRQ starts at 1 */

        /* Now convert the quality name to a quality value
           and convert that to a bit. These should all be
           less than 9 bits because they are in the NDF file. */
        qval = smf_qual_str_to_val( qname, &tmpfam, status );

        if (*status == SMF__BADQNM ) {
          /* annul status and just copy this bit from the file
             to SMURF without change. This might result in a clash
             of bits but we either do that or drop out the loop
             and assume everything is broken */
          if (*status != SAI__OK) errAnnul(status);
          ndfqtosmf[bit] = bit;
          ndfqtoval[bit] = BIT_TO_VAL(bit);

        } else if( *status == SAI__OK ){
          if (lfamily == SMF__QFAM_NULL) {
            lfamily = tmpfam;
          } else if (lfamily != tmpfam) {
            msgOutif(MSG__QUIET, "",
                     "WARNING: Quality names in file come from different families",
                     status );
          }
          ndfqtosmf[bit] = smf_qual_to_bit( qval, status );
          ndfqtoval[bit] = qval;

          /* not a 1 to 1 bit translation */
          if (bit != ndfqtosmf[bit]) identity = 0;
        }
      }

      /* Now copy from the file and translate the bits. If this is an
         identity mapping or we do not know the family then we go quick. */
      if (*status == SAI__OK) {
        if ( (identity && lfamily != SMF__QFAM_TCOMP) || lfamily == SMF__QFAM_NULL) {
          for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->qmap = qmap;
            pdata->ndfqval = ndfqval;
            pdata->lfamily = lfamily;
            pdata->ndfqtoval = ndfqtoval;
            pdata->ndfqval = ndfqval;
            pdata->operation = 2;
            thrAddJob( wf, 0, pdata, smf1_qual_map, 0, NULL, status );
          }
          thrWait( wf, status );

        } else {

          for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->qmap = qmap;
            pdata->ndfqval = ndfqval;
            pdata->lfamily = lfamily;
            pdata->ndfqtoval = ndfqtoval;
            pdata->ndfqval = ndfqval;
            pdata->operation = 3;
            thrAddJob( wf, 0, pdata, smf1_qual_map, 0, NULL, status );
          }
          thrWait( wf, status );

          /* we have uncompressed */
          if (lfamily == SMF__QFAM_TCOMP) lfamily = SMF__QFAM_TSERIES;
        }
      }
    }

    /* Free quality */
    irqRlse( &qlocs, status );

    /* no longer need the mapped data */
    ndfUnmap( indf, "QUALITY", status );
  }

  /* End the Thr job context */
  thrEndJobContext( wf, status );

  /* Free other resources. */
  job_data = astFree( job_data );

  if (family) *family = lfamily;
  return retval;
}



static void smf1_qual_map( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_qual_map

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_qual_map.

*  Invocation:
*     smf1_qual_map( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfQualMapData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfQualMapData *pdata;
   size_t i1;
   size_t i2;
   size_t i;
   smf_qual_t *p1;
   unsigned char *p2;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfQualMapData *) job_data_ptr;

   i1 = pdata->i1;
   i2 = pdata->i2;

   if( pdata->operation == 1 ){
      p1 = pdata->retval + i1;
      for( i = i1; i <= i2; i++) {
         *(p1++) = VAL__BADQ;
      }

   } else if( pdata->operation == 2 ){
      p1 = pdata->retval + i1;
      p2 = pdata->qmap + i1;
      for( i = i1; i <= i2; i++) {
         *(p1++) = *(p2++);
      }

   } else if( pdata->operation == 3 ){
      p1 = pdata->retval + i1;
      p2 = pdata->qmap + i1;
      for( i = i1; i <= i2; i++,p1++,p2++) {

/* Output buffer would be set to zero but it already has that value so do
   nothing. */
         if( *p2 == 0 ) {

/* This becomes a very laborious bitwise copy. */
         } else {

/* One to one mapping for TSERIES and MAP families. */
            if( pdata->lfamily == SMF__QFAM_TSERIES ||
                pdata->lfamily == SMF__QFAM_MAP ) {
               size_t k;
               for( k = 0; k < NDFBITS; k++ ) {
                  if( *p2 & pdata->ndfqval[k] ) *p1 |=  pdata->ndfqtoval[ k ];
               }

            } else if( pdata->lfamily == SMF__QFAM_TCOMP ) {
               size_t k;

/* This requires some guess work */
               for( k = 0; k < NDFBITS; k++ ) {
                  if( *p2 &  pdata->ndfqval[k] ) {
                     if(  pdata->ndfqtoval[k] & SMF__TCOMPQ_BAD ) {
                        *p1 |= (SMF__Q_BADB|SMF__Q_BADDA);
                     } else if( pdata->ndfqtoval[k] & SMF__TCOMPQ_ENDS ) {
                        *p1 |= (SMF__Q_PAD|SMF__Q_APOD);
                     } else if( pdata->ndfqtoval[k] & SMF__TCOMPQ_BLIP ) {
                        *p1 |= (SMF__Q_SPIKE | SMF__Q_JUMP | SMF__Q_LOWAP | SMF__Q_GENERIC);
                     } else if( pdata->ndfqtoval[k] & SMF__TCOMPQ_MATCH ) {
                        *p1 |= SMF__Q_COM;
                     } else if( pdata->ndfqtoval[k] & SMF__TCOMPQ_TEL ) {
                        *p1 |= SMF__Q_STAT;
                     } else if( pdata->ndfqtoval[k] & SMF__TCOMPQ_RING ) {
                        *p1 |= SMF__Q_RING;
                     } else {

/* just do the normal mapping */
                        *p1 |= pdata->ndfqtoval[k];
                     }
                  }
               }

            } else {
               *status = SAI__ERROR;
               errRep("", "Did not recognize quality family. "
                      "(Possible programming error)", status );
               break;
            }
         }
      }

   } else {
      *status = SAI__ERROR;
      errRepf( "", "smf1_qual_map: Invalid operation (%d) supplied.",
               status, pdata->operation );
   }
}



