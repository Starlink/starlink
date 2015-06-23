#include "sae_par.h"
#include "polsub.h"
#include "ast.h"
#include "math.h"
#include "mers.h"
#include "prm_par.h"
#include "star/thr.h"
#include <string.h>

typedef struct pol1RotquJobData {
   AstFrameSet *wcs;
   const double *uin;
   const double *qin;
   double *uout;
   double *qout;
   double angle;
   int ncol;
   int nrow;
   int p1;
   int p2;
   int var;
} pol1RotquJobData;

static void pol1RotquJob( void *job_data_ptr, int *status );

void pol1Rotqu( int nrow, int ncol, AstFrameSet *wcs, float angle, int var,
                const double *qin, const double *uin, double *qout,
                double *uout, int *status ){
/*
*+
*  Name:
*     pol1Rotqu

*  Purpose:
*     Rotate arrays of Q and U to refer to a different reference direction.

*  Language:
*     ANSI C

*  Synopsis:
*     void pol1Rotqu( int nrow, int ncol, AstFrameSet *wcs, float angle,
*                     int var, const double *qin, const double *uin,
*                     double *qout, double *uout, int *status )

*  Description:
*     The routine creates new Q and U values by rotating the reference
*     direction by a given angle.

*  Arguments:
*     nrow
*        The number of rows of pixels in each Q/U map.
*     ncol
*        The number of columns of pixels in each Q/U map.
*     wcs
*        A FrameSet containing a base Frame corresponding to GRID
*        coordinates within the Q (or U) array and a POLANAL Frame in
*        which the first axis defines the reference direction. It is
*        assumed that the Q and U arrays use identical FrameSets.
*     angle
*        The anti-clockwise angle from the GRID X axis to the required
*        reference direction.
*     var
*        If .TRUE., then the supplied arrays hold variance values.
*     qin( el )
*        The supplied array of Q values.
*     uin( el )
*        The supplied array of U values.
*     qout( el )
*        The Returned array of Q values.
*     uout( el )
*        The Returned array of U values.
*     status
*        The global status.

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory.
*     All Rights Reserved.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JUN-2015 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   ThrWorkForce *wf;
   const char *dom;
   int el;
   int icur;
   int ifrm;
   int iworker;
   int nfrm;
   int nw;
   int pstep;
   pol1RotquJobData *pdata;
   pol1RotquJobData *job_data;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Record the index of the current Frame and then find the POLANAL Frame
   and make it current. */
   icur = astGetI( wcs, "Current" );
   nfrm = astGetI( wcs, "NFrame" );
   for( ifrm = 1; ifrm <= nfrm; ifrm++ ) {
      astSetI( wcs, "Current", ifrm );
      dom = astGetC( wcs, "Domain" );
      if( dom && !strcmp( dom, "POLANAL" ) ) break;
   }

   if( ifrm > nfrm && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "", "Cannot find a POLANAL Frame in the input Q or U "
              "WCS FrameSet", status );
   }

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   nw = thrGetNThread( POLPACK__THREADS, status );
   wf = thrGetWorkforce( nw, status );

/* Create structures used to pass information to the worker threads. */
   if( nw < 1 ) nw = 1;
   job_data = astMalloc( nw*sizeof( *job_data ) );
   if( *status == SAI__OK ) {

/* Determine which pixels are to be processed by which threads. */
      el = nrow*ncol;
      pstep = el/nw;
      if( pstep < 1 ) pstep = 1;

      for( iworker = 0; iworker < nw; iworker++ ) {
         pdata = job_data + iworker;
         pdata->p1 = iworker*pstep;

/* Ensure that the last thread picks up any left-over pixels. */
         if( iworker < nw - 1 ) {
            pdata->p2 = pdata->p1 + pstep - 1;
         } else {
            pdata->p2 = el - 1;
         }

/* Each thread needs its own unlocked copy of the FrameSet. */
         pdata->wcs = astCopy( wcs );
         astUnlock( pdata->wcs, 1 );

/* Other stuff. */
         pdata->qin = qin;
         pdata->uin = uin;
         pdata->qout = qout;
         pdata->uout = uout;
         pdata->angle = angle;
         pdata->var = var;
         pdata->nrow = nrow;
         pdata->ncol = ncol;
         pdata->var = var;

/* Pass the job to the workforce for execution. */
         thrAddJob( wf, 0, pdata, pol1RotquJob, 0, NULL, status );
      }

/* Wait for the workforce to complete all jobs. */
      thrWait( wf, status );

/* Lock and annul the AST objects used by each thread. */
      for( iworker = 0; iworker < nw; iworker++ ) {
         pdata = job_data + iworker;
         astLock( pdata->wcs, 0 );
         pdata->wcs = astAnnul( pdata->wcs );
      }
   }

/* Re-instate the original current Frame. */
   astSetI( wcs, "Current", icur );

   icur = astGetI( wcs, "Current" );
/* Free resources. */
   job_data = astFree( job_data );
}


static void pol1RotquJob( void *job_data, int *status ) {
/*
*  Name:
*     pol1RotquJob

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     pol1Rotqu.

*  Invocation:
*     pol1RotquJob( void *job_data, int *status )

*  Arguments:
*     job_data = pol1RotquJobData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   AstFrameSet *wcs;
   const double *qin;
   const double *uin;
   double *beta;
   double *gx0;
   double *gy0;
   double *pbeta;
   double *qout;
   double *uout;
   double angle;
   double cos2a;
   double rot;
   double sin2a;
   int block_size;
   int gx;
   int gy;
   int iblock;
   int ipix;
   int nblock;
   int ncol;
   int npix;
   int p1;
   int p2;
   int pb1;
   int pb2;
   int var;
   pol1RotquJobData *pdata;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer to the job data, and then extract its contents into a
   set of local variables. */
   pdata = (pol1RotquJobData *) job_data;
   wcs = pdata->wcs;
   qin = pdata->qin;
   uin = pdata->uin;
   qout = pdata->qout;
   uout = pdata->uout;
   angle = pdata->angle;
   p1 = pdata->p1;
   p2 = pdata->p2;
   var = pdata->var;
   ncol = pdata->ncol;

/* Pointers to the first input and output Q and U values to be used by
   this thread. */
   qin += p1;
   uin += p1;
   qout += p1;
   uout += p1;

/* Lock the AST FrameSet for use by this thread. */
   astLock( wcs, 0 );

/* Number of pixels to be processed by this thread. */
   npix = p2 - p1 + 1;

/* To avoid overheads associated with making many AST calls for
   individual pixels, we process pixels i nblocks. But we do not do
   them all in a single block to avoid excessive memory requirements.
   Decide on a block size. */
   block_size = 10000;
   if( block_size > npix ) block_size = npix;

/* Allocate work space. */
   gx0 = astMalloc( block_size*sizeof( *gx0 ) );
   gy0 = astMalloc( block_size*sizeof( *gy0 ) );
   beta = astMalloc( block_size*sizeof( *beta ) );

/* Process pixels in blocks. */
   nblock = 1 + ( npix - 1 )/block_size;
   for( iblock = 0; iblock < nblock; iblock++ ) {

/* First and last pixel to be processed in this block. Ensure the last
   block ends at the last pixel to be processed by this thread. */
      pb1 = p1 + iblock*block_size;
      if( iblock == nblock - 1 ) {
         pb2 = p2;
      } else {
         pb2 = pb1 + block_size - 1;
      }

/* How many pixels in this block? */
      npix = pb2 - pb1 + 1;

/* Set up the grid coords of the first pixel to be processed in this
   block. */
      gx = 1 + ( pb1 % ncol );
      gy = 1 + ( pb1 / ncol );

/* Loop round all pixels being processed in this block. */
      for( ipix = 0; ipix < npix; ipix++ ) {

/* Record the GRID coords of this pixel. */
         gx0[ ipix ] = (double) gx;
         gy0[ ipix ] = (double) gy;

/* Update the grid coords of the next pixel to be processed by this
   thread. */
         if( gx == ncol ) {
            gx = 1;
            gy++;
         } else {
            gx++;
         }
      }

/* For each pixel in the block, find the angle from the GRID X axis to
   the old reference direction. Store them in the "beta" array. */
      pol1Pa2gr( wcs, 0, npix, gx0, gy0, beta, status );

/* Loop round all pixels being processed in this block. */
      pbeta = beta;
      for( ipix = 0; ipix < npix; ipix++,qin++,uin++,qout++,uout++,pbeta++ ) {

/* Check both inputs are good. */
         if( *qin != VAL__BADD && *uin != VAL__BADD && *pbeta != AST__BAD ) {

/* Find the total rotation angle. */
            rot = *pbeta - angle;

/*  Calculate the trig terms. */
            cos2a = cos( 2*rot );
            sin2a = sin( 2*rot );
            if( var ) {
               cos2a *= cos2a;
               sin2a *= sin2a;
            }

/* Calculate the rotated Q/U values or variances. */
            if( var ) {
               *qout = (*qin)*cos2a + (*uin)*sin2a;
               *uout = (*uin)*cos2a + (*qin)*sin2a;
            } else {
               *qout = (*qin)*cos2a - (*uin)*sin2a;
               *uout = (*uin)*cos2a + (*qin)*sin2a;
            }

/* Store bad output values for bad input values. */
         } else {
            *qout = VAL__BADD;
            *uout = VAL__BADD;
         }
      }
   }

/* Free resources. */
   gx0 = astFree( gx0 );
   gy0 = astFree( gy0 );
   beta = astFree( beta );

/* Unlock the AST FrameSet so it can be used by the main thread. */
   astUnlock( wcs, 1 );
}


