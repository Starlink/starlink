#include "sae_par.h"
#include "polsub.h"
#include "ast.h"
#include "math.h"
#include "mers.h"
#include "prm_par.h"
#include "star/thr.h"
#include "star/kaplibs.h"
#include <string.h>

typedef struct pol1RotrfJobData {
   AstFrame *pfrm;
   AstFrame *ptfrm;
   AstMapping *gpmap;
   AstMapping *gptmap;
   const double *qin;
   const double *qinv;
   const double *uin;
   const double *uinv;
   double *qout;
   double *qoutv;
   double *uout;
   double *uoutv;
   int iaxis;
   int ncol;
   int nrow;
   int p1;
   int p2;
} pol1RotrfJobData;

static void pol1RotrfJob( void *job_data_ptr, int *status );

void pol1Rotrf( int nrow, int ncol, AstFrameSet *wcs, AstFrameSet *twcs,
                int ifrm, int iaxis, const double *qin,
                const double *uin, double *qout, double *uout,
                const double *qinv, const double *uinv, double *qoutv,
                double *uoutv, int *status ){
/*
*+
*  Name:
*     pol1Rotrf

*  Purpose:
*     Rotate arrays of Q and U to refer to a different reference direction.

*  Language:
*     ANSI C

*  Synopsis:
*     void pol1Rotrf( int nrow, int ncol, AstFrameSet *wcs, AstFrameSet *twcs,
*                     int ifrm, int iaxis, const double *qin,
*                     const double *uin, double *qout, double *uout,
*                     const double *qinv, const double *uinv, double *qoutv,
*                     double *uoutv, int *status )

*  Description:
*     The routine creates new Q and U values by rotating the reference
*     direction either to a specified axis within some specified Frame, or
*     so that each pixel uses the same reference direction as the
*     corresponding pixel in a supplied template NDF.

*  Arguments:
*     nrow
*        The number of rows of pixels in each Q/U map.
*     ncol
*        The number of columns of pixels in each Q/U map.
*     wcs
*        A FrameSet containing a base Frame corresponding to GRID
*        coordinates within the Q (or U) array and a POLANAL Frame in
*        which the first axis defines the reference direction used by the
*        input Q and U values. It is assumed that the Q and U arrays use
*        identical FrameSets. Modified on exit to include a new POLANAL
*        Frame.
*     twcs
*        A FrameSet containing a base Frame corresponding to GRID
*        coordinates within the tempate NDF and a POLANAL Frame in
*        which the first axis defines the reference direction used by the
*        template Q and U values. Modified on exit to include all the
*        Frames from "wcs". If "twcs" is NULL, then the position angle
*        specified by arguments "ifrm" and "iaxis" is used.
*     ifrm
*        Only used if "twcs" is NULL. It is the one-based index of the
*        Frame within "wcs" that is used to define the required reference
*        direction.
*     iaxis
*        Only used if "twcs" is NULL. It is the zero-based index of the
*        axis within the Frame specified by "ifrm" that is used as the
*        new reference direction.
*     qin( el )
*        The supplied array of Q data values.
*     uin( el )
*        The supplied array of U data values.
*     qout( el )
*        The Returned array of Q data values.
*     uout( el )
*        The Returned array of U data values.
*     qinv( el )
*        The supplied array of Q variance values (may be NULL).
*     uinv( el )
*        The supplied array of U variance values (may be NULL).
*     qoutv( el )
*        The Returned array of Q variance values (may be NULL).
*     uoutv( el )
*        The Returned array of U variance values (may be NULL).
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
   AstFrame *pfrm;
   AstFrame *ptfrm;
   AstFrameSet *fs;
   AstMapping *gpmap;
   AstMapping *gpmapt;
   AstMapping *gptmap;
   AstMapping *map;
   ThrWorkForce *wf;
   int ipfrm;
   int perm[2];
   AstPermMap *pm;
   int el;
   int ibase;
   int icur;
   int iworker;
   int ndim;
   int nf;
   int nw;
   int pstep;
   pol1RotrfJobData *job_data;
   pol1RotrfJobData *pdata;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Start an AST context. */
   astBegin;

/* Get the number of axes in the wcs Base Frame. */
   ndim = astGetI( wcs, "nin" );

/* Record the index of the current Frame in wcs and then find the
   POLANAL Frame (it becomes the current Frame in "fs" - the base
   Frame in "fs" is the base frame from "wcs"). */
   icur = astGetI( wcs, "Current" );
   fs = astFindFrame( wcs, astFrame( ndim, "minaxes=1,maxaxes=20" ),
                      "POLANAL" );

/* Record the index of the original POLANAL Frame within "wcs", and then
   re-instate the original current Frame. */
   ipfrm = astGetI( wcs, "Current" );
   astSetI( wcs, "Current", icur );

/* Report an error if no POLANAL Frame was found. */
   if( !fs && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "", "Cannot find a POLANAL Frame in the input Q or U "
              "WCS FrameSet", status );
      ipfrm = AST__NOFRAME;

/* Get the Mapping from the GRID coords (the base Frame) in wcs to the
   POLANAL Frame, and the POLANAL Frame. */
   } else {
      gpmap = astGetMapping( fs, AST__BASE, AST__CURRENT );
      pfrm = astGetFrame( fs, AST__CURRENT );
      fs = astAnnul( fs );
   }

/* If "twcs" was supplied, we use the first axis within the POLANAL Frame
   as the new reference direction. */
   if( twcs ) {
      iaxis = 0;

      ndim = astGetI( twcs, "nin" );
      icur = astGetI( twcs, "Current" );
      fs = astFindFrame( twcs, astFrame( ndim, "minaxes=1,maxaxes=20" ),
                         "POLANAL" );
      astSetI( twcs, "Current", icur );
      if( !fs && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( "", "Cannot find a POLANAL Frame in the WCS FrameSet "
                 "of the template NDF.", status );
      } else {
         gpmapt = astGetMapping( fs, AST__BASE, AST__CURRENT );
         ptfrm = astGetFrame( fs, AST__CURRENT );
         fs = astAnnul( fs );
      }

/* Note the original number of Frames in the template FrameSet, and the
   index of the base Frame in the wcs FrameSet. */
      nf = astGetI( twcs, "NFrame" );
      ibase = astGetI( wcs, "Base" );

/* Merge the FrameSets, aligning them in an appropriate Frame (e.g. SKY).
   All the Frames from "wcs" get appended to the end of the Frame list
   in "twcs". */
      kpg1Asmrg( twcs, wcs, " ", 0, 3, status );

/* Get the Mapping from GRID coords in wcs to GRID coords in twcs.
   The GRID frame had index "ibase" within "wcs" so now has index
   "ibase+nf" within "twcs". */
      map = astGetMapping( twcs, ibase + nf, AST__BASE );

/* Combine this Mapping with the Mapping from twcs GRID coords to twcs
   POLANAL coords, to get the Mapping from wcs GRID coords to twcs
   POLANAL coords. */
      gptmap = (AstMapping *) astCmpMap( map, gpmapt, 1, " " );

/* If "twcs" was not supplied, we use suitable Mappings and Frames
   derived from "wcs" that result in the "ifrm" Frame being used as the
   "POLANAL" Frame. */
   } else {
      ptfrm = astGetFrame( wcs, ifrm );
      gptmap = astGetMapping( wcs, AST__BASE, ifrm );
      if( astGetI( ptfrm, "Naxes" ) != 2 && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( "", "The coordinate frame specified by parameter FRAME "
                 "is not -2-dimensional.", status );
      }
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

/* Each thread needs its own unlocked copy of the Mappings and Frames. */
         pdata->gpmap = astCopy( gpmap );
         astUnlock( pdata->gpmap, 1 );

         pdata->pfrm = astCopy( pfrm );
         astUnlock( pdata->pfrm, 1 );

         pdata->gptmap = astCopy( gptmap );
         astUnlock( pdata->gptmap, 1 );

         pdata->ptfrm = astCopy( ptfrm );
         astUnlock( pdata->ptfrm, 1 );

/* Other stuff. */
         pdata->iaxis = iaxis;
         pdata->ncol = ncol;
         pdata->nrow = nrow;
         pdata->qin = qin;
         pdata->qinv = qinv;
         pdata->qout = qout;
         pdata->qoutv = qoutv;
         pdata->uin = uin;
         pdata->uinv = uinv;
         pdata->uout = uout;
         pdata->uoutv = uoutv;

/* Pass the job to the workforce for execution. */
         thrAddJob( wf, 0, pdata, pol1RotrfJob, 0, NULL, status );
      }

/* Wait for the workforce to complete all jobs. */
      thrWait( wf, status );

/* Lock and annul the AST objects used by each thread. */
      for( iworker = 0; iworker < nw; iworker++ ) {
         pdata = job_data + iworker;

         astLock( pdata->gpmap, 0 );
         pdata->gpmap = astAnnul( pdata->gpmap );

         astLock( pdata->pfrm, 0 );
         pdata->pfrm = astAnnul( pdata->pfrm );

         astLock( pdata->gptmap, 0 );
         pdata->gptmap = astAnnul( pdata->gptmap );

         astLock( pdata->ptfrm, 0 );
         pdata->ptfrm = astAnnul( pdata->ptfrm );
      }
   }


/* Now modify the "wcs" FrameSet to describe the returned arrays. First,
   remove the old POLANAL Frame. */
   astRemoveFrame( wcs, ipfrm );

/* If the new POLANAL Frame is parallel to the second axis of the
   specified Frame, we need a PermMap to make it parallel to the first axis. */
   if( iaxis == 1 ) {
      perm[ 0 ] = 2;
      perm[ 1 ] = 1;
      pm = astPermMap( 2, perm, 2, perm, NULL, " " );
      gptmap = (AstMapping *) astCmpMap( gptmap, pm, 1, " " );
   }

/* Add a new POLANAL Frame into the wcs FrameSet. */
   astSetC( ptfrm, "Label(1)", "Polarimetric reference direction" );
   astSetC( ptfrm, "Label(2)", " " );
   astSetC( ptfrm, "Title", "Polarimetric reference frame" );
   icur = astGetI( wcs, "Current" );
   astAddFrame( wcs, AST__BASE, gptmap, ptfrm );
   astSetI( wcs, "Current", icur );

/* Free resources. */
   job_data = astFree( job_data );

/* End the AST context. */
   astEnd;
}


static void pol1RotrfJob( void *job_data, int *status ) {
/*
*  Name:
*     pol1RotrfJob

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     pol1Rotrf.

*  Invocation:
*     pol1RotrfJob( void *job_data, int *status )

*  Arguments:
*     job_data = pol1RotrfJobData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   AstMapping *gpmap;
   AstMapping *gptmap;
   AstFrame *pfrm;
   AstFrame *ptfrm;
   const double *qin;
   const double *uin;
   const double *qinv;
   const double *uinv;
   double *beta1;
   double *beta2;
   double *gx0;
   double *gy0;
   double *pbeta1;
   double *pbeta2;
   double *qout;
   double *uout;
   double *qoutv;
   double *uoutv;
   double cos2a;
   double rot;
   double sin2a;
   int block_size;
   int gx;
   int gy;
   int iaxis;
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
   pol1RotrfJobData *pdata;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer to the job data, and then extract its contents into a
   set of local variables. */
   pdata = (pol1RotrfJobData *) job_data;
   iaxis = pdata->iaxis;
   gpmap = pdata->gpmap;
   gptmap = pdata->gptmap;
   ncol = pdata->ncol;
   p1 = pdata->p1;
   p2 = pdata->p2;
   pfrm = pdata->pfrm;
   ptfrm = pdata->ptfrm;
   qin = pdata->qin;
   qinv = pdata->qinv;
   qout = pdata->qout;
   qoutv = pdata->qoutv;
   uin = pdata->uin;
   uinv = pdata->uinv;
   uout = pdata->uout;
   uoutv = pdata->uoutv;

/* Set a flag indicating if variances are being produced. */
   var = qoutv && uoutv && qinv && uinv;

/* Pointers to the first input and output Q and U values to be used by
   this thread. */
   qin += p1;
   uin += p1;
   qout += p1;
   uout += p1;

   if( var ) {
      qinv += p1;
      uinv += p1;
      qoutv += p1;
      uoutv += p1;
   }

/* Lock the AST objects for use by this thread. */
   astLock( gpmap, 0 );
   astLock( pfrm, 0 );
   astLock( gptmap, 0 );
   astLock( ptfrm, 0 );

/* Number of pixels to be processed by this thread. */
   npix = p2 - p1 + 1;

/* To avoid overheads associated with making many AST calls for
   individual pixels, we process pixels in blocks. But we do not do
   them all in a single block to avoid excessive memory requirements.
   Decide on a block size. */
   block_size = 10000;
   if( block_size > npix ) block_size = npix;

/* Allocate work space. */
   gx0 = astMalloc( block_size*sizeof( *gx0 ) );
   gy0 = astMalloc( block_size*sizeof( *gy0 ) );
   beta1 = astMalloc( block_size*sizeof( *beta1 ) );
   beta2 = astMalloc( block_size*sizeof( *beta2 ) );

/* Process each block of pixels. */
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

/* Loop round all pixels within the Q and U arrays being processed in this
   block. */
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

/* For each pixel in the block, find the angle from the GRID X axis in
   the Q/U arrays to the original reference direction. Store them in
   the "beta1" array. */
      pol1Pa2gr( gpmap, pfrm, 0, npix, gx0, gy0, beta1, status );

/* For each pixel in the block, find the angle from the GRID X axis in
   the Q/U arrays to the new reference direction. Store them in
   the "beta2" array. */
      pol1Pa2gr( gptmap, ptfrm, iaxis, npix, gx0, gy0, beta2, status );

/* Loop round all pixels being processed in this block. */
      pbeta1 = beta1;
      pbeta2 = beta2;
      for( ipix = 0; ipix < npix; ipix++,qin++,uin++,qout++,uout++,pbeta1++,pbeta2++) {

/* Check both inputs and angles are good. */
         if( *qin != VAL__BADD && *uin != VAL__BADD &&
             *pbeta1 != AST__BAD && *pbeta2 != AST__BAD ) {

/* Find the anti-clockwise rotation to apply to the Q/U values. */
            rot = *pbeta1 - *pbeta2;

/*  Calculate the trig terms. */
            cos2a = cos( 2*rot );
            sin2a = sin( 2*rot );

/* Calculate the rotated Q/U values or variances. */
            *qout = (*qin)*cos2a - (*uin)*sin2a;
            *uout = (*uin)*cos2a + (*qin)*sin2a;

/* Calculate variances if required. */
            if( var ) {
               if( *qinv != VAL__BADD && *uinv != VAL__BADD ) {
                  cos2a *= cos2a;
                  sin2a *= sin2a;
                  *qoutv = (*qinv)*cos2a + (*uinv)*sin2a;
                  *uoutv = (*uinv)*cos2a + (*qinv)*sin2a;
               } else {
                  *qoutv = VAL__BADD;
                  *uoutv = VAL__BADD;
               }
            }

/* Store bad output values for bad input values. */
         } else {
            *qout = VAL__BADD;
            *uout = VAL__BADD;
            if( var ) {
               *qoutv = VAL__BADD;
               *uoutv = VAL__BADD;
            }
         }

/* If required, increment the variance pointers to the next pixel. */
         if( var ) {
            qoutv++;
            uoutv++;
            qinv++;
            uinv++;
         }
      }
   }

/* Free resources. */
   gx0 = astFree( gx0 );
   gy0 = astFree( gy0 );
   beta1 = astFree( beta1 );
   beta2 = astFree( beta2 );

/* Unlock the AST objects so they can be locked for use by the main
   thread. */
   astUnlock( gpmap, 1 );
   astUnlock( pfrm, 1 );
   astUnlock( gptmap, 1 );
   astUnlock( ptfrm, 1 );
}


