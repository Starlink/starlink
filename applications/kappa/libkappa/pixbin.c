/* Indicate that we want to use the 8-byte NDF interface */
#define NDF_I8 1

#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/ndg.h"
#include "star/kaplibs.h"
#include "star/grp.h"
#include "par.h"
#include "prm_par.h"
#include "star/thr.h"

/* Macros to test of a generic data value is good. The "val" argument is
   a pointer to the start of the value. */
#define ISGOOD(val) ( !bad || memcmp( val, badbuf, bpp ) )

/* Local data types */
typedef struct  PixbinData {
   hdsdim *lbnd;
   hdsdim *olbnds;
   hdsdim *ubnd;
   hdsdim olbnd;
   hdsdim oubnd;
   int **pindices;
   int *pindex;
   int m;
   int n;
   int operation;
   int sndf;
   int tndf;
   size_t p1;
   size_t p2;
   size_t *strides;
   size_t *vindex;
} PixbinData;

/* Prototypes for local static functions. */
static void pixbin_work( void *job_data_ptr, int *status );



F77_SUBROUTINE(pixbin)( INTEGER(status) ){
/*
*+
*  Name:
*     PIXBIN

*  Purpose:
*     Places each pixel value in an input NDF into an output bin

*  Language:
*     C (designed to be called from Fortran)

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PIXBIN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application collects selected pixel values together from an
*     input NDF and places them into a single column of an output NDF.
*     Each such output column represents a "bin" into which a subset
*     of the input pixels is placed.
*
*     If the input NDF has N pixel axes, the user provides a set of M
*     N-dimensional "index" NDFs (where M is between 1 and 6). For each
*     pixel in the input NDF, the corresponding value in each of the
*     M index NDFs is found. This vector of M values is used (after
*     rounding them to the nearest integer) to determine the pixel
*     indices within the output (M-dimensional) NDF at which to store
*     the input pixel value. Thus each output pixel corresponds to a bin
*     into which one or more input pixels can be placed, as selected by
*     the index NDFs.
*
*     There are many possible ways in which the input pixels values
*     that fall in a single bin could be combined to create a single
*     representative output value for each bin. For instance, the output
*     NDF could contain the mean of the input values that fall in each
*     bin, or the maximum, or the standard deviation, etc. However, this
*     application does not store a single representative value for each bin.
*     Instead it stores all the separate input pixel values that fall in
*     each bin. This requires an extra trailing pixel axis in the output
*     NDF, with a lower pixel bounds of 1 and and upper pixel bound equal to
*     the maximum population of any bin. Each "column" of values parallel
*     to this final output pixel axis represents one bin and contains the
*     corresponding input pixel values at its lower end, with bad values
*     filling any unused higher pixels. The COLLAPSE application could
*     then be used to get a representative value for each bin by
*     collapsing this final pixel axis using any of the many estimators
*     provided by COLLAPSE.

*  Usage:
*     pixbin in out index

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input N-dimensional NDF.
*     INDEX = NDF (Read)
*        A group of index NDFs (all with N dimensions). The number of
*        index NDFs supplied should be in the range 1--6 and determines
*        the dimensionality of the output NDF. A section is taken from
*        each one so that it matches the input NDF supplied by Parameter
*        IN. The data values in the J'th index NDF are converted to
*        _INTEGER (by finding the nearest integer) and then used as the
*        pixel indices on the J'th output pixel axis.
*     OUT = NDF (Write)
*        The output NDF containing all the values from the input NDF
*        collected into a set of bins. This NDF will have M+1 pixel axes,
*        where M is the number of index NDF supplied using Parameter INDEX.
*        The final pixel axis enumerates the individual input pixels that
*        fall within each bin.

*  Examples:
*     pixbin m31 binned radius
*        Here the pixel values in a two-dimensional NDF called "m31" are
*        placed into bins as defined by the contents of a single two-dimensional
*        NDF called "radius", to create a two-dimensional output NDF called
*        "binned". (The number of pixel axes in the output is always one
*        more than the number of index NDFs.) The data values in "radius"
*        are used as the pixel indices along the first axis of the output
*        NDF, at which to store each input pixel value. Each column in the
*        output NDF contains the individual input pixel values assigned
*        to that bin, padded with bad values if necessary to fill the
*        column.

*  Related Applications:
*     KAPPA: COLLAPSE

*  Implementation Status:
*     -  This routine correctly processes the DATA, QUALITY, VARIANCE, LABEL,
*     TITLE, UNITS and HISTORY components of the input NDF and propagates all
*     extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     21-APR-2020 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-
*/

/* Local Variables: */
   Grp *grp;                 /* GRP identifier for configuration settings */
   PixbinData *job_data;     /* Pointer to all job data structures */
   PixbinData *pdata;        /* Pointer to single job data structure */
   ThrWorkForce *wf;         /* Pointer to workforce (a pool of threads) */
   char *comps[ 3 ] = {"Data", "Variance", "Quality"}; /* Array names */
   char *ptrin;              /* Pointer to input array */
   char *ptrout;             /* Pointer to output array */
   char badbuf[ 8 ];         /* Buffer for bad value bit pattern */
   char type[ DAT__SZTYP + 1 ];/* NDF data type */
   hdsdim lbnd[ NDF__MXDIM ];/* Lower pixel bounds of input NDF */
   hdsdim olbnd[ NDF__MXDIM ];/* Lower pixel bounds of output NDF */
   hdsdim oubnd[ NDF__MXDIM ];/* Upper pixel bounds of output NDF */
   hdsdim ubnd[ NDF__MXDIM ];/* Upper pixel bounds of input NDF */
   int *binpop;              /* Array holding population of each bin */
   int *pi;                  /* Pointer to next index value */
   int *pindex[ NDF__MXDIM ];/* Pointers to mapped index arrays */
   int bad;                  /* Check for bad values? */
   int icomp;                /* Index of next array component */
   int indf2;                /* Identifier for output NDF */
   int indf;                 /* Identifier for input NDF */
   int jlast;                /* Zero-based index on last output axis */
   int maxpop;               /* Largest bin population */
   int n;                    /* Number of pixel axes in input NDF */
   int nw;                   /* Number of threads to use */
   int round;                /* NDF library rounding flag */
   int there;                /* Does the object exist? */
   int tndf;                 /* Identifier for an index NDF */
   int64_t i;                /* Loop count */
   int64_t j;                /* Loop count */
   size_t *pvindex;          /* Pointer to output index of next input pixel */
   size_t *vindex;           /* O/p 0-based vector index of each i/p pixel */
   size_t bpp;               /* Bytes per input array element */
   size_t el;                /* Number of mapped elements in input */
   size_t elout;             /* Number of mapped elements in output */
   size_t iv;                /* Zero-based index of output pixel */
   size_t m;                 /* Number of index NDFs */
   size_t nbin;              /* Number of output bins */
   size_t size;              /* Size of GRP group */
   size_t step;              /* No. of pixels for each worker thread */
   size_t stride[ NDF__MXDIM ];/* Stride between cells on each axis */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Start an NDF context */
   ndfBegin();

/* Get an identifier for the input NDF. We use NDG (via kpg1_Rgndf)
   instead of calling ndfAssoc directly since NDF/HDS has problems with
   file names containing spaces, which NDG does not have. */
   kpg1Rgndf( "IN", 1, 1, "", &grp, &size, status );
   ndgNdfas( grp, 1, "READ", &indf, status );
   grpDelet( &grp, status );

/* Get the pixel bounds of the input NDF, and the number of pixels in it. */
   ndfBound( indf, NDF__MXDIM, lbnd, ubnd, &n, status );
   ndfSize( indf, &el, status );

/* Get a group of index NDFs. */
   kpg1Rgndf( "INDEX", NDF__MXDIM - 1, 1, "", &grp, &m, status );

/* Ensure the NDF library converts floating-point values to integer by
   rounding to the nearest integer value. Record the original value of
   tuning parameter so it can be re-instated later. */
   ndfGtune( "ROUND", &round, status );
   ndfTune( 1, "ROUND", status );

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   wf = thrGetWorkforce( thrGetNThread( "KAPPA_THREADS", status ), status );

/* How many worker threads in the workforce? May not be the same as
   requested if the workforce already existed. */
   nw = wf ? wf->nworker : 1;

/* Allocate job data for threads. Each structure describes a single job
   to be allocated to a single worker thread. */
   if( m > nw ) {
      job_data = astCalloc( m, sizeof(*job_data) );
   } else {
      job_data = astCalloc( nw, sizeof(*job_data) );
   }

/* For each one, submit a job to the workforce to get a section that
   matches the input NDF, map its Data array as _INTEGER, and get the
   maximum and minimum data value in the NDF. */
   if( *status == SAI__OK ) {
      for( i = 0; i < m; i++ ) {

/* Get an identifier for the i'th index NDF (note, NDG uses one-based
   indices). Unlock it so that the worker thread can lock it for its own
   use. */
         ndgNdfas( grp, i + 1, "READ", &tndf, status );
         ndfUnlock( tndf, status );

/* Store information to be passed to the worker thread. */
         pdata = job_data + i;
         pdata->operation = 1;
         pdata->tndf = tndf;
         pdata->n = n;
         pdata->lbnd = lbnd;
         pdata->ubnd = ubnd;

/* Submit the job to the worker thread. */
         thrAddJob( wf, 0, pdata, pixbin_work, 0, NULL, status );
      }

/* Put the current thread to sleep until all the above jobs have
   completed. */
      thrWait( wf, status );

/* Gather results from all worker threads. */
      nbin = 1;
      for( i = 0; i < m; i++ ) {
         pdata = job_data + i;

/* Lock the NDF identifiers for use by this thread. */
         ndfLock( pdata->sndf, status );

/* Store the bounds of the output NDF on the i'th pixel axis. Report an
   error if the axis has zero length. */
         olbnd[ i ] = pdata->olbnd;
         oubnd[ i ] = pdata->oubnd;
         if( olbnd[ i ] > oubnd[ i ] && *status == SAI__OK ) {
            *status = SAI__ERROR;
            ndfMsg( "N", pdata->sndf );
            errRep( " ", "The parts of index NDF '^N' that overlap the "
                    "input NDF contain only bad pixels.", status );
            break;
         }

/* Store a pointer to the mapped index array. */
         pindex[ i ] = pdata->pindex;

/* Update the total number of bins in the output NDF. */
         nbin *= ( oubnd[ i ] - olbnd[ i ] + 1 );

/* Store the vector index strides between adjacent pixels on each output
   axis. */
         if( i == 0 ) {
            stride[ i ] = 1;
         } else {
            stride[ i ] = stride[ i - 1 ]*( oubnd[ i - 1 ] - olbnd[ i - 1 ] + 1 );
         }
      }
   }

/* Re-instate the original value of the NDF rounding flag. */
   ndfTune( round, "ROUND", status );

/* To speed things up, create an array with the shape and size of the
   input array, holding the zero-based vector index of the m-dimensional
   output pixel that corresponds to each input pixel. */
   vindex = astMalloc( el*sizeof(*vindex) );
   if( *status == SAI__OK ) {

/* Get the number of pixels to process in each worker thread. */
      if( el > nw ) {
         step = el/nw;
      } else {
         step = el;
         nw = 1;
      }

/* Submit a job to each worker thread to process a group of "step" input
   pixels. */
      for( i = 0; i < nw; i++ ) {

/* Store information to be passed to the worker thread. */
         pdata = job_data + i;
         pdata->operation = 2;
         pdata->p1 = i*step;
         if( i < nw - 1 ) {
            pdata->p2 = pdata->p1 + step - 1;
         } else {
            pdata->p2 = el - 1;
         }
         pdata->pindices = pindex;
         pdata->vindex = vindex;
         pdata->m = m;
         pdata->olbnds = olbnd;
         pdata->strides = stride;

/* Submit the job to the worker thread. */
         thrAddJob( wf, 0, pdata, pixbin_work, 0, NULL, status );
      }

/* Put the current thread to sleep until all the above jobs have
   completed. */
      thrWait( wf, status );
   }

/* Each pixel in the M-dimensional output NDF represents one bin into
   which input pixels can be placed. However, an extra output pixel axis
   is required that enumerate the values that fall in each bin. This final
   axis will have a lower bound of 1 and an upper bound equal to the
   maximum population in any one bin. So we now need to determine
   the maximum population in a single bin. To do this, we create an
   M-dimensional integer array (one pixel for each bin) filled with
   zeros, then use it to count the number of input pixels that fall in
   each bin. */
   binpop = astCalloc( nbin, sizeof( *binpop ) );
   if( *status == SAI__OK ) {

/* Loop round all pixels in the input array, maintaining a pointer to
   the corresponding element in the vindex array, which gives the index of
   the output pixel corresponding to the input pixel. Increment each
   corresponding value in binpop. */
      pvindex = vindex;
      for( j = 0; j < el; j++,pvindex++ ) {
         if( *pvindex != SIZE_MAX ) binpop[ *pvindex ]++;
      }

/* Find the maximum bin population. */
      maxpop = 0;
      pi = binpop;
      for( j = 0; j < nbin; j++,pi++ ) {
         if( *pi > maxpop ) maxpop = *pi;
      }

/* Set up the bounds of the final axis of the output NDF. */
      if( maxpop > 0 ) {
         olbnd[ m ] = 1;
         oubnd[ m ] = maxpop;
      } else if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( " ", "No output data to store.", status );
      }

/* Create the output NDF, propagating the Units component from the input in
   addition to the default list. Then set its bounds to those found above. */
      ndfProp( indf, "Units", "OUT", &indf2, status );
      ndfSbnd( m + 1, olbnd, oubnd, indf2, status );
   }

/* Create each required output array component in turn. */
   for( icomp = 0; icomp < 3 && *status == SAI__OK; icomp++ ) {

/* Check the component is defined in the input NDF. Continue on to the next
   component if not. */
      ndfState( indf, comps[ icomp ], &there, status );
      if( !there ) continue;

/* Get the stored data type of the array. The input and output arrays
   will have the same data type (ensured by ndfProp). */
      ndfType( indf, "Data", type, sizeof(type), status );

/* See if there may be any bad values in the input array. */
      ndfBad( indf, comps[ icomp ], 0, &bad, status );

/* Get the number of bytes in each value and store the bit pattern of the
   corresponding bad value in a char buffer. This is so that we do not
   need to write loads of explicitly generic code. */
      kpg1Badbits( type, badbuf, &bpp, status );

/* Map the array in its stored data type, initialising it to hold
   bad values. */
      ndfMap( indf2, comps[ icomp ], type, "Write/bad", (void **) &ptrout,
              &elout, status );

/* Map the input Data array in its stored data type (the same as the
   output NDF's native data type). */
      ndfMap( indf, comps[ icomp ], type, "Read", (void **) &ptrin, &el,
              status );

/* We re-use the binpop array to count the number of values currently
   in each bin. Initialise it by filling it with zeros. */
      memset( binpop, 0, nbin*sizeof( *binpop ) );

/* Loop round all pixels in the input array. */
      maxpop = 0;
      pvindex = vindex;
      for( j = 0; j < el; j++,pvindex++ ) {

/* If the input pixel is to be put into the output and the input value being
   copied is good... */
         if( *pvindex != SIZE_MAX && ISGOOD( ptrin ) ){

/* Get the index on the last axis of the output NDF at which to store the
   current input value. Increment the index at the same time. */
            jlast = ( binpop[ *pvindex ] )++;

/* Get the index within the output array at which to store the current
   input value. */
            iv = *pvindex + jlast*nbin;

/* Record the maximum number of good values stored in any bin. */
            if( jlast > maxpop ) maxpop = jlast;

/* Copy the input value to the output. */
            memcpy( ptrout + iv*bpp, ptrin, bpp );

         }

/* Increment the pointer t0 the next input value. */
         ptrin += bpp;
      }

/* Unmap the array to reduce peak memory usage. */
      ndfUnmap( indf, comps[ icomp ], status );
      ndfUnmap( indf2, comps[ icomp ], status );
   }

/* Truncate the output NDF to remove any trailing planes that are
   entirely bad. */
   maxpop++;
   if( maxpop < oubnd[ m ] ) {
      oubnd[ m ] = maxpop;
      ndfUnmap( indf2, "*", status );
      ndfSbnd( m + 1, olbnd, oubnd, indf2, status );
   }

/* Free resources. */
   grpDelet( &grp, status );
   binpop = astFree( binpop );
   job_data = astFree( job_data );
   vindex = astFree( vindex );

/* End the NDF context */
   ndfEnd( status );

/* If an error has occurred, issue another error report identifying the
   program which has failed (i.e. this one). */
   if( *status != SAI__OK ) {
      errRep( " ", "PIXBIN: Failed to bin the pixels in an NDF.", status );
   }
}












static void pixbin_work( void *job_data_ptr, int *status ){
/*
*  Name:
*     pixbin_work

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     pixbin.

*  Invocation:
*     void pixbin_work( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = PixbinData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/


/* Local Variables: */
   PixbinData *pdata;
   hdsdim *olbnd;
   int *pi;
   int *pin[ NDF__MXDIM ];
   int hi;
   int i;
   int ii;
   int ix;
   int lo;
   int m;
   size_t *pvindex;
   size_t *stride;
   size_t el;
   size_t iv;
   size_t j;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (PixbinData *) job_data_ptr;

/* Get a section of an index NDF that matches the input NDF, map its
   Data array as _INTEGER, and get the maximum and minimum data value
   in the NDF. */
   if( pdata->operation == 1 ) {
      ndfLock( pdata->tndf, status );
      ndfSect( pdata->tndf, pdata->n, pdata->lbnd, pdata->ubnd, &pdata->sndf,
               status );
      ndfMap( pdata->sndf, "Data", "_INTEGER", "Read",
              (void **) &pdata->pindex, &el, status );
      lo = VAL__MAXI;
      hi = VAL__MINI;
      if( *status == SAI__OK ) {
         pi = pdata->pindex;
         for( j = 0; j < el; j++,pi++ ) {
            if( *pi != VAL__BADI ) {
               if( *pi < lo ) lo = *pi;
               if( *pi > hi ) hi = *pi;
            }
         }
      }
      pdata->olbnd = lo;
      pdata->oubnd = hi;

/* The tndf and sndf identifier refer to the same base NDF. So unlocking
   one (using ndfUnlock) will annull the other. The main thread needs to
   use the mapped Data array associated with sndf, so we unlock sndf. We
   also annull tndf for good measure, although it is not strictly necessary
   since unlocking sndf will annull tndf. */
      ndfAnnul( &pdata->tndf, status );
      ndfUnlock( pdata->sndf, status );

/* Get the zero-based pixel index within the m-dimensional output array
   for a specified set of input pixels. */
   } else if( pdata->operation == 2 ) {
      m = pdata->m;
      olbnd = pdata->olbnds;
      stride = pdata->strides;

/* Initialise pointers into the "m" index arrays. They initially point
   to the value associated with the first pixel to be processed by this
   thread. */
      for( i = 0; i < m; i++ ) {
         pin[ i ] = pdata->pindices[ i ] + pdata->p1;
      }

/* Initialise the pointer to the first zero-based index value to be
   returned by this thread. */
      pvindex = pdata->vindex + pdata->p1;

/* Loop round all the input pixels to be processed by this thread. */
      for( j = pdata->p1; j <= pdata->p2; j++ ) {

/* Initialise the vector index for this input pixel, then loop round each
   index array. */
         iv = 0;
         for( i = 0; i < m; i++ ) {


/* Get the pixel index on output axis "i" for the current input pixel.
   Increment the pointer ready for the next input pixel. */
            ix = *( pin[ i ]++ );

/* If the output pixel index is good, update the zero-based vector index
   of the output pixel. Otherwise, set a special value (SIZE_MAX) for the
   vector index and break out of the axis loop. */
            if( ix != VAL__BADI ) {
               iv += ( ix - olbnd[ i ] )*stride[ i ];
            } else {
               iv = SIZE_MAX;
               for( ii = i + 1; ii < m; ii++ ) pin[ ii ]++;
               break;
            }
         }

/* Store the vector index in the vindex array and increment the pointer
   ready for the next input pixel. */
         *(pvindex++) = iv;
      }

/* Report an error for any other operation. */
   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "pixbin_work: Illegal operation %d (programming error).",
               status, pdata->operation );
   }

}
