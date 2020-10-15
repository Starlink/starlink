/*
*+
*  Name:
*     smf_dataOrder_array

*  Purpose:
*     Low-level data buffer re-ordering function

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     newbuf = smf_dataOrder_array( ThrWorkForce *wf, void * oldbuf,
*                                   smf_dtype oldtype, smf_dtype newtype,
*                                   dim_t ndata, dim_t ntslice,
*                                   dim_t nbolo, dim_t tstr1, dim_t bstr1,
*                                   dim_t tstr2, dim_t bstr2, int inPlace,
*                                   int freeOld, int * status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL). [Currently
*        unused since using multiple threads slows down the algorithm
*        rather than speeding it up.]
*     oldbuf = void * (Given and Returned)
*        Pointer to the data buffer to be re-ordered. Also contains the
*        re-ordered data if inPlace=1
*     oldtype = smf_dtype (Given)
*        Data type of the original buffer
*     newtype = smf_dtype (Given)
*        Data type of the target buffer
*     ndata = dim_t (Given)
*        Number of elements in oldbuf
*     ntslice = dim_t (Given)
*        Number of time slices in oldbuf
*     nbolo = dim_t (Given)
*        Number of bolometers in oldbuf
*     tstr1 = dim_t (Given)
*        Time stride of oldbuf
*     bstr1 = dim_t (Given)
*        Bolo stride of oldbuf
*     tstr2 = dim_t (Given)
*        Time stride of re-ordered buffer
*     bstr2 = dim_t (Given)
*        Bolo stride of re-ordered buffer
*     inPlace = int (Given)
*        If set replace the contents of oldbuf with the re-ordered data.
*     freeOld = int (Given)
*        If set, and inPlace=0, free oldbuf before returning.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Provide pointer to void. Reorder that array in some new
*     workspace then either copy everything back into the supplied
*     array and return it or free the memory (assumed to be malloced
*     and not mmapped) and return the new workspace.

*  Returned Value:
*     newbuf = void *
*        Pointer to the re-ordered data.

*  Notes:
*     Will do nothing if the input and output strides are the same,
*     the inPlace flag is true, and the data types are the same.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-08-31 (EC):
*        Move private routine written by TIMJ out of smf_dataOrder.c
*     2010-09-17 (EC):
*        Add freeOrder flag in case routine is being used for copy
*     2011-04-25 (TIMJ):
*        Trap case when input and output strides are the same.
*     2011-06-22 (EC):
*        Add ability to typecast while copying (oldtype/newtype)
*     2014-01-13 (DSB):
*        Added multi-threading.

*  Notes:

*  Copyright:
*     Copyright (C) 2010-2014 Science & Technology Facilities Council.
*     Copyright (C) 2010-2011 University of British Columbia.
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

#include <stdio.h>

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Prototypes for local static functions. */
static void smf1_dataOrder_array( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfDataOrderArrayData {
   dim_t itime1;
   dim_t itime2;
   dim_t nbolo;
   dim_t bstr1;
   dim_t tstr1;
   dim_t bstr2;
   dim_t tstr2;
   smf_dtype newtype;
   smf_dtype oldtype;
   void *oldbuf;
   void *newbuf;
} SmfDataOrderArrayData;

#define FUNC_NAME "smf_dataOrder_array"

#define COPY1(Type) { \
         Type *pout, *qout; \
         Type *pin, *qin; \
\
         qin = (Type *) pdata->oldbuf + itime1*tstr1; \
         qout = (Type *) pdata->newbuf + itime1*tstr2; \
\
         for( itime=itime1; itime<=itime2; itime++ ) { \
            pin = qin; \
            pout = qout; \
            for( ibolo=0; ibolo<nbolo; ibolo++ ) { \
               *pout = *pin; \
               pin += bstr1; \
               pout += bstr2; \
            } \
            qin += tstr1; \
            qout += tstr2; \
         } }

#define COPY2(Type_in,Type_out,Bad_in,Bad_out) { \
         Type_out *pout, *qout; \
         Type_in *pin, *qin; \
\
         qin = (Type_in *) pdata->oldbuf + itime1*tstr1; \
         qout = (Type_out *) pdata->newbuf + itime1*tstr2; \
\
         for( itime=itime1; itime<=itime2; itime++ ) { \
            pin = qin; \
            pout = qout; \
            for( ibolo=0; ibolo<nbolo; ibolo++ ) { \
               *pout = ( *pin != Bad_in ) ? *pin : Bad_out; \
               pin += bstr1; \
               pout += bstr2; \
            } \
            qin += tstr1; \
            qout += tstr2; \
         } }


void * smf_dataOrder_array( ThrWorkForce *wf, void * oldbuf, smf_dtype oldtype,
                            smf_dtype newtype, dim_t ndata, dim_t ntslice,
                            dim_t nbolo, dim_t tstr1, dim_t bstr1,
                            dim_t tstr2, dim_t bstr2, int inPlace,
                            int freeOld, int * status ) {
  dim_t sznew = 0;        /* Size of new data type */
  void * newbuf = NULL;    /* Space to do the reordering */
  void * retval = NULL;    /* Return value with reordered buffer */
  SmfDataOrderArrayData *job_data = NULL;
  SmfDataOrderArrayData *pdata;
  int nw;
  dim_t step;
  int iw;

  retval = oldbuf;
  if (*status != SAI__OK) return retval;
  if (!retval) return retval;

  /* Can't do inPlace without realloc'ing if data types don't match...
     so generate bad status for now */
  if( (newtype != oldtype) && inPlace ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": inPlace not supported if newtype != oldtype",
            status );
    return retval;
  }

  /* For now the only data conversion that is supported is from SMF__INTEGER
     to SMF__DOUBLE */
  if( (oldtype!=newtype) &&
      ((newtype!=SMF__DOUBLE) || (oldtype!=SMF__INTEGER)) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME
            ": type conversion only supported from integer -> double",
            status );
    return retval;
  }

  /* Special case the inPlace variant with no reordering / typecasting */
  if ( inPlace && tstr1 == tstr2 && bstr1 == bstr2 && oldtype == newtype ) {
    return retval;
  }

  /* Size of data type */
  sznew = smf_dtype_sz(newtype, status);

  /* Allocate buffer */
  newbuf = astMalloc( ndata*sznew );

  if( *status == SAI__OK ) {

    /* if the input and output strides are the same, and the data types are
       the same,  we just memcpy */
    if ( tstr1 == tstr2 && bstr1 == bstr2 && oldtype == newtype ) {

      memcpy( newbuf, oldbuf, sznew*ndata );

    } else {

      /* We currently ignore any supplied WorkForce, since using multiple
         threads slows down the re-ordered rather than speeding it up - I
         presume because of contention issues. I've tried ensuring that
         the output array is accessed sequenctially but that does not seem
         to improve anything. Leave the mult-threaded infrastructure
         here, in case a better solution is found. */
      wf = NULL;

      /* How many threads do we get to play with */
      nw = wf ? wf->nworker : 1;

      /* Find how many time slices to process in each worker thread. */
      step = ntslice/nw;
      if( ntslice == 0 ) step = 1;

      /* Allocate job data for threads, and store common values. Ensure that
         the last thread picks up any left-over time slices.  Store the
         info required by the wrker threads, then submit jobs to the work
         force. */
      job_data = astCalloc( nw, sizeof(*job_data) );
      if( *status == SAI__OK ) {

        for( iw = 0; iw < nw; iw++ ) {
          pdata = job_data + iw;
          pdata->itime1 = iw*step;
          if( iw < nw - 1 ) {
            pdata->itime2 = pdata->itime1 + step - 1;
          } else {
            pdata->itime2 = ntslice - 1 ;
          }

          pdata->nbolo = nbolo;
          pdata->bstr1 = bstr1;
          pdata->tstr1 = tstr1;
          pdata->bstr2 = bstr2;
          pdata->tstr2 = tstr2;
          pdata->newtype = newtype;
          pdata->oldtype = oldtype;
          pdata->newbuf = newbuf;
          pdata->oldbuf = oldbuf;

          thrAddJob( wf, 0, pdata, smf1_dataOrder_array, 0, NULL, status );
        }

        /* Wait for the jobs to complete. */
        thrWait( wf, status );
      }

      job_data = astFree( job_data );
    }

    if( inPlace ) {
      /* Copy newbuf to oldbuf */
      memcpy( oldbuf, newbuf, ndata*sznew );
      /* Free newbuf */
      newbuf = astFree( newbuf );

      retval = oldbuf;
    } else {

      if( freeOld ) {
        /* Free oldbuf */
        oldbuf = astFree( oldbuf );
      }

      /* Set pntr to newbuf */
      retval = newbuf;
    }
  }

  return retval;
}


static void smf1_dataOrder_array( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_dataOrder_array

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_dataOrder_array.

*  Invocation:
*     smf1_dataOrder_array( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfDataOrderArrayData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfDataOrderArrayData *pdata;
   dim_t itime;
   dim_t ibolo;
   dim_t itime1;
   dim_t itime2;
   dim_t bstr1;
   dim_t tstr1;
   dim_t bstr2;
   dim_t tstr2;
   dim_t nbolo;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfDataOrderArrayData *) job_data_ptr;

   itime1 = pdata->itime1;
   itime2 = pdata->itime2;
   nbolo = pdata->nbolo;
   bstr1 = pdata->bstr1;
   tstr1 = pdata->tstr1;
   bstr2 = pdata->bstr2;
   tstr2 = pdata->tstr2;

/* Loop over all of the elements and re-order the data */
   switch( pdata->oldtype ) {

   case SMF__INTEGER:
      if( pdata->newtype == SMF__DOUBLE ) {
         COPY2(int,double,VAL__BADI,VAL__BADD)
      } else {
         COPY1(int)
      }
      break;

   case SMF__FLOAT:
     COPY1(float)
     break;

   case SMF__DOUBLE:
     COPY1(double)
     break;

   case SMF__USHORT:
     COPY1(unsigned short)
     break;

   case SMF__UBYTE:
     COPY1(unsigned char)
     break;

   default:
     msgSetc("DTYPE",smf_dtype_str(pdata->oldtype, status));
     *status = SAI__ERROR;
     errRep( "", FUNC_NAME
             ": Don't know how to handle ^DTYPE type.", status);
   }

}

