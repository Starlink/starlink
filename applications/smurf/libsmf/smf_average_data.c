/*
*+
*  Name:
*     smf_average_data

*  Purpose:
*     Average SCUBA-2 timestream data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_average_data( const smfData *data, int start,  int nslice, 
*                       const int interval, double **avdata, size_t *nelem,
*                       int *status);

*  Arguments:
*     data = const smfData * (Given)
*        Input data
*     start = int (Given)
*        Index of starting time slice
*     nslice = int (Given)
*        Number of time slices to average together
*     interval = const int (Given)
*        Averaging interval
*     avdata = double ** (Returned)
*        Pointer to 2 or 3-D array of averaged data. Must be freed by
*        caller.
*     nelem = size_t * (Returned)
*        Number of elements in the averaged data
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description: 
*     This routine....  For DREAM data, interval should be set to the
*     number of samples per cycle (nsampsycle); for STARE it should be
*     1.
*
*     To get a single average frame returned, set interval to 1 and
*     start to the desired starting index.
*
*     To average the whole time series into a single frame, set nslice
*     to 0, irrespective of the value of interval.

*  Notes:
*     The caller should free the memory allocated in avdata by this routine
*     by using smf_free.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-08-21 (AGG):
*        Initial version
*     2007-07-06 (AGG):
*        - Ignore bad values
*        - Check averaging window is a factor of the number of samples
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-05-29 (TIMJ):
*        Clarify that someone needs to free this memory.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006-2007 University of British Columbia. All
*     Rights Reserved.

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

/* Standard includes */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* Starlink includes */
#include "sae_par.h"
#include "star/ndg.h"
#include "ndf.h"
#include "ast.h"
#include "mers.h"
#include "prm_par.h"
#include "dat_par.h"
#include "star/hds.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"
#include "smurf_par.h"

/* SC2DA includes */
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"
#include "sc2da/dream_par.h"

#define FUNC_NAME "smf_average_data"

void smf_average_data( const smfData *data, int start,  int nslice, 
                       const int interval, double **avdata, size_t *nelem, int *status) {

  int base;                 /* Base index */
  double currentdata;       /* Value of current data point */
  int i;                    /* Loop counter */
  int j;                    /* Loop counter */
  int k;                    /* Loop counter */
  int lastslice;            /* Index of the last time slice in the current sum */
  int nbol;                 /* Number of data points per timeslice (bolometers) */
  int nframes;              /* Number of time slices in input data */
  int nsamples;             /* Number of time slices in the average */
  int noutslice = 1;        /* Number of output time slices */
  int offset;               /* Offset into output array for current data value */
  double sum;               /* Sum of input data */
  double *tstream = NULL;   /* Pointer to input data stream */

  if ( *status != SAI__OK ) return;

  /* Do we have 3-d time-series data? */
  if ( data->ndims != 3 ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti("N",data->ndims);
      errRep( FUNC_NAME, 
              "Input data not in time-series format (ndims = ^N, should be 3)", 
              status );
      return;
    }
  }

  /* check type */
  if (data->dtype != SMF__DOUBLE) {
    *status = SAI__ERROR;
    msgSetc( "TYP", smf_dtype_str(data->dtype, status ) );
    errRep(" ", FUNC_NAME ": This function averages _DOUBLE only not ^TYP",
           status );
    return;
  }

  /* OK we have 3-D data so retrieve the number of time slices */
  nframes = (data->dims)[2];
  /* Check that the starting value is valid */
  if ( start < 0 || start > nframes-1 ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti("I",start);
      msgSeti("N",nframes-1);
      errRep( FUNC_NAME, 
              "Starting index, ^I, out of range (must lie between 0 and ^N)", 
              status );
      return;
    }
  }
  /* Check that the number of time slices to average is valid */
  if ( nslice < 1 || nslice > nframes ) {
    if ( nslice == 0 ) {
      nslice = nframes;
      start = 0;
    } else {
      if ( *status == SAI__OK ) {
        *status = SAI__ERROR;
        msgSeti("I",nslice);
        msgSeti("N",nframes);
        errRep( FUNC_NAME, 
                "Number of time slices to average, ^I, out of range (must lie between 1 and ^N)", 
                status );
        return;
      }
    }
  }
  /* Check that the averaging interval is valid */
  if ( interval < 1 || interval > nframes ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti("I",interval);
      msgSeti("N",nframes);
      errRep( FUNC_NAME, 
              "Averaging interval, ^I, out of range (must lie between 1 and ^N)", 
              status );
      return;
    }
  }

  /* Check that if interval is not unity then there are an integral
     number of intervals in the data range */
  /* Also calculate the number of elements in the output data */
  if ( interval != 1 ) {
    noutslice = nframes / nslice;
    /* Add 1 to the number of output slices if nslice is not a factor
       of nframes */
    if ( noutslice != nframes%nslice ) {
      /* Tell user - would be good to store this knowledge in the
         output file somehow */
      msgSeti("S",nslice);
      msgSeti("N",noutslice);
      msgSeti("F",nframes);
      msgOutif(MSG__VERB, FUNC_NAME, 
               "Warning: time stream does not contain an integer number of output time slices: noutslice * nslice (^N * ^S) != nframes (^F)", 
               status);
      msgOutif(MSG__VERB, FUNC_NAME, 
               "The final image will be made up from fewer samples than the rest", 
               status);
      /* Increment the number of output slices */
      noutslice++;
    }
  } else {
    noutslice = interval;
  }

  nbol = (data->dims)[0] * (data->dims)[1];
  *nelem = nbol * noutslice;

  /* Retrieve pointer to the input data */
  tstream = (data->pntr)[0];
  /* Allocate space for output data */
  *avdata = smf_malloc( *nelem, sizeof(double), 1, status );
  if ( *status == SAI__OK ) {
    /* All's well so calculate average values */
    base = start + interval - 1;
    /* Begin by looping over the number of output time slices */
    for ( i=0; i<noutslice; i++ ) {
      offset = i*nbol;
      if (i == noutslice - 1) {
        lastslice = nframes;
      } else {
        lastslice = base + nslice;
      }
      /* Then for each element in the current output time slice.... */
      for ( j=0; j<nbol; j++ ) {
        sum = 0.0;
        nsamples = 0;
        /* Sum all the contributions from that element in the desired
           range of time slices */
        for ( k=base; k<lastslice; k++ ) {
          currentdata = tstream[j + k*nbol];
          if ( currentdata != VAL__BADD ) {
            sum += currentdata;
            nsamples++;
          }
        }
        /* Calculate the average and store it in the output array */
        sum /= nsamples;
        (*avdata)[j + offset] = sum;
      }
    }
  } else {
    /* cleanup resources */
    *avdata = smf_free( *avdata, status);
  }
}

