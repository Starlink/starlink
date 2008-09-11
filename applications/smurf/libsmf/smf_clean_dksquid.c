/*
*+
*  Name:
*     smf_clean_dksquid

*  Purpose:
*     Clean raw smfData by removing a scaled, offset version of the dark squid

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_clean_dksquid( smfData *indata, size_t window, int *status );

*  Arguments:
*     indata = smfData * (Given)
*        Pointer to the input smfData. Should be raw, un-flatfielded.
*     window = size_t (Given)
*        Width of boxcar smooth for squid before fitting and removing
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

*  Description: 
*     Columns of SCUBA-2 detectors exhibit a strong correlated signal
*     which is shown clearly in the dark squids. As the dark squids do
*     not see astronomical or atmospheric signal they can be used as a
*     template to remove this correlated noise without risk of
*     removing signal from real sources. The dark squids are smoothed
*     to avoid increasing the white noise level of detectors once they
*     are subtracted. They are fit to the raw (un-flatfielded)
*     detector by solving for a gain and offset using the explicit
*     least-squares solution. If a dark squid is constant this routine
*     flags every detector in the column as SMF__Q_BADB.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-08-27 (EC):
*        Initial version
*     2008-09-11 (EC):
*        -flag SMF__Q_BADB all bolos in column if dark squid is dead
*        -fixed array index bug

*  Copyright:
*     Copyright (C) 2008 University of British Columbia.
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
#include "ast.h"
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "prm_par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_clean_dksquid"

void smf_clean_dksquid( smfData *indata, size_t window, int *status ) {

  size_t arrayoff;        /* Array offset */
  double corr;            /* Linear correlation coefficient */
  int dkgood;             /* Flag for non-constant dark squid */ 
  double *dksquid=NULL;   /* Buffer for smoothed dark squid */
  int firstdk;            /* First value in dksquid signal */
  double gain;            /* Gain parameter from template fit */
  size_t i;               /* Loop counter */
  dim_t index;            /* index into buffer */
  int isTordered;         /* Data order */
  size_t j;               /* Loop counter */
  size_t k;               /* Loop counter */
  dim_t nbolo;            /* Number of bolometers */
  dim_t ncol;             /* Number of columns */
  dim_t ndata;            /* Number of data points */
  dim_t nrow;             /* Number of rows */
  dim_t ntslice;          /* Number of time slices */
  double offset;          /* Offset parameter from template fit */
  unsigned char *qua=NULL;/* Pointer to quality array */
  dim_t stride;           /* Distance between bolo samples in array */


  if (*status != SAI__OK) return;

  /* Check for NULL smfData pointer */
  if( !indata ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME 
            ": possible programming error, smfData pointer is NULL", status );
    return;
  }

  /* Check for NULL smfDA */
  if( !indata->da) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME 
            ": possible programming error, no smfDA struct in smfData", status);
    return;
  }

  /* Check for NULL dksquid */
  if( !indata->da->dksquid) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME 
            ": possible programming error, no dksquid array in smfData", 
            status);
    return;
  }

  /* Check for 3-d data and get dimensions */
  smf_get_dims( indata, &nbolo, &ntslice, &ndata, status );

  /* Obtain the number of columns */
  isTordered = indata->isTordered;
  if( isTordered ) {
    ncol = indata->dims[SMF__COL_INDEX];    /* x, y, t */
    nrow = indata->dims[SMF__ROW_INDEX];
  } else {
    ncol = indata->dims[1+SMF__COL_INDEX];  /* t, x, y */
    nrow = indata->dims[1+SMF__ROW_INDEX];
  }

  /* Pointer to quality */
  qua = indata->pntr[2];

  /* Allocate space for a dark squid */
  dksquid = smf_malloc( ntslice, sizeof(*dksquid), 0, status );

  /* Loop over columns */
  for( i=0; (*status==SAI__OK)&&(i<ncol); i++ ) {
    
    /* Copy the dark squid into an array (always time-ordered), while 
       checking to see if it is a constant value (dead) */
    index = i;
    firstdk = indata->da->dksquid[index];
    dkgood = 0;
    for( j=0; j<ntslice; j++ ) {
      dksquid[j] = indata->da->dksquid[index];
      index += ncol;
      if( dksquid[j] != firstdk ) dkgood=1;
    }

    if( dkgood ) {
      /* Smooth the dark squid template */
      smf_boxcar1D( dksquid, ntslice, window, NULL, 0, status );
    } 

    /* Loop over rows, removing the fitted dksquid template. */
    for( j=0; (*status==SAI__OK) && (j<nrow); j++ ) {

      /* Calculate index of first sample for this bolometer, and the stride */
      if( SMF__COL_INDEX ) {
        index = i*nrow + j;
      } else {
        index = i + j*ncol;
      }

      if( isTordered ) {
        stride = nbolo;
      } else {
        stride = 1;
        index *= ntslice;
      }

      /* If dark squid is bad, flag this entire bolo as bad */
      if( !dkgood && qua ) {
        arrayoff = index;
        for( k=0; k<ntslice; k++ ) {
          qua[arrayoff] |= SMF__Q_BADB;
          arrayoff += stride;
        }
      }

      /* Continue if dkgood, and no indication of a bad bolometer */      
      if( dkgood && (!qua || !(qua[index]&SMF__Q_BADB)) ) {

        switch( indata->dtype ) {
        case SMF__DOUBLE:
          smf_templateFit1D( &( ((double *)indata->pntr[0])[index] ),
                             ntslice, stride, dksquid, 1, &gain, &offset, 
                             &corr, status );
          break;
          
        case SMF__INTEGER:
          smf_templateFit1I( &( ((int *)indata->pntr[0])[index] ),
                             ntslice, stride, dksquid, 1, &gain, &offset, 
                             &corr, status );
          break;
          
        default:
          msgSetc( "DT", smf_dtype_string( indata, status ));
          *status = SAI__ERROR;
          errRep( " ", "Unsupported data type for dksquid cleaning (^DT)",
                  status );
        }
       
        /* Annul SMF__INSMP as it was probably due to a bad bolometer */
        if( *status == SMF__INSMP ) {
          errAnnul( status );
          msgSeti( "COL", i );
          msgSeti( "ROW", j );
          msgOutif( MSG__DEBUG, "", FUNC_NAME
                    ": ROW,COL (^ROW,^COL) insufficient good samples", status );
        } else {
          msgSeti( "COL", i );
          msgSeti( "ROW", j );
          msgSetd( "GAI", gain );
          msgSetd( "OFF", offset );
          msgSetd( "CORR", corr );
          msgOutif( MSG__DEBUG, "", FUNC_NAME
                    ": ROW,COL (^ROW,^COL) GAIN,OFFSET,CORR (^GAI,^OFF,^CORR)",
                    status );
        }
      }
    }
  }

  /* Clean Up */
  if( dksquid ) dksquid = smf_free( dksquid, status );
}
