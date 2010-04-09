
/*
*+
*  Name:
*     smf_check_quality

*  Purpose:
*     Check for consistency between quality and data array

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     size_t smf_check_quality( smfData *data,
*                               unsigned char *quality,
*                               int showbad, int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData
*     quality = unsigned char* (Given)
*        If defined check this quality array instead of that supplied with data
*     showbad = int (Given)
*        If set, display locations where they where inconsistencies found.
*     status = int* (Given and Returned)
*        Pointer to global status.

* Return Value:
*     size_t = count of inconsistent samples found.

*  Description:
*     Traverse the data and quality arrays. Any VAL__BADD encountered without
*     SMF__Q_BADDA set are inconsistencies. Also, any non-finite
*     values in the data array other than VAL__BADD (e.g. inf/NaN) are
*     considered inconsistencies as they should have been converted
*     previously using smf_convert_bad. Finally, every instance of
*     SMF__Q_BADDA flag must match a VAL__BADD in the data array.

*  Notes:
*     Will only handle SMF__DOUBLE data type. Will set bad status otherwise.
*     Returns 0 if bad status.

*  Authors:
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-03-31 (EC):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 University of British Columbia.
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
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Other includes */

#define FUNC_NAME "smf_check_quality"

size_t smf_check_quality( smfData *data, unsigned char *quality,
                          int showbad, int *status ) {

  int badqual;                  /* Bad quality at this sample? */
  double *d=NULL;               /* Pointer to data array */
  size_t i;                     /* loop counter */
  int isbad;                    /* inconsistency found */
  size_t j;                     /* loop counter */
  size_t nbad=0;                /* inconsistency counter */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
  dim_t ntslice;                /* Number of time slices */
  size_t bstride;               /* bol stride */
  size_t tstride;               /* time slice stride */
  unsigned char *qual=NULL;     /* Pointer to the QUALITY array */
  double val;                   /* Value from DATA array */

  if ( *status != SAI__OK ) return 0;

  /* Check for DATA */
  if( !data ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL data supplied", status );
    return 0;
  }

  if( !data->pntr[0] ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData does not contain a DATA component",
            status );
    return 0;
  }

  if( data->dtype != SMF__DOUBLE ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData does not have type SMF__DOUBLE", status );
    return 0;
  }

  d = (double *) data->pntr[0];

  /* Check for QUALITY */
  if( quality ) {
    qual = (unsigned char *) quality;       /* external QUALITY */
  } else {
    qual = (unsigned char *) data->pntr[2]; /* QUALITY given by smfData */
  }

  if( !qual ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL quality supplied", status);
    return 0;
  }


  /* Calculate data dimensions */
  smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, &ndata, &bstride,
                &tstride, status );

  if( *status == SAI__OK ) {

    /* Traverse array checking for inconsistencies */
    for( i=0; i<nbolo; i++ ) {
      for( j=0; j<ntslice; j++ ) {
        isbad = 0;

        badqual = qual[i*bstride+j*tstride]&SMF__Q_BADDA;
        val = d[i*bstride+j*tstride];

        if( (val==VAL__BADD) && !badqual ) {
          isbad = 1;
          if( showbad ) {
            msgOutf( "", "b%zu t%zu: VAL__BADD without SMF__Q_BADDA",
                     status, i, j );
          }
        }

        if( badqual && (val!=VAL__BADD) ) {
          isbad = 1;
          if( showbad ) {
            msgOutf( "", "b%zu t%zu: SMF__Q_BADDA without VAL__BADD",
                     status, i, j );
          }
        }

        if( !isfinite(val) ) {
          isbad = 1;
          if( showbad ) {
            msgOutf( "", "b%zu t%zu: non-finite value encountered",
                     status, i, j );
          }
        }

        if( isbad ) {
          nbad++;
        }
      }
    }
  }

  return nbad;
}
