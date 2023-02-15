/*
*+
*  Name:
*     smf_fix_data

*  Purpose:
*     Fix observation data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_fix_data( msglev_t msglev, smfData *data, int * status );

*  Arguments:
*     msglev = msglev_t (Given)
*        Messaging level to be used for output information. This allows a
*        task whose entire job is to fix up meta data to report information
*        at default level whereas a task that simply wants the data fixed before
*        proceeding could use a debug level.
*     data = smfData * (Given)
*        smfData to be examined and fixed.
*     status = int * (Given & Returned)
*        Pointer to global status

*  Returned Value:
*     Returns int indicating whether the meta data were modified. 0 indicates
*     no modifications were made. Bits corresponding to the smf_metadata_fixups
*     enum will be used to indicate which parts of the meta data were modified.

*  Description:
*     Analyzes the smfData struct and determines whether data
*     need repairing. It is assumed that smf_fix_metadata has already
*     been run, so header information (which must be present) should be
*     accurate. The disk file associated with the smfData will not be updated.
*     The data component is required to be mapped.

*  Authors:
*     EC: Ed Chapin (UBC)

*  Notes:
*     o Works on SCUBA-2 time-series data.  All other smfDatas cause the
*     function to return without comment.
*     o If no header is present routine will also return without comment.
*     o A check is made for a DATA component. However, if a VARIANCE and/
*       or quality component exist but are not mapped, inconsistencies
*       could arise.

*  History:
*     2011-07-08 (EC):
*        Initial version based on smf_fix_metada: to handle s4a row readout
*        order error from May/June 2011.
*     2011-07-11 (EC):
*        Replace values in dead row with zero instead of VAL__BAD?

*  Copyright:
*     Copyright (C) 2011 University of British Columbia.
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

#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "prm_par.h"
#include "mers.h"
#include "star/one.h"

#include "smf.h"
#include "smf_err.h"

#include <stdio.h>
#include <strings.h>

#define FUNC_NAME "smf_fix_data"

/* Name of the FITS entry for storing the fixed data flag */
#define SMFFIXDATA "FIXDATA"

/* Indent for informational messages */
#define INDENT "   "

/* Define missing row for the s4a may/june 2011 readout error */
#define FIXDATA_S4A_MISSROW 14

int smf_fix_data ( msglev_t msglev, smfData * data, int * status ) {

  int have_fixed = 0;        /* Did we fix anything? */
  smfHead *hdr = NULL;       /* Data header struct */
  dim_t i;                  /* Loop counter */
  dim_t j;                  /* Loop counter */
  dim_t k;                  /* Loop counter */
  dim_t l;                  /* Loop counter */
  sc2ast_subarray_t subnum;  /* subarray number */
  int was_fixed = 0;         /* Were these data fixed before? */

  if (*status != SAI__OK) return have_fixed;

  /* Validate arguments - need smfData with smfHead including FITS
     and state */
  smf_validate_smfData( data, 1, 0, status );
  if (*status != SAI__OK) return have_fixed;

  hdr = data->hdr;
  smf_validate_smfHead( hdr, 1, 1, status );
  if (*status != SAI__OK) return have_fixed;

  /* Only works on SCUBA-2 time-series data */
  if( (hdr->instrument != INST__SCUBA2) ||
      (data->ndims != 3) ) {
    return have_fixed;
  }

  /* Need to at least have a DATA component. It might be a good idea
     to check if there is a file associated with the data, and check
     to see whether the caller mapped the DATA, VARIANCE, QUALITY. If
     they only mapped a subset of those, we could generate a warning that
     this routine may be introducing an inconsistency */
  if( !data->pntr[0] ) {
    return have_fixed;
  }

  /* Check to see if a FIXDATA FITS keyword exists -- if so, smf_fix_data
     has already been run on the data and we return. */
  smf_getfitsi( hdr, SMFFIXDATA, &was_fixed, status );
  if( *status == SMF__NOKWRD ) {
    errAnnul( status );
  } else {
    return have_fixed;
  }

  /* Fix the May/June 2011 s4a row readout order error. Check the
     subarray first, then get the MJD for the start of the observation
     to decide if we need to proceed. */

  smf_find_subarray( hdr, NULL, 0, &subnum, status );
  if( (*status == SAI__OK) && (subnum == S4A) ) {
    double dateendfix;       /* UTC MJD end date when fix needed */
    double dateobs;          /* UTC MJD observation start */
    double datestartfix;     /* UTS MJD start date when fix needed */
    AstTimeFrame *tf = NULL; /* time frame for date conversion */

    /* Get the MJD for start of the observation */
    smf_find_dateobs( hdr, &dateobs, NULL, status );

    /* Get the MJD for the first and last dates bracketing the period
       during which the row order fix needs to be done */

    tf = astTimeFrame( " " );
    astSet( tf, "TimeScale=UTC" );

    /* From Mike Macintosh Thu, 7 Jul 2011 15:07:51 +0000:

       "I did a scan of the engineering MCE status files and it looks
       the 'missing' row on s4a originated between 09:30 and 10:30 HST
       on 26 May 2011. I haven't checked in detail whether the
       'missing' row was consistently missing after that date but the
       assumption is that it was."
    */

    astSet( tf, "TimeOrigin=%s", "2011-05-26T19:00:00" );
    datestartfix = astGetD( tf, "TimeOrigin" );

    /* From Dan Bintley Wed, 06 Jul 2011 00:23:18 -1000:

       "I have changed the source code in

       /jac_sw/itsroot/scuba2_src/scuba2Da/setup/SG450_M1004D1000/mce_zero.txt"
    */

    astSet( tf, "TimeOrigin=%s", "2011-07-06T10:23:00" );
    dateendfix = astGetD( tf, "TimeOrigin" );

    tf = astAnnul( tf );

    /* Proceed with fix if within date range */
    if( (*status == SAI__OK) && (dateobs >= datestartfix) &&
        (dateobs <= dateendfix) ) {
      dim_t targetbolo;
      dim_t sourcebolo;
      dim_t bstride;
      dim_t ncols;
      dim_t nrows;
      dim_t ntslice;
      dim_t tstride;

      msgOutif( msglev, "", INDENT "Reparing row order readout error", status );

      smf_get_dims( data, &nrows, &ncols, NULL, &ntslice, NULL, &bstride,
                    &tstride, status );

      if( (*status==SAI__OK) && (nrows != 40) ) {
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME ": looks like we need to fix the data, but we "
                "don't have 40 rows.", status );
      }

      /* Rows 0--13 are OK, row 14 was not read out
         (FIXDATA_S4A_MISSROW), so we need to move all of rows 14--38
         up to the range 15--39. After doing the shift, overwrite the
         missing row 14 with 0 (or should this be VAL__BAD? in some
         situations?) */

      if( *status==SAI__OK ) {
        /* Row counter */
        for( i=nrows-1; (*status==SAI__OK)&&(i>=(FIXDATA_S4A_MISSROW+1)); i-- ){
          for( j=0; (*status==SAI__OK)&&(j<ncols); j++ ) {
            smf_qual_t *qual = data->qual;

            /* Calculate the bolo index */
            if( SC2STORE__ROW_INDEX ) {
              /* Fastest changing index is column number */
              sourcebolo = (i-1)*ncols + j;
              targetbolo = i*ncols + j;
            } else {
              /* Fastest changing index is row number */
              sourcebolo = (i-1) + j*nrows;
              targetbolo = i + j*nrows;
            }

            /* Loop for DATA / VARIANCE components */
            for( l=0; (*status==SAI__OK)&&(l<2); l++ ) {
              void *buf = data->pntr[l];

              if( buf ) {
                switch( data->dtype ) {
                case SMF__INTEGER:
                  for( k=0; k<ntslice; k++ ) {
                    ((int *)buf)[targetbolo*bstride + k*tstride] =
                      ((int *)buf)[sourcebolo*bstride + k*tstride];
                  }

                  if( i==(FIXDATA_S4A_MISSROW+1) ) {
                    for( k=0; k<ntslice; k++ ) {
                      ((int *)buf)[sourcebolo*bstride + k*tstride] = 0;
                    }
                  }
                  break;
     
                case SMF__USHORT:
                  for( k=0; k<ntslice; k++ ) {
                    ((unsigned short *)buf)[targetbolo*bstride + k*tstride] =
                      ((unsigned short *)buf)[sourcebolo*bstride + k*tstride];
                  }

                  if( i==(FIXDATA_S4A_MISSROW+1) ) {
                    for( k=0; k<ntslice; k++ ) {
                      ((unsigned short *)buf)[sourcebolo*bstride + k*tstride] =
                        0;
                    }
                  }

                  break;
           
                case SMF__DOUBLE:
                  for( k=0; k<ntslice; k++ ) {
                    ((double *)buf)[targetbolo*bstride + k*tstride] =
                      ((double *)buf)[sourcebolo*bstride + k*tstride];
                  }

                  if( i==(FIXDATA_S4A_MISSROW+1) ) {
                    for( k=0; k<ntslice; k++ ) {
                      ((double *)buf)[sourcebolo*bstride + k*tstride] =
                        0;
                    }
                  }

                  break;

                default:
                  *status = SAI__ERROR;
                  errRepf( "", FUNC_NAME
                           ": Don't know how to handle %s type.", status,
                           smf_dtype_str(data->dtype,status) );
                }
              }
            }

            /* Now do quality: Just set SMF__Q_BADDA for the dead
               row. Not sure if I should do something with sidecar
               quality as well? */
            if( qual ) {
              for( k=0; k<ntslice; k++ ) {
                qual[targetbolo*bstride + k*tstride] =
                  qual[sourcebolo*bstride + k*tstride];
              }
              
              if( i==(FIXDATA_S4A_MISSROW+1) ) {
                for( k=0; k<ntslice; k++ ) {
                  qual[sourcebolo*bstride + k*tstride] = SMF__Q_BADDA;
                }
              }
            }



          }
        }
      }

      /* If we get here and status is OK, set return value */
      if( *status == SAI__OK ) {
        have_fixed |= SMF__FIXED_ROWORDER;
      }
    }
  }

  /* Set FITS header so that we know smf_fix_data was run */
  smf_fits_updateL( hdr, SMFFIXDATA, have_fixed,
                    (have_fixed ? "Data have been fixed" : "Data do not require fixing"),
                    status );

  return have_fixed;
}
