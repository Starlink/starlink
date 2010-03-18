/*
*+
*  Name:
*     smf_qualstats_report

*  Purpose:
*     Display a quality flagging statistics report for a smfArray

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:

*     smf_qualstats_report( const smfArray *qua, size_t last_qcount[8],
*                    size_t this_qcount[8], int init, int *status )

*  Arguments:
*     qua = const smfArray *qua (Given)
*        Pointer to smfArray of smfData's containing quality
*     last_qcount = size_t[8] (Given and Returned)
*        Pointer to array that countains number of occurences of each
*        quality bit in qual from the last call. Updated at the end. May
*        be set to NULL.
*     this_qcount = size_t[8] (Given and Returned)
*        Pointer to array that countains number of occurences of each
*        quality bit in current call.
*     init = int (Given)
*        If set, first call so initialize the qcount buffers.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     The smfArray will contain pointers to smfData's for each subarray.
*     If the main DATA components are SMF__UBYTE assume they contain
*     the quality array (e.g. QUA from the iterative map-maker). If the
*     main data array has a different type, check for the smfdata.pntr[2]
*     array. Count the total number of quality bits and display a report
*     on the number of flags, and the change from the previous call. If
*     last_qcount is NULL don't check for differences from the last call.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-03-16 (EC):
*        Initial Version

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
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_qualstats_report"

void smf_qualstats_report( const smfArray *qua,
                           size_t last_qcount[SMF__NQBITS],
                           size_t this_qcount[SMF__NQBITS], int init,
                           int *status ) {

  /* Local Variables */
  size_t bstride;               /* bolo stride */
  size_t i;                     /* loop counter */
  dim_t idx;                    /* Subarray counter */
  dim_t nbolo;                  /* number of bolos */
  size_t nbolo_tot;             /* total bolos in all subarrays */
  size_t ndata;                 /* total number of data points */
  dim_t ntslice;                /* number of time slices */
  dim_t ntslice_ref;            /* reference number of time slices */
  size_t qcount[8];             /* temporary quality bit counter */
  unsigned char *qual=NULL;     /* pointer to quality buffer */
  double steptime=0.005;        /* length of sample -- assume 200 Hz data */
  size_t tstride;               /* time slice stride */

  /* Main routine */
  if (*status != SAI__OK) return;

  if( !qua ) {
    *status = SAI__ERROR;
     errRep(" ", FUNC_NAME
            ": NULL qua pointer supplied.", status);
    return;
  }

  if( !this_qcount ) {
    *status = SAI__ERROR;
     errRep(" ", FUNC_NAME
            ": NULL this_qcount pointer supplied.", status);
    return;
  }

  if( init ) {
    /* Initialize last_qcount */
    memset( last_qcount, 0, SMF__NQBITS*sizeof(*last_qcount) );
  } else {
    /* Set last_qcount to this_qcount */
    memcpy(last_qcount, this_qcount, SMF__NQBITS*sizeof(*last_qcount));
  }

  /* Initialize this_qcount */
  memset( this_qcount, 0, SMF__NQBITS*sizeof(*this_qcount) );

  /* Loop over subarray */
  nbolo_tot = 0;
  ntslice_ref = 0;
  for( idx=0; (idx<qua->ndat)&&(*status==SAI__OK); idx++ ) {

    /* Get pointer to quality array and its dimensions */
    if( qua->sdata[idx]->dtype == SMF__UBYTE ) {
      qual = qua->sdata[idx]->pntr[0];
    } else {
      qual = qua->sdata[idx]->pntr[2];
    }

    if( !qual ) {
      *status = SAI__ERROR;
      errRep(" ", FUNC_NAME
             ": NULL qual pointer encountered", status);
    } else {
      smf_get_dims( qua->sdata[idx], NULL, NULL, &nbolo, &ntslice,
                    NULL, &bstride, &tstride, status );


      /* get quality statistics for the current subarray */
      smf_qualstats( qual, nbolo, bstride, ntslice, tstride, qcount, NULL,
                     status );

      /* add to total number of bolometers and check for length consistency */
      nbolo_tot += nbolo;
      if( !ntslice_ref ) {
        ntslice_ref = ntslice;
        if( qua->sdata[idx]->hdr ) {
          steptime = qua->sdata[idx]->hdr->steptime;
        }
      } else if( ntslice != ntslice_ref ) {
        *status = SAI__ERROR;
        errRep(" ", FUNC_NAME
               ": Different subarrays have mismatch in ntslice.", status);
      }

      if( *status == SAI__OK ) {
        /* Add quality counts from this subarray to the total */
        for( i=0; i<SMF__NQBITS; i++ ) {
          this_qcount[i] += qcount[i];
        }
      }

    }
  }

  /* Generate report */
  if( *status == SAI__OK ) {
    ndata = nbolo_tot*ntslice;

    if( init ) {
      msgOut( "", "--- Size of the entire data array ------------------------------------------",
              status );
      msgOutf("", "# bolometers : %zu", status, nbolo_tot );
      msgOutf("", "# time slices: %zu (%5.1lf min.)", status, ntslice,
              ntslice*steptime/60.);
      msgOutf("", "Total samples: %zu", status, ndata );
    }

    msgOutf("", "--- Quality flagging statistics --------------------------------------------",
            status );
    for( i=0; (i<SMF__NQBITS)&&(*status==SAI__OK); i++ ) {

      char scalestr[80];

      /* Report numbers of detectors or time slices if it makes sense for
         the given quality bit */
      switch( 1<<i ) {
      case SMF__Q_BADS: /* flatfield or DA flagged -- usually entire bolos */
        sprintf( scalestr, "%7zu bolos  ",
                 this_qcount[i] / ntslice );
        break;

      case SMF__Q_BADB: /* Entire bolos */
        sprintf( scalestr, "%7zu bolos  ",
                 this_qcount[i] / ntslice );
        break;

      case SMF__Q_PAD: /* Padding is for all bolos at given tslice */
        sprintf( scalestr, "%7zu tslices",
                 this_qcount[i] / nbolo_tot );
        break;

      case SMF__Q_APOD: /* Apodization is for all bolos at given tslice */
        sprintf( scalestr, "%7zu tslices",
                 this_qcount[i] / nbolo_tot );
        break;

      case SMF__Q_STAT: /* Stationary is for all bolos at given tslice */
        sprintf( scalestr, "%7zu tslices",
                 this_qcount[i] / nbolo_tot );
        break;

      default:
        sprintf( scalestr, "               " );
        break;
      }

      if( init ) {
        msgOutf("","%6s: %10zu (%5.2lf%%),%20s", status,
                smf_qual_str(i,status),
                this_qcount[i],
                100. * (double) this_qcount[i] / (double) ndata,
                scalestr);
      } else {
        msgOutf("","%6s: %10zu (%5.2lf%%),%20s,change %10li (%+6.2lf%%)",
                status,
                smf_qual_str(i,status),
                this_qcount[i],
                100. * (double) this_qcount[i] / (double) ndata,
                scalestr,
                (long) this_qcount[i] - (long) last_qcount[i],
                100. * ((double) this_qcount[i] - (double) last_qcount[i]) /
                (double) ndata );
      }
    }
  }

}
