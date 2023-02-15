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
*     smf_qualstats_report( ThrWorkForce *wf, msglev_t msglev, smf_qfam_t qfamily, int nopad,
*                           const smfArray *qua, dim_t last_qcount[SMF__NQBITS],
*                           dim_t *last_nmap,
*                           int init, dim_t * ngood_tslice,
*                           dim_t *numdata, double *exptime, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pool of worker threads.
*     msglev = msglev_t (Given)
*        Messaging level for output.
*     qfamily = smf_qfam_t (Given)
*        Quality family associated with this quality array.
*     nopad = int (Given)
*        If true padding will not be included in report.
*     qua = const smfArray *qua (Given)
*        Pointer to smfArray of smfData's containing quality
*     last_qcount = dim_t[SMF__NQBITS] (Given and Returned)
*        Pointer to array that countains number of occurences of each
*        quality bit in qual from the last call. Updated to current counts
*        upon return. Will only use the number of elements determined by
*        the quality family.
*     last_nmap = dim_t* (Given and Returned)
*        Pointer to number of samples that would have gone into the map
*        last iteration. Updated to current number upon return.
*     init = int (Given)
*        If set, first call so initialize the qcount buffers.
*     ngood_tslice = dim_t* (Returned)
*        If non-null, contains the number of usable time slices.
*     numdata = dim_t* (Returned)
*        If non-null, the maximum possible number of bolometers
*     exptime = double * (Returned)
*        If non-null, the total exposure time represented by the non-pad
*        samples.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     The smfArray will contain pointers to smfData's for each subarray.
*     If the main DATA components are SMF__QUALTYPE assume they contain
*     the quality array (e.g. QUA from the iterative map-maker). If the
*     main data array has a different type, check for the smfdata.qual
*     array. Count the total number of quality bits and display a report
*     on the number of flags, and the change from the previous call. If
*     last_qcount is NULL don't check for differences from the last call.

*  Notes:
*     The effective number of bolometers in the map can be derived
*     using
*          nbolo_eff = *last_nmap / *ngood_tslice

*  Authors:
*     Edward Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-03-16 (EC):
*        Initial Version
*     2010-03-19 (EC):
*        Simplify interface, and keep track of total samples in map
*     2010-05-25 (TIMJ):
*        Convert "total samples available for map" into a number
*        of working bolometers.
*     2010-05-26 (TIMJ):
*        Return the number of usable time slices.
*     2010-05-27 (TIMJ):
*        Factor out calculation code into smf_qualstats_model
*     2010-06-16 (EC):
*        Return ndata.
*     2010-06-23 (TIMJ):
*        Add quality family support.
*     2010-07-14 (TIMJ):
*        Control message level of report
*     2010-07-16 (TIMJ):
*        Add ability to ignore padding.
*     2011-04-15 (TIMJ):
*        Add SMF__Q_EXT
*     2015-03-23 (TIMJ):
*        Do not report count of boundary slices if "nopad" is non-zero.
*     2016-01-13 (DSB):
*        Added argument exptime.

*  Copyright:
*     Copyright (C) 2010 University of British Columbia.
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

/* Prototypes for local functions. */
static const char *smf1_qual_str( smf_qfam_t family, int usebit, int bit_or_val,
                                  const char **descr, char *buf, int *status );

void smf_qualstats_report( ThrWorkForce *wf, msglev_t msglev, smf_qfam_t qfamily, int nopad,
                           const smfArray *qua, dim_t last_qcount[SMF__NQBITS],
                           dim_t *last_nmap, int init, dim_t *ngood_tslice,
                           dim_t *numdata, double *exptime, int *status ) {

  /* Local Variables */
  char buf[50];                /* Buffer for quality name */
  int i;                       /* loop counter */
  dim_t nbolo_tot;             /* total bolos in all subarrays */
  dim_t ndata;                 /* total number of data points */
  dim_t ntgood;                /* Number of good time slices */
  dim_t nmap;                  /* number of good map samples */
  dim_t nmax;                  /* theoretical maximum good map samples */
  dim_t nqbits = 0;            /* Number of quality bits in this family */
  dim_t ntslice;                /* number of time slices */
  dim_t qcount[SMF__NQBITS];   /* total current quality bit counter */
  double steptime=0.005;        /* length of sample -- assume 200 Hz data */
  dim_t tbound;                /* time slices in boundary */
  dim_t tpad;                  /* time slices in padding */

  /* Main routine */
  if (*status != SAI__OK) return;

  if( !qua ) {
    *status = SAI__ERROR;
     errRep(" ", FUNC_NAME
            ": NULL qua pointer supplied.", status);
    return;
  }

  if( !last_qcount ) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME
           ": NULL last_qcount pointer supplied.", status);
    return;
  }

  if( !last_nmap ) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME
           ": NULL last_nmap pointer supplied.", status);
    return;
  }

  nqbits = smf_qfamily_count( qfamily, status );

  if( init ) {
    /* Initialize last_qcount */
    memset( last_qcount, 0, nqbits*sizeof(*last_qcount) );

    /* Initialize last_namp */
    *last_nmap = 0;
  }

  /* Initialize counts */
  memset( qcount, 0, nqbits*sizeof(*qcount) );

  /* Find out the properties of the smfArray */
  smf_qualstats_model( wf, qfamily, nopad, qua, qcount, &nbolo_tot, &nmap, &nmax,
                       &ntslice, &ntgood, &tbound, &tpad, status );

  /* Get a more accurate steptime if we can */
  if (qua->sdata[0] && qua->sdata[0]->hdr) {
    steptime = qua->sdata[0]->hdr->steptime;
  }

  /* Total observation time. */
  if( exptime ) {
    if( nopad ) {
      *exptime = ntslice*steptime;
    } else {
      *exptime = ntgood*steptime;
    }
  }

  /* Generate report */
  if( *status == SAI__OK ) {
    ndata = nbolo_tot*ntslice;

    if( init ) {
      msgOutiff(msglev, "", "--- Size of the entire data array ------------------------------------------",
              status );
      msgOutiff(msglev, "", "bolos  : %zu", status, nbolo_tot );
      if( nopad ) {
         msgOutiff(msglev, "", "tslices: %"
                   DIM_T_FMT "(%.1lf min)", status, ntslice, ntslice*steptime/60.);
      } else {
         msgOutiff(msglev, "", "tslices: bnd:%zu(%.1lf min), map:%zu(%.1lf min), tot:%"
                   DIM_T_FMT "(%.1lf min)", status,
                   (dim_t)ntslice-(nmax/nbolo_tot), tbound*steptime/60.,
                   nmax/nbolo_tot, ntgood*steptime/60.,
                   ntslice, ntslice*steptime/60.);
      }
      msgOutiff(msglev, "", "Total samples: %zu", status, ndata );
    }

    msgOutiff(msglev, "", "--- Quality flagging statistics --------------------------------------------",
            status );
    for( i=0; (i<nqbits)&&(*status==SAI__OK); i++ ) {

      char scalestr[SZFITSTR];

      /* Do not report anything when there is nothing to say */
      if ( qcount[i] == 0 ) continue;

      /* Report numbers of detectors or time slices if it makes sense for
         the given quality bit */
      switch( BIT_TO_VAL(i) ) {
      case SMF__Q_BADDA: /* flatfield or DA flagged -- bolo excluding padding*/
        sprintf( scalestr, "%7zu bolos  ",
                 qcount[i] / ((dim_t)ntslice-tpad) );
        break;

      case SMF__Q_BADB: /* Entire bolos */
      case SMF__Q_NOISE: /* Noisy bolometers are entire bolos */
        sprintf( scalestr, "%7zu bolos  ",
                 qcount[i] / (dim_t)ntslice );
        break;

      case SMF__Q_PAD: /* Padding is for all bolos at given tslice */
      case SMF__Q_APOD: /* Apodization is for all bolos at given tslice */
      case SMF__Q_STAT: /* Stationary is for all bolos at given tslice */
      case SMF__Q_EXT: /* Extinction correction is usually for time slices */
        sprintf( scalestr, "%7zu tslices",
                 qcount[i] / nbolo_tot );
        break;

      default:
        sprintf( scalestr, "               " );
        break;
      }

      if( init ) {
        msgOutiff(msglev, "","%6s: %10zu (%5.2lf%%),%20s", status,
                  smf1_qual_str(qfamily, 1, i, NULL, buf, status),
                  qcount[i],
                  100. * (double) qcount[i] / (double) ndata,
                  scalestr);
      } else {
        msgOutiff(msglev, "","%6s: %10zu (%5.2lf%%),%20s,change %10li (%+.2lf%%)",
                  status,
                  smf1_qual_str(qfamily, 1, i, NULL, buf, status),
                  qcount[i],
                  100. * (double) qcount[i] / (double) ndata,
                  scalestr,
                  (long) qcount[i] - (long) last_qcount[i],
                  ((long) qcount[i] - (long) last_qcount[i] && last_qcount[i] != 0 ) ?
                  100. * ((double) qcount[i] - (double) last_qcount[i]) /
                  (double) last_qcount[i] : 0 );
      }
    }

    /* Total number of samples for the map */
    msgOutiff(msglev, "",
              "Total samples available for map: %10zu, %5.2lf%% of max (%g bolos)",
              status, nmap, 100. * (double) nmap / (double) nmax,
              (double)nmap / (double)ntgood );

    if( !init ) {
      msgOutiff(msglev, "",
                "     Change from last report: %10li, %+.2lf%% of previous",
                status, (long) nmap - (long) *last_nmap,
                ((long) nmap - (long) *last_nmap) ?
                100. * ((double) nmap - (double) *last_nmap) /
                (double) *last_nmap : 0 );
    }
  }

  if( *status == SAI__OK ) {
    /* Update last_qcount to qcount */
    memcpy(last_qcount, qcount, nqbits*sizeof(*last_qcount));

    /* Update last_nmap to nmap */
    *last_nmap = nmap;

    if (ngood_tslice) *ngood_tslice = ntgood;

    if (numdata) *numdata = ndata;
  }

}




/* A wrapper for smf_qual_str that removes the "_TS" suffix from any
   quality names. The returned pointer points to the supplied buffer. */

static const char *smf1_qual_str( smf_qfam_t family, int usebit, int bit_or_val,
                                  const char **descr, char *buf, int *status ) {
   const char *result = "";
   const char *basic = smf_qual_str( family, usebit, bit_or_val, descr, status );
   if( basic ) {
      strcpy( buf, basic );
      char *p = strstr( buf, "_TS" );
      if( p ) *p = 0;
      result = buf;
   }
   return result;
}


