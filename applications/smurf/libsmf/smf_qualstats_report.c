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
*     smf_qualstats_report( const smfArray *qua,
*                           size_t last_qcount[SMF__NQBITS],
*                           size_t *last_nmap,
*                           int init, size_t * ngood_tslice, int *status )

*  Arguments:
*     qua = const smfArray *qua (Given)
*        Pointer to smfArray of smfData's containing quality
*     last_qcount = size_t[8] (Given and Returned)
*        Pointer to array that countains number of occurences of each
*        quality bit in qual from the last call. Updated to current counts
*        upon return.
*     last_nmap = size_t* (Given and Returned)
*        Pointer to number of samples that would have gone into the map
*        last iteration. Updated to current number upon return.
*     init = int (Given)
*        If set, first call so initialize the qcount buffers.
*     ngood_tslice = size_t* (Returned)
*        If non-null, contains the number of usable time slices.
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

*  Copyright:
*     Copyright (C) 2010 University of British Columbia.
*     Copyright (C) 2010 Science and Technology Facilities Council.
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
                           size_t *last_nmap, int init,
                           size_t *ngood_tslice, int *status ) {

  /* Local Variables */
  size_t bstride;               /* bolo stride */
  size_t i;                     /* loop counter */
  dim_t idx;                    /* Subarray counter */
  dim_t nbolo;                  /* number of bolos */
  size_t nbolo_tot;             /* total bolos in all subarrays */
  size_t ndata;                 /* total number of data points */
  size_t ntgood;                /* Number of good time slices */
  size_t nmap;                  /* number of good map samples */
  size_t nmax;                  /* theoretical maximum good map samples */
  dim_t ntslice;                /* number of time slices */
  dim_t ntslice_ref;            /* reference number of time slices */
  size_t qcount[8];             /* total current quality bit counter */
  unsigned char *qual=NULL;     /* pointer to quality buffer */
  double steptime=0.005;        /* length of sample -- assume 200 Hz data */
  size_t subqcount[8];          /* subarray quality bit counter */
  size_t subnmap;               /* nmap for subarray */
  size_t subnmax;               /* nmax for subarray */
  size_t tbound;                /* time slices in boundary */
  size_t tpad;                  /* time slices in padding */
  size_t tstride;               /* time slice stride */
  size_t whichbit;              /* which bit is set */

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

  if( init ) {
    /* Initialize last_qcount */
    memset( last_qcount, 0, SMF__NQBITS*sizeof(*last_qcount) );

    /* Initialize last_namp */
    *last_nmap = 0;
  }

  /* Initialize counts */
  memset( qcount, 0, SMF__NQBITS*sizeof(*qcount) );
  nmap = 0;
  nmax = 0;

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
      smf_qualstats( qual, nbolo, bstride, ntslice, tstride, subqcount, NULL,
                     &subnmap, &subnmax, status );

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
        /* Add counts from this subarray to the total */
        for( i=0; i<SMF__NQBITS; i++ ) {
          qcount[i] += subqcount[i];
        }

        nmap += subnmap;
        nmax += subnmax;
      }

    }
  }

  /* Calculate time slices in boundary */
  tbound = ntslice-nmax/nbolo_tot;

  /* Calculate time slices in padding */
  whichbit = 0;
  while( !((1<<whichbit) & SMF__Q_PAD) ) { /* which bit is SMF__Q_PAD? */
    whichbit++;
  }
  tpad = qcount[whichbit] / nbolo_tot;

  /* Calculate the number of time slices that can be used */
  ntgood = ntslice - tbound;

  /* Generate report */
  if( *status == SAI__OK ) {
    ndata = nbolo_tot*ntslice;

    if( init ) {
      msgOut( "", "--- Size of the entire data array ------------------------------------------",
              status );
      msgOutf("", "bolos  : %zu", status, nbolo_tot );
      msgOutf("", "tslices: bnd:%zu(%.1lf min), map:%zu(%.1lf min), tot:%zu(%.1lf min)", status,
              ntslice-nmax/nbolo_tot, tbound*steptime/60.,
              nmax/nbolo_tot, ntgood*steptime/60.,
              ntslice, ntslice*steptime/60.);
      msgOutf("", "Total samples: %zu", status, ndata );
    }

    msgOutf("", "--- Quality flagging statistics --------------------------------------------",
            status );
    for( i=0; (i<SMF__NQBITS)&&(*status==SAI__OK); i++ ) {

      char scalestr[80];

      /* Report numbers of detectors or time slices if it makes sense for
         the given quality bit */
      switch( 1<<i ) {
      case SMF__Q_BADDA: /* flatfield or DA flagged -- bolo excluding padding*/
        sprintf( scalestr, "%7zu bolos  ",
                 qcount[i] / (ntslice-tpad) );
        break;

      case SMF__Q_BADB: /* Entire bolos */
        sprintf( scalestr, "%7zu bolos  ",
                 qcount[i] / ntslice );
        break;

      case SMF__Q_PAD: /* Padding is for all bolos at given tslice */
        sprintf( scalestr, "%7zu tslices",
                 qcount[i] / nbolo_tot );
        break;

      case SMF__Q_APOD: /* Apodization is for all bolos at given tslice */
        sprintf( scalestr, "%7zu tslices",
                 qcount[i] / nbolo_tot );
        break;

      case SMF__Q_STAT: /* Stationary is for all bolos at given tslice */
        sprintf( scalestr, "%7zu tslices",
                 qcount[i] / nbolo_tot );
        break;

      default:
        sprintf( scalestr, "               " );
        break;
      }

      if( init ) {
        msgOutf("","%6s: %10zu (%5.2lf%%),%20s", status,
                smf_qual_str(i,status),
                qcount[i],
                100. * (double) qcount[i] / (double) ndata,
                scalestr);
      } else {
        msgOutf("","%6s: %10zu (%5.2lf%%),%20s,change %10li (%+.2lf%%)",
                status,
                smf_qual_str(i,status),
                qcount[i],
                100. * (double) qcount[i] / (double) ndata,
                scalestr,
                (long) qcount[i] - (long) last_qcount[i],
                ((long) qcount[i] - (long) last_qcount[i]) ?
                100. * ((double) qcount[i] - (double) last_qcount[i]) /
                (double) last_qcount[i] : 0 );
      }
    }

    /* Total number of samples for the map */
    msgOutf("",
            "Total samples available for map: %10zu, %5.2lf%% of max (%g bolos)",
            status, nmap, 100. * (double) nmap / (double) nmax,
            (double)nmap / (double)ntgood );

    if( !init ) {
      msgOutf("",
              "     Change from last report: %10li, %+.2lf%% of previous",
              status, (long) nmap - (long) *last_nmap,
              ((long) nmap - (long) *last_nmap) ?
              100. * ((double) nmap - (double) *last_nmap) /
              (double) *last_nmap : 0 );
    }
  }

  if( *status == SAI__OK ) {
    /* Update last_qcount to qcount */
    memcpy(last_qcount, qcount, SMF__NQBITS*sizeof(*last_qcount));

    /* Update last_nmap to nmap */
    *last_nmap = nmap;

    if (ngood_tslice) *ngood_tslice = ntgood;

  }

}
