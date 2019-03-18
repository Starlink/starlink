/*
*+
*  Name:
*     smf_correct_extinction

*  Purpose:
*     Low-level EXTINCTION correction function

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     int smf_correct_extinction(ThrWorkForce *wf, smfData *data, smf_tausrc *thetausrc,
*                             smf_extmeth method, AstKeyMap * extpars, double tau,
*                             double *allextcorr, double ** wvmtaucache, int *status);

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData* (Given)
*        smfData struct
*     thetausrc = smf_tausrc (Given)
*        Source of opacity value. On exit reflects the value that was used if a dynamic
*        option was given. Options are:
*          SMF__TAUSRC_AUTO: Use WVM if available and reliable, else use WVM or CSO fit.
*          SMF__TAUSRC_CSOTAU: Use the supplied "tau" argument as if it is CSO tau.
*          SMF__TAUSRC_CSOFIT: Use an external fit to the CSO data.
*          SMF__TAUSRC_WVMRAW: Use the WVM time series data.
*          SMF__TAUSRC_WVMFIT: Use an external fit to the WVM data.
*          SMF__TAUSRC_TAU: Use the tau value as if it is for this filter.
*     method = smf_extmeth (Given)
*        Flag to denote which method to use for calculating the airmass of a bolometer.
*          SMF__EXTMETH_SINGLE: Use single airmass for each time slice.
*          SMF__EXTMETH_FULL: Calculate airmass for each bolometer.
*          SMF__EXTMETH_ADAPT: Use FULL only if warranted. Else use FAST.
*     extpars = AstKeyMap * (Given)
*        AST keymap containing the parameters required to convert the
*        tau (on the CSO scale) to the current filter. Must contain
*        the "taurelation" key which itself will be a keymap
*        containing the parameters for the specific filter. Currently
*        only used if we are scaling from CSO or WVM.
*     tau = double (Given)
*        Optical depth at 225 GHz or filter wavelength. Only used if thetausrc is
*        AUTO, TAU or CSOTAU. If the bad value is used a default value will be
*        used from the header.
*     allextcorr = double* (Given and Returned)
*        If given, store calculated corrections for each bolo/time slice. Must
*        have same dimensions as bolos in *data
*     wvmtaucache = double ** (Given and Returned)
*        If *wvmtaucache is non-null the cache will assumed to contain the appropriately
*        smoothed WVM tau data, else WVM tau will be calculated by this routine.
*        If WVM tau was calculated this pointer is updated to point to the WVM tau data.
*        Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     Non-zero if the returned extinction corrections are the same for
*     every bolometer. Zero otherwize.

*  Description:
*     This is the main low-level routine implementing the EXTINCTION
*     task. If allextcorr is specified, no correction is applied to
*     the data (the correction factors are just stored).
*
*     The adaptive method looks at the combination of Airmass and Tau to determine
*     whether a full per-bolometer calculation is required or whether a single
*     airmass is sufficient (as read from the telescope tracking position).

*  Notes:
*     In WVM mode the adaptive mode checking occurs at each time slice. This is done because
*     tau can change from time slice to time slice, which could lead to a change
*     significant enough to cross the threshold. This is not important if a static
*     correction method (eg CSO tau) is in use. For CSOTAU and TAU modes the check is
*     performed once based on either the start or end airmass (since the tau is not changing).
*
*     If the wvm values are bad then the previous tau value is used. This should only occur
*     when the data acquisition has failed and many JCMTSTATE entries are missing.

*     To speed up calculation an external CSO tau scale array may be provided. For example
*     the tau calculated from the first subarray can be provided to the other 3 related
*     subarrays.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (TIMJ)
*     Ed Chapin (UBC))
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (AGG):
*        Initial test version
*     2005-11-14 (AGG):
*        Update API to accept a smfData struct
*     2005-12-20 (AGG):
*        Store the current coordinate system on entry and reset on return.
*     2005-12-21 (AGG):
*        Use the curslice as a frame offset into the timeseries data
*     2005-12-22 (AGG):
*        Deprecate use of curslice, make use of virtual flag instead
*     2006-01-11 (AGG):
*        API updated: tau no longer needed as it is retrieved from the
*        header. For now, image data uses the MEANWVM keyword from the
*        FITS header
*     2006-01-12 (AGG):
*        API update again: tau will be needed in the case the user
*        supplies it at the command line
*     2006-01-24 (AGG):
*        Floats to doubles
*     2006-01-25 (AGG):
*        Code factorization to simplify logic. Also corrects variance
*        if present.
*     2006-01-25 (TIMJ):
*        1-at-a-time astTran2 is not fast. Rewrite to do the astTran2
*        a frame at a time. Speed up from 85 seconds to 2 seconds.
*     2006-02-03 (AGG):
*        Add the quick flag. Not pretty but it gives a factor of 2 speed up
*     2006-02-07 (AGG):
*        Can now use the WVMRAW mode for getting the optical
*        depth. Also calculate the filter tau from the CSO tau in the
*        CSOTAU method
*     2006-02-17 (AGG):
*        Store and monitor all three WVM temperatures
*     2006-04-21 (AGG):
*        Check and update history if routine successful
*     2006-07-04 (AGG):
*        Update calls to slaAirmas to reflect new C interface
*     2006-07-26 (TIMJ):
*        sc2head no longer used. Use JCMTState instead.
*     2007-03-05 (EC):
*        Return allextcorr.
*        Check for existence of header.
*     2007-03-22 (AGG):
*        Check for incompatible combinations of data and parameters
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-04-03 (EC):
*        Assert time-ordered (ICD compliant) data
*     2008-04-29 (AGG):
*        Code reorg to ensure correct status handling
*     2008-04-30 (EC):
*        - Extra valid pointer checks
*        - Removed time-ordered data assertion. Handle bolo-ordered case.
*     2008-05-03 (EC):
*        - Only write history if applying extinction (!allextcorr case)
*     2008-06-12 (TIMJ):
*        - fix compiler warnings. origsystem must be copied in case
*        another call to astGetC is inserted in the code without thought.
*     2008-12-08 (TIMJ):
*        simplify smf_tslice_ast call logic
*        use sizeof(*var)
*     2008-12-09 (TIMJ):
*        - Use astTranGrid rather than astTran2
*        - Do not set system in frameset if it is already AZEL
*        - Do not bother resetting system on a frameset that is about to be
*          annulled.
*     2008-12-15 (TIMJ):
*        - use smf_get_dims. Tidy up isTordered handling and index handling.
*        - outline of adaptive mode.
*     2008-12-16 (TIMJ):
*        - new API using integer constants rather than strings.
*        - adaptive mode now working (inside and outside loop).
*     2009-11-04 (TIMJ):
*        Sometimes the DA drops TCS packets so we have to deal with
*        this by picking a recent airmass. Won't be completely accurate
*        but will be good enough for extinction correction of points that
*        will not be gridded into a map.
*     2010-01-18 (TIMJ):
*        Trap for bad WVM values and reuse the previous tau. The WVM C code
*        does not trap for this itself and so returns Inf.
*     2010-02-12 (TIMJ):
*        - Handle bad telescope data in "full" mode.
*        - Take control of our default cso tau for WVM mode and initialise
*          the "old" wvm array to bads rather than zero because the WVM
*          uses zero itself.
*     2010-02-16 (TIMJ):
*        Add auto mode to use WVM if available else CSO.
*     2010-06-03 (TIMJ):
*        Add extinction scaling parameters keymap.
*     2010-12-14 (TIMJ):
*        Trap negative taus from WVM. Currently just use the previous good
*        value.
*     2010-12-21 (TIMJ):
*        Trap 0 airmass in same manner as VAL__BADD airmass. Fall back to using
*        TCS_AZ_AC2.
*     2012-03-06 (TIMJ):
*        Use PAL instead of SLA.
*     2013-01-25 (DSB):
*        Change "corrthresh" from 0.01 to 0.02. This is to reduce the
*        number of cases where per-bolo EXT values are created, since
*        dumping the EXT model in such cases produces huge files (a
*        problem when running skyloop).
*     2013-06-21 (TIMJ):
*        Change AUTO mode so that it looks at WVM then CSO fit and then gives up.
*     2013-07-20 (TIMJ):
*        Update the tausrc argument so the caller knows which scheme was used.
*     2015-11-19 (GSB):
*        Try WVM fit before CSO fit in AUTO mode.
*     2019-03-18 (GSB):
*        Check new ngood_pre_despike parameter from smf_calc_smoothedwvm.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2015-2019 East Asian Observatory.
*     Copyright (C) 2008-2010, 2012, 2013 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2005-2008 University of British Columbia.
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

/* Standard includes */
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "star/pal.h"
#include "star/one.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"
#include "smf_err.h"

/* internal prototype */
static int is_large_delta_atau ( double airmass1, double elevation1, double tau, int *status);
static void smf1_correct_extinction( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfCorrectExtinctionData {
   dim_t f1;
   dim_t f2;
   dim_t nframes;
   dim_t npts;
   double *allextcorr;
   double *indata;
   double tau;
   double *vardata;
   double *wvmtau;
   double amstart;
   double amfirst;
   int *lbnd;
   int *ubnd;
   int isTordered;
   int ndims;
   int allquick;
   smfData *data;
   smfHead *hdr;
   smf_extmeth method;
   smf_tausrc tausrc;
} SmfCorrectExtinctionData;

/* A mutex used to serialise access to the smfData */
static pthread_mutex_t data_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Simple default string for errRep */
#define FUNC_NAME "smf_correct_extinction"

int smf_correct_extinction(ThrWorkForce *wf, smfData *data, smf_tausrc *thetausrc, smf_extmeth method,
                            AstKeyMap * extpars, double tau, double *allextcorr,
                            double **wvmtaucache, int *status) {

  /* Local variables */
  int allquick = 0;        /* Is the extinction for all bolometers the same? */
  double amstart = VAL__BADD; /* Airmass at start */
  double amend = VAL__BADD;   /* Airmass at end */
  double elstart = VAL__BADD; /* Elevation at start (radians) */
  double elend = VAL__BADD;/* Elevation at end (radians) */
  smfHead *hdr = NULL;     /* Pointer to full header struct */
  double *indata = NULL;   /* Pointer to data array */
  int isTordered;          /* data order of input data */
  int lbnd[2];             /* Lower bound */
  size_t ndims;            /* Number of dimensions in input data */
  dim_t nframes = 0;       /* Number of frames */
  dim_t npts = 0;          /* Number of data points */
  dim_t nx = 0;            /* # pixels in x-direction */
  dim_t ny = 0;            /* # pixels in y-direction */
  smf_tausrc tausrc;       /* Local copy of tausrc value */
  int ubnd[2];             /* Upper bound */
  double *vardata = NULL;  /* Pointer to variance array */
  double * wvmtau = NULL;  /* WVM tau (smoothed or not) for these data */
  int nw;                  /* Number of worker threads */
  int iw;                  /* Thread index */
  SmfCorrectExtinctionData *job_data = NULL;  /* Array of job descriptions */
  SmfCorrectExtinctionData *pdata;   /* Pointer to next job description */
  size_t framestep;         /* Number of frames per thread */

  /* Check status */
  if (*status != SAI__OK) return allquick;

  /* If no correction requested, return */
  if( method==SMF__EXTMETH_NONE ) {
    msgOutif(MSG__VERB, "", FUNC_NAME ": Extinction method=none, returning",
             status );
    return allquick;
  }

  if ( ! thetausrc ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Must supply a thetausrc argument. Possible programming error.",
            status );
    return allquick;
  }

  /* Use a local value for tausrc as we update it in this routine. In particular,
     CSOFIT becomes WVMRAW and this would be confusing to the caller */
  tausrc = *thetausrc;

  /* If no opacity monitor specified generate bad status */
  if( tausrc==SMF__TAUSRC_NULL ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": No source of opacity information could be determined",
            status );
    return allquick;
  }

  if( smf_history_check( data, FUNC_NAME, status) ) {
    /* If caller not requesting allextcorr fail here */
    if( !allextcorr ) {
      msgSetc("F", FUNC_NAME);
      msgOutif(MSG__VERB," ",
               "^F has already been run on these data, returning to caller",
               status);
      return allquick;
    }
  }

  /* Acquire the data order */
  isTordered = data->isTordered;

  /* make sure we have a header */
  hdr = data->hdr;
  if( hdr == NULL ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "Input data has no header", status);
    return allquick;
  }

  /* Do we have 2-D image data? */
  ndims = data->ndims;
  if (ndims == 2) {
    nframes = 1;
    nx = (data->dims)[0];
    ny = (data->dims)[1];
    npts = nx*ny;
  } else {
    /* this routine will also check for dimensionality */
    smf_get_dims( data, &nx, &ny, &npts, &nframes, NULL, NULL, NULL, status );
  }

  /* Tell user we're correcting for extinction */
  msgOutif(MSG__VERB," ",
           "Correcting for extinction.", status);

  /* Should check data type for double if not allextcorr case */
  if( !allextcorr ) {
    if (!smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status)) return allquick;
  }

  /* Check that we're not trying to use the WVM for 2-D data */
  if ( ndims == 2 && tausrc == SMF__TAUSRC_WVMRAW ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Method WVMRaw can not be used on 2-D image data", status );
      return allquick;
    }
  } else if (ndims == 2 && tausrc == SMF__TAUSRC_CSOFIT ) {
    /* This is CSOTAU mode with the value calculated from the fits. We have to either
       calculate the value here based on the FITS headers or we have to ensure that
       when this mode triggers we've been given the fallback tau derived in this manner.
       Revisit this as the code develops (we do not want to be reading fits multiple times).
    */
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Method CSOFIT not yet supported on 2-D image data", status );
      return allquick;
    }
  } else if (ndims == 2 && tausrc == SMF__TAUSRC_WVMFIT ) {
   if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Method WVMFIT not yet supported on 2-D image data", status );
      return allquick;
    }
  } else if (ndims < 2 || ndims > 3) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRepf( FUNC_NAME, "Can not extinction correct data with %zd dimension(s)", status,
              ndims );
      return allquick;
    }
  }

  /* if we are WVMRAW, WVMFIT, CSOFIT or AUTO and we have a cache we should always use it since
     we assume it was filled in properly the previous time. */
  if (wvmtaucache && *wvmtaucache &&
      (tausrc == SMF__TAUSRC_WVMRAW ||
       tausrc == SMF__TAUSRC_WVMFIT ||
       tausrc == SMF__TAUSRC_AUTO ||
       tausrc == SMF__TAUSRC_CSOFIT)) {
    wvmtau = *wvmtaucache;
    smf_smfFile_msg( data->file, "FILE", 1, "<unknown>");
    msgOutiff( MSG__VERB, "", "Using cached high resolution data for extinction correction of ^FILE",
               status);
    tausrc = SMF__TAUSRC_WVMRAW; /* We are now WVMRAW as we have the data */

    /* Assume that we only do not know the provenance if in AUTO mode */
    if (tausrc == SMF__TAUSRC_AUTO) *thetausrc = SMF__TAUSRC_CACHED;
  }

  if (!wvmtau && tausrc == SMF__TAUSRC_WVMRAW) {
    size_t ntotaltau = 0;
    size_t ngoodtau = 0;
    size_t ngood_pre_despike = 0;
    smf_calc_smoothedwvm( wf, NULL, data, extpars, &wvmtau, &ntotaltau,
                          &ngoodtau, &ngood_pre_despike, status );
    smf_smfFile_msg( data->file, "FILE", 1, "<unknown>");
    msgOutiff( MSG__VERB, "", "Using WVM mode for extinction correction of ^FILE"
               " %.0f %% of WVM data are present (%.0f %% before despiking)", status,
               (double)(100.0*(double)ngoodtau/(double)ntotaltau),
               (double)(100.0*(double)ngood_pre_despike/(double)ntotaltau) );
  }

  if (*status == SAI__OK &&
      (tausrc == SMF__TAUSRC_CSOFIT ||
       tausrc == SMF__TAUSRC_WVMFIT)) {
    /* Calculate the fit but we can use the same cache that WVM uses */
    dim_t nframes = 0;
    smf_calc_taufit( data, tausrc, extpars, &wvmtau, &nframes, status );
    smf_smfFile_msg( data->file, "FILE", 1, "<unknown>");
    msgSetc("FITSRC", (tausrc == SMF__TAUSRC_CSOFIT) ? "CSO" : "WVM");
    msgOutiff( MSG__QUIET, "", "Using ^FITSRC fits for extinction correction of ^FILE",
               status );
    /* Rebrand as WVM data from this point on */
    tausrc = SMF__TAUSRC_WVMRAW;
  }

  /* AUTO mode logic */
  /*
   * Default position is to use WVM data but we have two caveats
   *
   *  1. Was this observation done during a period where the WVM has been flagged as unreliable?
   *  2. If the WVM is nominally okay, do we have sufficient good data during this period?
   *
   * If the WVM should not be used we fallback to seeing if we have a fit available for this
   * night from the CSO data.
   *
   * If we do not have a reliable WVM or CSO fit then we fallback to using a fixed CSO number
   * from the header.
   *
   * This final fallback position is unfortunate as it is highly likely that this is not a reliable
   * number if we have fits for every night of observing (we have no information on whether a missing
   * fit indicates the CSO was too unstable to use or whether it means we simply haven't got to it
   * yet).
   *
   */

  /* Check auto mode */
  if (tausrc == SMF__TAUSRC_AUTO && *status == SAI__OK) {

    smf_smfFile_msg( data->file, "FILE", 1, "<unknown>" );

    if (ndims == 2) {
      /* have to use CSO mode */
      tausrc = SMF__TAUSRC_CSOTAU;
      *thetausrc = tausrc;
    } else if (ndims == 3) {
      /* We have already done the cache test so not needed here */
      double percentgood = 0.0;
      double percentgood_pre_despike = 0.0;
      double percentgood_thresh = 80.0;

      /* Is the WVM nominally stable for this night? */
      if (smf_is_wvm_usable( data->hdr, status ) ) {

        /* Calculate the WVM tau data and see if we have enough good data */
        size_t ngoodtau = 0;
        size_t ngood_pre_despike = 0;
        size_t ntotaltau = 0;
        smf_calc_smoothedwvm( wf, NULL, data, extpars, &wvmtau, &ntotaltau,
                              &ngoodtau, &ngood_pre_despike, status );
        percentgood = 100.0 * ((double)ngoodtau / (double)ntotaltau);
        percentgood_pre_despike = 100.0 * ((double) ngood_pre_despike / (double) ntotaltau);

        if ( percentgood > percentgood_thresh ) {
          tausrc = SMF__TAUSRC_WVMRAW;
          msgOutiff( MSG__VERB, "", "Selecting WVM mode for extinction correction of ^FILE."
                     " %.0f %% of WVM data are present (%.0f %% before despiking)",
                     status, percentgood, percentgood_pre_despike );
          *thetausrc = tausrc;
        } else {
          tausrc = SMF__TAUSRC_AUTO; /* keep it AUTO (a no-op but make it clear) */
          if (wvmtau) wvmtau = astFree( wvmtau );
        }
      }

      /* Next check for an available WVM fit. */
      if (tausrc == SMF__TAUSRC_AUTO && *status == SAI__OK) {
        dim_t nframes = 0;
        smf_calc_taufit( data, SMF__TAUSRC_WVMFIT, extpars, &wvmtau, &nframes, status );
        if (*status == SAI__OK) {
          smf_smfFile_msg( data->file, "FILE", 1, "<unknown>");
          msgOutiff( MSG__QUIET, "", "Using WVM fits for extinction correction of ^FILE",
                     status );
          /* Rebrand as WVM data from this point on */
          tausrc = SMF__TAUSRC_WVMRAW;
          *thetausrc = SMF__TAUSRC_WVMFIT;
        } else if (*status == SMF__BADFIT) {
          /* No fit, carry on. */
          errAnnul( status );
        }
      }

      /* at this point we either have WVM data handled or we still think we are AUTO.
         Do a CSO FIT check */
      if (tausrc == SMF__TAUSRC_AUTO && *status == SAI__OK) {
        dim_t nframes = 0;
        smf_calc_taufit( data, SMF__TAUSRC_CSOFIT, extpars, &wvmtau, &nframes, status );
        if (*status == SAI__OK) {
          smf_smfFile_msg( data->file, "FILE", 1, "<unknown>");
          msgOutiff( MSG__QUIET, "", "Using CSO fits for extinction correction of ^FILE",
                     status );
          /* Rebrand as WVM data from this point on */
          tausrc = SMF__TAUSRC_WVMRAW;
          *thetausrc = SMF__TAUSRC_CSOFIT;
        } else if (*status == SMF__BADFIT) {
          /* No fit, carry on. */
          errAnnul( status );
        }
      }

      /* At this point if we are not WVMRAW then we have a serious issue. It means that
         WVM was unusable and we did not have a good CSO fit. We should not continue at this
         point as to continue implies that we know what we should do. The user should decide
         how much they trust the opacity for the night. There has to be a reason why there
         is no CSO fit for the night. */
      if (*status == SAI__OK && tausrc != SMF__TAUSRC_WVMRAW) {
        *status = SAI__ERROR;
        msgFmt( "PCGFIN", "%.0f %%", percentgood );
        msgFmt( "PCGPDS", "%.0f %%", percentgood_pre_despike );

        /* Try to select a suggestion to help, for example, the operator
           running the summit pipeline, identify what might be wrong. */
        msgSetc( "SUGGN", (percentgood_pre_despike < 1.0) ? "WVM data missing or entirely unusable." :(
                          (percentgood_pre_despike <= percentgood_thresh) ? "WVM data may be insufficient." :(
                          (percentgood <= percentgood_thresh) ? "WVM data insufficient after despiking." :(
                          "WVM data unexpectedly rejected." ))));

        errRep("", "Unable to determine opacity data for this observation. "
               "^SUGGN ^PCGFIN of WMV data are present (^PCGPDS before despiking).",
               status );
      }
    }
  }

  /* If we have a CSO Tau then convert it to the current filter. This will also
     convert bad values to a value derived from the header if appropriate. */
  if ( tausrc == SMF__TAUSRC_CSOTAU ) {
    tau = smf_cso2filt_tau( hdr, tau, extpars, status );
    /* The tau source is now a real tau */
    tausrc = SMF__TAUSRC_TAU;
  }

  /* Find the airmass range for this data */
  smf_find_airmass_interval( hdr, &amstart, &amend, &elstart, &elend, status );
  if (*status == SAI__OK && (amstart == VAL__BADD || amend == VAL__BADD)) {
    *status = SAI__ERROR;
    errRep( "", "No good airmass values found in JCMTSTATE structure for these data",
            status );
  }

  /* if we are not doing WVM correction but are in adaptive mode we can determine
     whether or not we will have to use full or single mode just by looking at the
     airmass data. */
  if (ndims == 3 && tausrc != SMF__TAUSRC_WVMRAW && method == SMF__EXTMETH_ADAPT) {
    /* first and last is a good approximation given that most SCUBA-2 files will only
       be a minute duration. */
    double refel;
    double refam;

    /* only need to examine the largest airmass */
    if (amstart > amend) {
      refam = amstart;
      refel = elstart;
    } else {
      refam = amend;
      refel = elend;
    }

    /* and choose a correction method */
    if (is_large_delta_atau( refam, refel, tau, status) ) {
      method = SMF__EXTMETH_FULL;
      msgOutiff(MSG__DEBUG, " ",
               "Adaptive extinction algorithm selected per-bolometer airmass value "
               "per time slice (am=%g, tau=%g)", status, refam, tau);
    } else {
      msgOutiff(MSG__DEBUG, " ",
               "Adaptive extinction algorithm selected single airmass value per time slice"
               " (am=%g, tau=%g)", status, refam, tau);
      method = SMF__EXTMETH_SINGLE;
    }

  }

  /* Assign pointer to input data array if status is good */
  if ( *status == SAI__OK ) {
    indata = (data->pntr)[0];
    vardata = (data->pntr)[1];
  }

  /* Jump to the cleanup section if status is bad by this point
     since we need to free memory */
  if (*status != SAI__OK) goto CLEANUP;

  /* Array bounds for astTranGrid call */
  lbnd[0] = 1;
  lbnd[1] = 1;
  ubnd[0] = nx;
  ubnd[1] = ny;

  /* Unlock the AST objects in the smfData so that the worker threads can
     lock them. */
  smf_lock_data( data, 0, status );

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Find how many frames to process in each worker thread. */
  framestep = nframes/nw;
  if( framestep == 0 ) {
    framestep = 1;
    nw = nframes;
  }

  /* Allocate job data for threads, and store the range of frames to be
     processed by each one. Ensure that the last thread picks up any
     left-over frames. */
  job_data = astCalloc( nw, sizeof(*job_data) );
  if( *status == SAI__OK ) {
    for( iw = 0; iw < nw; iw++ ) {
      pdata = job_data + iw;
      pdata->f1 = iw*framestep;
      if( iw < nw - 1 ) {
        pdata->f2 = pdata->f1 + framestep - 1;
      } else {
        pdata->f2 = nframes - 1 ;
      }

      pdata->nframes = nframes;
      pdata->npts = npts;
      pdata->allextcorr = allextcorr;
      pdata->indata = indata;
      pdata->tau = tau;
      pdata->vardata = vardata;
      pdata->wvmtau = wvmtau;
      pdata->amstart = amstart;
      pdata->amfirst = amstart + ( amend - amstart )*pdata->f1/( nframes - 1 );
      pdata->lbnd = lbnd;
      pdata->ubnd = ubnd;
      pdata->isTordered = isTordered;
      pdata->ndims = ndims;
      pdata->data = data;
      pdata->hdr = hdr;
      pdata->method = method;
      pdata->tausrc = tausrc;

      /* Submit the job to the workforce. */
      thrAddJob( wf, 0, pdata, smf1_correct_extinction, 0, NULL, status );
    }

    /* Wait for all jobs to complete. */
    thrWait( wf, status );

    /* Record if all time slices used a single air mass. */
    allquick = 1;
    for( iw = 0; iw < nw; iw++ ) {
      pdata = job_data + iw;
      if( ! pdata->allquick ) {
        allquick = 0;
        break;
      }
    }

    /* Free the job data. */
    job_data = astFree( job_data );
  }

  /* Lock the AST objects in the smfData for use by this thread. */
  smf_lock_data( data, 1, status );

  /* Add history entry if !allextcorr */
  if( (*status == SAI__OK) && !allextcorr ) {
    smf_history_add( data, FUNC_NAME, status);
  }

 CLEANUP:
  if (wvmtaucache) {
    if (!*wvmtaucache) {
      *wvmtaucache = wvmtau;
    }
  } else {
    wvmtau = astFree( wvmtau );
  }

  return allquick;
}

/* Add footprint to the reference elevation and compare it to the reference airmass.
   Combine with tau and return true if some threshold is exceeded. */

static int is_large_delta_atau ( double airmass1, double elevation1, double tau,
                                 int *status) {
  double elevation2; /* elevation at edge of array */
  double airmass2;   /* airmass at edge of array */
  double delta;      /* difference between airmass1 and airmass2 times the tau  */
  const double footprint = 10.0 * DD2R / 60.0;  /* 10 arcmin */
  const double corrthresh = 0.02; /* threshold to switch from quick to slow as fraction */

  if (*status != SAI__OK) return 0;

  /* short circuit if tau is extremely small */
  if (tau < 0.000001) return 0;

  /* Bad telescope information */
  if (elevation1 == VAL__BADD) return 0;

  /* get the elevation and add on the array footprint. Calculate the airmass
     at the new location and find the difference to the reference airmass.
     The fractional error in exp(Atau) is (delta Airmass)*tau.
  */

  elevation2 = elevation1 - footprint;  /* want smaller el for larger am */
  airmass2 = palAirmas( M_PI_2 - elevation2 );
  delta = fabs(airmass1-airmass2) * tau;
  if (delta > corrthresh) {
    return 1;
  } else {
    return 0;
  }
}

















static void smf1_correct_extinction( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_correct_extinction

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_correct_extinction.

*  Invocation:
*     smf1_correct_extinction( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfCorrectExtinctionData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

  /* Local Variables: */
  SmfCorrectExtinctionData *pdata;
  double airmass;          /* Airmass */
  double state_airmass;    /* Airmass read from header */
  double state_az_ac2;     /* Elevation read from header */
  double amprev;           /* Previous airmass in loop */
  double *azel = NULL;     /* AZEL coordinates */
  size_t base;             /* Offset into 3d data array */
  double extcorr = 1.0;    /* Extinction correction factor */
  dim_t i;                 /* Loop counter */
  dim_t k;                 /* Loop counter */
  AstFrameSet *wcs = NULL; /* Pointer to AST WCS frameset */
  AstFrameSet *state_wcs = NULL; /* Pointer to copy of frameset from header */

  /* Check inherited status */
  if( *status != SAI__OK ) return;

  /* Get a pointer that can be used for accessing the required items in the
  supplied structure. */
  pdata = (SmfCorrectExtinctionData *) job_data_ptr;

  /* It is more efficient to call astTranGrid than astTran2
     Allocate memory in adaptive mode just in case. */
  if (pdata->method == SMF__EXTMETH_FULL ||
      pdata->method == SMF__EXTMETH_ADAPT ) {
    azel = astMalloc( (2*pdata->npts)*sizeof(*azel) );
  }

  amprev = pdata->amfirst;

  /* Assume we are using quick mode for all time slices. */
  pdata->allquick = 1;

  for ( k=pdata->f1; k<=pdata->f2 && (*status == SAI__OK) ; k++) {
    /* Flags to indicate which mode we are using for this time slice */
    int quick = 0;  /* use single airmass */
    int adaptive = 0; /* switch from quick to full if required */
    if (pdata->method == SMF__EXTMETH_SINGLE) {
      quick = 1;
    } else if (pdata->method == SMF__EXTMETH_ADAPT) {
      quick = 1;
      adaptive = 1;
    } else {
      pdata->allquick = 0;
    }

    /* Call tslice_ast to update the header for the particular
       timeslice. If we're in QUICK mode then we don't need the WCS. Use
       a mutex to prevent multiple threads writing to the header at the same
       time.  */
    thrMutexLock( &data_mutex, status );
    smf_lock_data( pdata->data, 1, status );
    smf_tslice_ast( pdata->data, k, !quick, NO_FTS, status );

    /* Copy the required bit of the header into thread-local storage. */
    if( *status == SAI__OK ) {
       state_airmass = pdata->hdr->state->tcs_airmass;
       state_az_ac2 = pdata->hdr->state->tcs_az_ac2;
       if( !quick && pdata->tau != VAL__BADD && pdata->hdr->wcs) state_wcs = astCopy( pdata->hdr->wcs );
    }

    /* Unlock the AST Objects in the smfData then unlock the local mutex. */
    smf_lock_data( pdata->data, 0, status );
    thrMutexUnlock( &data_mutex, status );

    /* Abort if an error has occurred. */
    if( *status != SAI__OK ) break;

    /* Read the WVM tau value if required */
    if (pdata->tausrc == SMF__TAUSRC_WVMRAW) {
      pdata->tau = pdata->wvmtau[k];
    }

    /* in all modes we need to keep track of the previous airmass in case
       we need to gap fill bad telescope data */
    if ( quick && pdata->ndims == 2 ) {
      /* for 2-D we use the FITS header directly */
      /* This may change depending on exact FITS keyword */
      airmass = pdata->amstart;

      /* speed is not an issue for a 2d image */
      adaptive = 0;

    } else {
      /* Else use airmass value in state structure */
      airmass = state_airmass;

      /* if things have gone bad use the previous value else store
         this value. We also need to switch to quick mode and disable adaptive. */
      if (airmass == VAL__BADD || airmass == 0.0 ) {
        if ( state_az_ac2 != VAL__BADD ) {
          /* try the elevation */
          airmass = palAirmas( M_PI_2 - state_az_ac2 );
        } else {
          airmass = amprev;
          quick = 1;
          adaptive = 0;
        }
      } else {
        amprev = airmass;
      }
    }

    /* If we're using the FAST application method, we assume a single
       airmass and tau for the whole array but we have to consider adaptive mode.
       If the tau is bad the extinction correction must also be bad. */
    if( pdata->tau == VAL__BADD) {
      extcorr = VAL__BADD;
    } else if (quick) {
      /* we have an airmass, see if we need to provide per-pixel correction */
      if (adaptive) {
        if (is_large_delta_atau( airmass, state_az_ac2, pdata->tau, status) ) {
          /* we need WCS if we disable fast mode */
          quick = 0;
          pdata->allquick = 0;

          thrMutexLock( &data_mutex, status );
          smf_lock_data( pdata->data, 1, status );
          smf_tslice_ast( pdata->data, k, 1, NO_FTS, status );
          state_airmass = pdata->hdr->state->tcs_airmass;
          state_az_ac2 = pdata->hdr->state->tcs_az_ac2;
          if (pdata->hdr->wcs) state_wcs = astCopy( pdata->hdr->wcs );
          smf_lock_data( pdata->data, 0, status );
          thrMutexUnlock( &data_mutex, status );

        }
      }

      if (quick) extcorr = exp(airmass*pdata->tau);
    }

    /* The previous test may have forced quick off so we can not combine
       the tests in one if-then-else block */
    if (!quick && pdata->tau != VAL__BADD )  {
      /* Not using quick so retrieve WCS to obtain elevation info */
      wcs = state_wcs;
      /* Check current frame, store it and then select the AZEL
         coordinate system */
      if (wcs != NULL) {
        if (strcmp(astGetC(wcs,"SYSTEM"), "AZEL") != 0) {
          astSet( wcs, "SYSTEM=AZEL"  );
        }
        /* Transfrom from pixels to AZEL */
        astTranGrid( wcs, 2, pdata->lbnd, pdata->ubnd, 0.1, 1000000, 1, 2,
                     pdata->npts, azel );
      } else {
        /* this time slice may have bad telescope data so we trap for this and re-enable
           "quick" with a default value. We'll only get here if airmass was good but
           SMU was bad so we use the good airmass. The map-maker won't be using this
           data but we need to use something plausible so that we do not throw off the FFTs */
        quick = 1;
        extcorr = exp(airmass*pdata->tau);
      }
    }
    /* Loop over data in time slice. Start counting at 1 since this is
       the GRID coordinate frame */
    base = pdata->npts * k;  /* Offset into 3d data array (time-ordered) */

    for (i=0; i < pdata->npts && ( *status == SAI__OK ); i++ ) {
      /* calculate array indices - assumes that astTranGrid fills up
         azel[] array in same order as bolometer data are aligned */
      size_t index;
      if ( pdata->isTordered ) {
        index = base + i;
      } else {
        index = k + (pdata->nframes * i);
      }

      if (!quick) {
        if (pdata->tau != VAL__BADD) {
          double zd;
          zd = M_PI_2 - azel[pdata->npts+i];
          airmass = palAirmas( zd );
          extcorr = exp(airmass*pdata->tau);
        } else {
          extcorr = VAL__BADD;
        }
      }

      if( pdata->allextcorr ) {
        /* Store extinction correction factor */
        pdata->allextcorr[index] = extcorr;
      } else {
        /* Otherwise Correct the data */
        if (extcorr != VAL__BADD) {
          if( pdata->indata && (pdata->indata[index] != VAL__BADD) ) {
            pdata->indata[index] *= extcorr;
          }

          /* Correct the variance */
          if( pdata->vardata && (pdata->vardata[index] != VAL__BADD) ) {
            pdata->vardata[index] *= extcorr * extcorr;
          }
        } else {
          if (pdata->indata) pdata->indata[index] = VAL__BADD;
          if (pdata->vardata) pdata->vardata[index] = VAL__BADD;
        }
      }

    }

    /* Note that we do not need to free "wcs" or revert its SYSTEM
       since smf_tslice_ast will replace the object immediately. */
  } /* End loop over timeslice */

  azel = astFree( azel );

}

