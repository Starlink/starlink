/*
*+
*  Name:
*     smf_fix_metadata_scuba2

*  Purpose:
*     Fix observation metadata for SCUBA2 data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_fix_metadata_scuba2( msglev_t msglev, smfData * data, int have_fixed,
*                                  int * ncards,  int * status );

*  Arguments:
*     msglev = msglev_t (Given)
*        Messaging level to be used for output information. This allows a
*        task whose entire job is to fix up meta data to report information
*        at default level whereas a task that simply wants the data fixed before
*        proceeding could use a debug level.
*     data = smfData * (Given)
*        smfData to be examined and fixed.
*     have_fixed = int (Given)
*        Current status of have_fixed. Modified value will be returned.
*     ncards = int * (Given & Returned)
*        Number of fits cards in the smfData on entry. Can be modified
*        if FITS headers are removed by this routine but should not be
*        modified if cards are added.
*     status = int * (Given & Returned)
*        Pointer to global status

*  Returned Value:
*     Returns int indicating whether the meta data were modified. 0 indicates
*     no modifications were made. Bits corresponding to the smf_metadata_fixups
*     enum will be used to indicate which parts of the meta data were modified.

*  Description:
*     Analyzes the smfData struct and determines whether meta data
*     should be modified. If necessary the FitsChan and JCMTSTATE components
*     will be updated. In some cases it is necessary to open the data file to
*     read information from the XML configuration content. If that XML is unavailable
*     some modifications will not be possible.
*
*     The disk file associated with the smfData will not be updated. The data
*     component is not required to be mapped.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     EC: Ed Chapin (UBC)
*     BRADC: Brad Cavanagh (JAC, Hawaii)
*     DSB: David Berry (JAC, Hawaii)

*  Notes:
*     o This function should not be called directly but should be
*     called solely from smf_fix_metadata().

*  History:
*     2009-11-27 (TIMJ):
*        Split from smf_fix_metadata
*     2010-02-24 (TIMJ):
*        Fix SHUTTER header for 20100223 fast flatfield ramps.
*     2010-05-07 (TIMJ):
*        Correct flatfield ramp heater values for pre-April 2010 data
*     2010-06-02 (TIMJ):
*        Calculate STEPTIME from the RTS_END values. The actual steptime
*        is slightly longer than the requested step time.
*     2010-10-09 (TIMJ):
*        Make sure we have two sequence steps when calculating
*        step time (eg s8d20100112_00055_0008)
*     2010-12-06 (TIMJ):
*        Fix shutter header so that it is always numeric.
*        Add SEQ_TYPE header to old 2009 data.
*        Fix RTS_NUM for 2009 data taken in the dark
*     2011-03-22 (TIMJ):
*        Do the RTS_NUM fix for more data since engineering data with a private
*        sequence has the same problem. Also check that the end value is zero.
*        Can not check for shutter state either.
*     2012-10-24 (DSB):
*        Set POL_CRD to FPLANE for all POL-2 data prior to some date in
*        the future that has not yet been decided.
*     2012-12-21 (DSB):
*        Ensure that the RTS_END values used to recalculate STEPTIME are
*        good.
*     2013-03-15 (TIMJ):
*        Recalculate start and end WVM taus using current algorithm.
*     2013-03-18 (DSB):
*        - Update the ISO times for the starting and ending WVM values.
*        - Ensure WVM headers are left unchanged without error if
*        airmass cannot be determined.
*     2013-07-31 (TIMJ):
*        Disable WVM recalc for FOCUS
*     2013-03-18 (DSB):
*        If airmass cannot be determined, only annull the error if it
*        is SAI__ERROR (i.e. do not hide errors from instra-structure
*        libraries that might indicate a programming problem).
*     2014-12-16 (DSB):
*        Do an explicit check to see if the supplied scan goes crazy,
*        rather than using a blacklist of known crazy scans.
*     2017-03-27 (DSB):
*        Modify smf__validate_scan to take account of any map offset
*        specified by the MAP_X and MAP_Y FITS headers.
*     2018-09-24 (DSB):
*        Re-calculate SCAN_VEL from JCMTSTATE info if it is undefined
*        in the FITS header.
*     2018-12-6 (DSB):
*        Clear bad status before setting new SCAN_VEL value.

*  Copyright:
*     Copyright (C) 2009-2014 Science & Technology Facilities Council.
*     Copyright (C) 2017-2018 East Asian Observatory.
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
#include "star/pal.h"

#include "smf.h"
#include "smf_err.h"

#include <stdio.h>
#include <strings.h>

/* Local struct containing header information that may be accessed multiple times
   without having to use many variables */

struct FitsHeaderStruct {
  int utdate;
  char instrume[81];
  char seq_type[81];
};

/* Local helper routines */
static void smf__calc_wvm_index( smfHead * hdr, const char * amhdr, size_t index, double *tau, double * time, int * status );
static int smf__validate_scan( smfHead *hdr, int *status );


#define FUNC_NAME "smf_fix_metadata_scuba2"

/* Indent for informational messages */
#define INDENT "   "

int smf_fix_metadata_scuba2 ( msglev_t msglev, smfData * data, int have_fixed, int *ncards, int * status ) {

  AstFitsChan * fits = NULL; /* FITS header (FitsChan) */
  struct FitsHeaderStruct fitsvals; /* Quick access Fits header struct */
  smfHead *hdr = NULL;       /* Data header struct */
  AstKeyMap * obsmap = NULL; /* Info from all observations */
  AstKeyMap * objmap = NULL; /* All the object names used */
  double scanvel;            /* Value of SCAN_VEL header */
  int validate_scans;        /* Should scan patterns be validated? */
  if (*status != SAI__OK) return have_fixed;

  /* Validate arguments - need smfFile and smfHead */
  smf_validate_smfData( data, 1, 1, status );
  if (*status != SAI__OK) return have_fixed;

  hdr = data->hdr;
  smf_validate_smfHead( hdr, 1, 1, status );
  if (*status != SAI__OK) return have_fixed;

  fits = hdr->fitshdr;

  if (hdr->instrument != INST__SCUBA2) {
    if (*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep("", " Attempting to fix metadata using SCUBA-2 algorithms but this is not SCUBA-2 data",
             status );
    }
    return have_fixed;
  }

  /* Update units string to something that is FITS standard compliant
     - we used "DAC units" for a while but in FITS land this becomes
     "decacoulomb * units" */
  if ( strncmp( hdr->units, "DAC", 3) == 0 ) {
    one_strlcpy( hdr->units, "adu", SMF__CHARLABEL, status );
  }

  /* Clock jitter and readout efficiencies mean we need to recalculate STEPTIME from the data.
     This is possible because we know that we have a continuous sequence in each file (unlike
     ACSIS). */
  if (hdr->allState) {
    /* it will be odd if it is not there */
    size_t nframes = hdr->nframes;

    size_t istart = 0;
    double start_time = (hdr->allState)[istart].rts_end;
    while( start_time == VAL__BADD && ++istart < nframes ) {
      start_time = (hdr->allState)[istart].rts_end;
    }

    size_t iend = nframes - 1;
    double end_time = (hdr->allState)[iend].rts_end;
    while( end_time == VAL__BADD && iend-- > 0 ) {
      end_time = (hdr->allState)[iend].rts_end;
    }

    double steptime = VAL__BADD;
    double newstep;

    smf_getfitsd( hdr, "STEPTIME", &steptime, status );
    newstep = steptime;

    /* it is possible for a file to contain only one step since
       the DA just dumps every N-steps. We can not recalculate the
       step time in that case. */
    nframes = iend - istart + 1;
    if (nframes > 1) {

      /* duration of file in days */
      newstep = end_time - start_time;

      /* convert to seconds */
      newstep *= SPD;

      /* Convert to step time */
      newstep /= (nframes - 1);
    } else if( nframes > 0 ) {
      /* work it out from RTS_END and TCS_TAI */
      JCMTState * onlystate = &((hdr->allState)[istart]);
      if ( onlystate->tcs_tai != VAL__BADD &&
           onlystate->tcs_tai != onlystate->rts_end) {
        /* TCS_TAI is in the middle of the step */
        newstep = 2.0 * ( onlystate->rts_end - onlystate->tcs_tai ) * SPD;
      }
    } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      if( data->file ) {
         smf_smfFile_msg( data->file, "N", 1, "<unknown>" );
         errRep("", "No valid RTS_END values found in NDF '^N'.", status );
      } else {
         errRep("", "No valid RTS_END values found.", status );
      }
    }

    if (steptime != newstep) {
      msgOutiff( msglev, "", INDENT "Recalculated step time as %g sec from JCMTSTATE (was %g sec)",
                 status, newstep, steptime);
      smf_fits_updateD( hdr, "STEPTIME", newstep, NULL, status );
      hdr->steptime = newstep;
      have_fixed |= SMF__FIXED_FITSHDR;
    }
  }

  /* Some observations do not have a value for SCAN_VEL (e.g. 20150918
    #24). In such cases, determine a SCAN_VEL value from the pointing info. */
  if( *status == SAI__OK ) {
    scanvel = VAL__BADD;
    smf_getfitsd( hdr, "SCAN_VEL", &scanvel, status );
    if( *status != SAI__OK || scanvel == VAL__BADD ) {
      if( *status != SAI__OK ) errAnnul( status );
      size_t nflagged;
      smf_flag_slewspeed( data, 0.0, 0.0, &nflagged, &scanvel, status );
      if( scanvel != VAL__BADD ) {
        msgOutiff( msglev, "", INDENT "Recalculated scan velocity as %g "
                   "arcsec/sec from JCMTSTATE (was undefined)", status,
                   scanvel );
        smf_fits_updateD( hdr, "SCAN_VEL", scanvel, NULL, status );
        hdr->scanvel = scanvel;
        have_fixed |= SMF__FIXED_FITSHDR;
      }
    }
  }

  /* Read some FITS headers, intialising the struct first */
  fitsvals.utdate = VAL__BADI;
  *(fitsvals.instrume) = '\0';
  smf_getfitsi( hdr, "UTDATE", &(fitsvals.utdate), status );
  smf_getfitss( hdr, "INSTRUME", fitsvals.instrume, sizeof(fitsvals.instrume), status );

  /* Print out summary of this observation - this may get repetitive if multiple files come
     from the same observation in one invocation but it seems better to describe each fix up
     separately and in context. */
  obsmap = astKeyMap( " " );
  objmap = astKeyMap( " " );
  smf_obsmap_fill( data, obsmap, objmap, status );
  smf_obsmap_report( msglev, obsmap, objmap, status );
  obsmap = astAnnul( obsmap );
  objmap = astAnnul( objmap );

  /* First we need to look for a BACKEND header which we do not write to the raw
     data files but CADC would like to see equal to INSTRUME */
  if (!astTestFits( fits, "BACKEND", NULL ) ) {
    have_fixed |= SMF__FIXED_FITSHDR;
    smf_fits_updateS( hdr, "BACKEND", fitsvals.instrume, "Name of the backend", status );
    msgOutif( msglev, "",  INDENT "Setting backend for SCUBA-2 observation.", status);
  }

  /* BASETEMP was reading MUXTEMP for pre-20091101 data */
  if ( fitsvals.utdate < 20091101 ) {
    double muxtemp = 0.0;
    have_fixed |= SMF__FIXED_FITSHDR;
    smf_getfitsd( hdr, "BASETEMP", &muxtemp, status );
    smf_fits_updateU( hdr, "BASETEMP", "[K] Base temperature", status );
    smf_fits_updateD( hdr, "MUXTEMP", muxtemp, "[K] Mux temperature", status );
    msgOutif( msglev, "", INDENT "Mux temperature is being read from BASETEMP header.", status );
  }

  /* Sometime before 20091119 the SHUTTER keyword was written as a string
     OPEN or CLOSED. Rewrite those as numbers */
  if (fitsvals.utdate < 20091119) {
    double shutval = 0.0;
    /* Try to read as a double. */
    smf_fits_getD( hdr, "SHUTTER", &shutval, status );

    /* Old data was a string. Convert to a double */
    if (*status == AST__FTCNV) {
      char shutter[100];
      errAnnul( status );
      smf_fits_getS( hdr, "SHUTTER", shutter, sizeof(shutter), status);
      if (strcmp(shutter, "CLOSED") == 0) {
        shutval = 0.0;
      } else {
        shutval = 1.0;
      }
      /* update the value */
      have_fixed |= SMF__FIXED_FITSHDR;
      smf_fits_updateD( hdr, "SHUTTER", shutval, "shutter position 0-Closed 1-Open", status );
      msgOutif( msglev, "", INDENT "Forcing SHUTTER header to be numeric", status );
    }
  }

  /* Engineering data with just SCUBA2 and no RTS left the RTS_NUM field
     filled with zeroes. Just assume that a zero in RTS_NUM is always
     indicative of a private sequence. */
  if (fitsvals.utdate < 20110401) {
    size_t nframes = hdr->nframes;
    JCMTState * curstate = &((hdr->allState)[0]);
    JCMTState * endstate = &((hdr->allState)[nframes-1]);
    if (curstate->rts_num == 0 && endstate->rts_num == 0) {
      /* have to set the values from the SEQSTART and SEQEND headers
         since those were set correctly (although any value would
         do of course apart from the sanity check in smf_find_science. */
      size_t i;
      int seqnum = 1;
      smf_fits_getI( hdr, "SEQSTART", &seqnum, status );
      for ( i=0; i<nframes; i++) {
        curstate = &((hdr->allState)[i]);
        curstate->rts_num = seqnum;
        seqnum++;
      }
      have_fixed |= SMF__FIXED_JCMTSTATE;
      msgOutif( msglev, "", INDENT "Private RTS sequence. Fixing RTS_NUM.", status );
    }
  }

  /* work out if this is a fast flat observation taken before May 2010 */
  if (fitsvals.utdate > 20100218 && fitsvals.utdate < 20100501) {
    char buff[100];
    /* need to know whether this is a FASTFLAT */
    smf_getfitss( hdr, "SEQ_TYPE", buff, sizeof(buff), status );

    if (strcmp( buff, "FASTFLAT" ) == 0 ) {

      /* Fast flats had incorrect SHUTTER settings for one night */
      if (fitsvals.utdate == 20100223) {
        have_fixed |= SMF__FIXED_FITSHDR;
        smf_fits_updateD( hdr, "SHUTTER", 1.0, "shutter position 0-Closed 1-Open", status );
        msgOutif( msglev, "", INDENT "Shutter was open for fast flatfield ramp. Correcting.", status );
      }

      /* Need to fix up SC2_HEAT ramps */
      /* the problem is that the data were assumed to be taken with 3 measurements
         in each heater setting. What actually happened was that the first 5 were
         done at the reference setting and then the data were grouped in threes
         finishing with a single value at the reference setting again.

         For example the heater values and the actual values look something like

           Stored    1 1 1 2 2 2 3 3 3 4 4 4 5 5 5
           Actual    0 0 0 0 0 2 2 2 3 3 3 4 4 4 5

         So we can correct for this by starting at the end and copying in the value
         two slots further down until we get to position #4. Then replacing that with
         the PIXHEAT number.
      */

      {
        size_t i;
        int pixheat = 0;
        size_t nframes = hdr->nframes;
        smf_getfitsi( hdr, "PIXHEAT", &pixheat, status );

        /* shift everything up by 2 */
        for (i=nframes-1; i > 4; i--) {
          JCMTState * curstate = &((hdr->allState)[i]);
          JCMTState * prevstate = &((hdr->allState)[i-2]);
          curstate->sc2_heat = prevstate->sc2_heat;
        }

        /* fill in the first 5 slots with the same value */
        for (i=0; i<5; i++) {
          JCMTState * curstate = &((hdr->allState)[i]);
          curstate->sc2_heat = pixheat;
        }
        have_fixed |= SMF__FIXED_JCMTSTATE;
      }
    }
  }

  /* We always recalculate the WVM start and end tau values so that the header
     reflects something approximating the value that was actually used in the
     extinction correction.

     Note that smf_calc_smoothedwvm can do a better job because it has multiple
     subarrays to get all the values from. We just have to try with what we
     have from a single subarray. We do step into the time series until we
     find something good.

     The header values should mostly agree with the recalculated values if the
     WVM code at the time matches the code in SMURF for that date. This has not
     been true in cases where we have retrospectively realised that there has been
     a calibration error in the WVM. So that we do not have to keep track of those
     times explicitly we currently recalculate every time. If this recalculation
     becomes a problem (smf_calc_wvm has a cache now to minimize this) it should
     be possible to disable this recalculation if the file is less than, say,
     30 minutes old to indicate we are running in near realtime.

     As a special case we do not recalculate the headers for FOCUS observations
     as the WVM reading is somewhat irrelevant and simply slows things down.

  */

  if( *status == SAI__OK ){

    /* Have not parsed header yet to extract type so do it explicitly here */
    char obstype[100];
    smf_getfitss( hdr, "OBS_TYPE", obstype, sizeof(obstype), status );

    if (strcasecmp( obstype, "focus") != 0) {

      size_t i;
      size_t nframes = hdr->nframes;
      double starttau = VAL__BADD;
      double starttime = VAL__BADD;
      double endtau = VAL__BADD;
      double endtime = VAL__BADD;

      /* Create a TimeFrame that can be used to format MJD values into ISO
         date-time strings, including a "T" separator between time and date. */
      AstTimeFrame *tf = astTimeFrame( "Format=iso.0T" );

      for (i=0; i < nframes && *status == SAI__OK; i++) {
        smf__calc_wvm_index( hdr, "AMSTART", i, &starttau, &starttime, status );
        if (starttau != VAL__BADD) break;
        if (*status == SAI__ERROR) errAnnul( status );
      }

      /* if we did not get a start tau we are not going to get an end tau */
      if (starttau != VAL__BADD) {
        for (i=0; i < nframes && *status == SAI__OK; i++) {
          smf__calc_wvm_index( hdr, "AMEND", nframes - 1 - i, &endtau, &endtime, status );
          if (endtau != VAL__BADD) break;
          if (*status == SAI__ERROR) errAnnul( status );
        }
      }

      /* If we could not find any WVM readings then we have a bit of a problem.
         Do we clear the FITS headers or do we leave them untouched? Leave them
         alone for now. */
      if (starttau != VAL__BADD && starttime != VAL__BADD) {
        smf_fits_updateD( hdr, "WVMTAUST", starttau, "186GHz Tau from JCMT WVM at start", status );

        /* Convert starttime MJD to ISO format and update the value in the
           FITS header. */
        smf_fits_updateS( hdr, "WVMDATST", astFormat( tf, 1, starttime ),
                          "Time of WVMTAUST", status );
        have_fixed |= SMF__FIXED_FITSHDR;
      }

      if (endtau != VAL__BADD && endtime != VAL__BADD) {
        smf_fits_updateD( hdr, "WVMTAUEN", endtau, "186GHz Tau from JCMT WVM at end", status );

        /* Convert endtime MJD to ISO format and update the value in the
           FITS header. */
        smf_fits_updateS( hdr, "WVMDATEN", astFormat( tf, 1, endtime ),
                          "Time of WVMTAUEN", status );
        have_fixed |= SMF__FIXED_FITSHDR;
      }

      /* Free the TimeFrame. */
      tf = astAnnul( tf );

    }
  }


  /* SEQ_TYPE header turned up in 20091125. Before that date the SEQ_TYPE only
     had two values. If the shutter was open then SEQ_TYPE is just OBS_TYPE. In the
     dark only a FLATFIELD sometimes finished with a noise but in that case CALCFLAT
     doesn't care so we just call it a flatfield sequence anyhow. We could look at
     the OBSEND flag but I'm not sure it makes a difference. */
  if ( fitsvals.utdate < 20091125 ) {
    char obstype[100];
    char seqtype[100];
    double shutval = 0.0;
    /* need to know what type of observation this is */
    smf_getfitss( hdr, "OBS_TYPE", obstype, sizeof(obstype), status );
    /* and the shutter status */
    smf_fits_getD( hdr, "SHUTTER", &shutval, status );

    if (shutval == 0.0 && strcasecmp( obstype, "flatfield" ) != 0 ) {
      /* flatfield was the only non-noise observation in the dark */
      one_strlcpy( seqtype, "NOISE", sizeof(seqtype), status );
      msgOutif( msglev, "", INDENT "Setting sequence type to NOISE", status );
    } else {
      /* Shutter was open so SEQ_TYPE is just OBS_TYPE */
      one_strlcpy( seqtype, obstype, sizeof(seqtype), status );
      msgOutif( msglev, "",  INDENT "Setting sequence type to obs type", status);
    }
    smf_fits_updateS( hdr, "SEQ_TYPE", seqtype, "Type of sequence", status );
    have_fixed |= SMF__FIXED_FITSHDR;
  }

  /* If the telescope goes crazy during the subscan (i.e. spends a significant
     amount of time outside the expected map area), null the telescope data
     for the subscan. This check can be suppressed by setting a zero
     value for config parameter VALIDATE_SCANS. This function does not
     have direct access to the config KeyMap, so it gets the VALIDATE_SCANS
     value from the smurf globals keymap. The top level makemap function
     copies the VALIDATE_SCANS value from the config keymap to the globals
     keymap. */
  validate_scans = smf_get_global0I( "VALIDATE_SCANS", 0, status );
  if ( validate_scans && !smf__validate_scan( hdr, status ) ) {
    size_t nframes = hdr->nframes;
    JCMTState * curstate;
    size_t i;
    for ( i=0; i<nframes; i++ ) {
      curstate = &((hdr->allState)[i]);
      curstate->jos_drcontrol |= DRCNTRL__PTCS_BIT;
    }
    msgOut( "", INDENT "WARNING: Rejecting subscan due to extreme excursion",
            status );
    have_fixed |= SMF__FIXED_JCMTSTATE;
  }

  /* The second half of observation 14 on 20111215 (scuba2_00014_20111215T061536)
     has a elevation pointing shift */
  if (fitsvals.utdate == 20111215) {
    char obsid[81];
    const char fitskey[] = "FIXPCORR";
    smf_getobsidss( hdr->fitshdr, obsid, sizeof(obsid), NULL, 0, status);

    if (strcmp(obsid, "scuba2_00014_20111215T061536") == 0 ) {
      int seqcount;
      smf_getfitsi( hdr, "SEQCOUNT", &seqcount, status );
      if (seqcount == 5) {
        int have_fixed_pntg = 0;
        smf_fits_getL( hdr, fitskey, &have_fixed_pntg, status );
        if (*status == SMF__NOKWRD) {
          have_fixed = 0;
          errAnnul( status );
        }
        if (!have_fixed_pntg) {
          size_t nframes = hdr->nframes;
          size_t i;
          const double dlon = 0.0;
          const double dlat = -16.83; /* From making maps of each half */
          /* Correct the pointing */
          msgOutif( msglev, "", INDENT "Applying pointing anomaly correction", status );
          for (i=0;i<nframes;i++) {
            JCMTState * curstate = &((hdr->allState)[i]);
            /* This is an AZEL correction */
            smf_add_smu_pcorr( curstate, 1, dlon, dlat, status );
          }
          smf_fits_updateL(hdr, fitskey, 1, "Applied internal pointing correction", status);
          have_fixed |= SMF__FIXED_JCMTSTATE;
        }
      }
    }

  }

  /* For POL-2 data prior to 18-JAN-2013, the POL_CRD header was always
     "FPLANE" in reality, even if the POL_CRD value in JCMTSTATE said
     something else. */
  if ( fitsvals.utdate < 20130118 ) {
    char polcrd[80] = "<unset>";
    smf_getfitss( hdr, "POL_CRD", polcrd, sizeof(polcrd), status );
    if (*status == SMF__NOKWRD) {
       errAnnul( status );
    } else if( !strcmp( polcrd, "TRACKING" ) || !strcmp( polcrd, "AZEL" ) ) {
      msgOutiff( msglev, "",  INDENT "Changing POL_CRD from %s to FPLANE", status, polcrd);
      smf_fits_updateS( hdr, "POL_CRD", "FPLANE",
                        "Coordinate system of polarimeter", status );
      have_fixed |= SMF__FIXED_FITSHDR;
    }
  }

  return have_fixed;
}


static void smf__calc_wvm_index( smfHead * hdr, const char * amhdr, size_t index, double *tau, double * time, int * status ) {
  double approxam = VAL__BADD;
  *tau = VAL__BADD;
  *time = VAL__BADD;

  if (*status != SAI__OK) return;

  smf_getfitsd( hdr, amhdr, &approxam, status );

  /* This saves us calling smf_tslice_ast as it is the only thing that needs to be set for smf_calc_wvm to run */
  hdr->state = &((hdr->allState)[index]);
  if (hdr->state->wvm_time != VAL__BADD) {
    double thistau = smf_calc_wvm( hdr, approxam, NULL, status );
    if (thistau != VAL__BADD) {
      *tau = thistau;
      *time = hdr->state->wvm_time;
    }
  }
  return;
}


static int smf__validate_scan( smfHead *hdr, int *status ) {

/* Local Variables: */
   JCMTState *state;
   dim_t iframe;
   dim_t maxbad;
   dim_t nbad;
   double lat0;
   double lat;
   double lon0;
   double lon;
   double map_hght;
   double map_wdth;
   double mapx;
   double mapy;
   double rad;
   double sep;
   int result = 1;

/* Check inherited status. */
   if( *status != SAI__OK ) return result;

/* Get the expected map dimensions, in arc-seconds. */
   map_hght = 0.0;
   smf_getfitsd( hdr, "MAP_HGHT", &map_hght, status );

   map_wdth = 0.0;
   smf_getfitsd( hdr, "MAP_WDTH", &map_wdth, status );

/* Get the requested map offset (in tracking coords) from the
   boresight, in arc-seconds. Convert to radians. */
   mapx = 0.0;
   smf_getfitsd( hdr, "MAP_X", &mapx, status );
   mapx *= AST__DD2R/3600.0;

   mapy = 0.0;
   smf_getfitsd( hdr, "MAP_Y", &mapy, status );
   mapy *= AST__DD2R/3600.0;

/* If the above keywords do not have usable values, we cannot do this
   check, so play safe and assume all is well. */
   if( map_hght > 0.0 && map_wdth > 0.0 ) {

/* Get the maximum expected distance of the boresight from the map
   centre, and convert from arc-seconds to radians. */
      rad = AST__DD2R*sqrt( map_hght*map_hght + map_hght*map_hght )/3600.0;

/* Set the maximum number of bad time slices that can be found before the
   whole scan is rejected as bad. This is 10% of the total number of frames. */
      maxbad = hdr->nframes/10;

/* Loop round all time slices. */
      nbad = 0;
      state = hdr->allState;
      for( iframe = 0; iframe < hdr->nframes; iframe++,state++ ) {

/* Check the boresight tracking values are good in the header. */
         lon = state->tcs_tr_ac1;
         lat = state->tcs_tr_ac2;
         lon0 = state->tcs_tr_bc1;
         lat0 = state->tcs_tr_bc2;
         if( lon != VAL__BADD && lat != VAL__BADD &&
             lon0 != VAL__BADD && lat0 != VAL__BADD ) {

/* Get the arc-distance from the boresight to the map centre at
   the current time slice. */
            sep = palDsep( lon, lat, lon0 + mapx, lat0 + mapy );

/* If it is greater than the expected radius of the map, increment the
   count of such time slices. If the number of bad slices exceeds the
   limit, there is no point in doing any more time slices so break out
   of the loop and return zero to indicate the scan pattern has gone
   crazy. */
            if( sep > rad ) {
               if( ++nbad > maxbad ) {
                  result = 0;
                  break;
               }
            }
         }
      }
   }

   return result;

}


