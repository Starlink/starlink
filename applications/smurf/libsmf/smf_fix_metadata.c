/*
*+
*  Name:
*     smf_fix_metadata

*  Purpose:
*     Fix observation metadata

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_fix_metadata( msglev_t msglev, smfData * data, int * status );

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
*     no modifications were made.

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

*  Notes:
*     o Currently only examines ACSIS data. All other smfDatas cause the
*     function to return without comment.
*     o OFF exposure calculation for scan observations is an estimate
*     due to the difficulty of working out exactly where in the scan a
*     particular spectrum is from. Accurate determination requires all files
*     from a single observation.
*     o OFF exposure calculation for continuum jiggle observations will
*     be incorrect.

*  History:
*     2009-05-19 (TIMJ):
*        Initial version. Just processes ACS_EXPOSURE and ACS_OFFEXPOSURE
*     2009-05-25 (TIMJ):
*        Use new smf_obsmap_report API.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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

#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "prm_par.h"
#include "mers.h"

#include "smf.h"
#include "smf_err.h"

#include <stdio.h>

/* Local struct containing header information that may be accessed multiple times
   without having to use many variables */

struct FitsHeaderStruct {
  double obsgeox;
  double obsgeoy;
  double obsgeoz;
  int obsnum;
  int utdate;
  int jigl_cnt;
  int num_cyc;
  int jos_mult;
  int jos_min;
  int num_nods;
  int ncalstep;
  int nrefstep;
  int stbetref;
  int stbetcal;
  char focaxis[2];
  int nfocstep;
  int focstep;
  double instap_x;
  double instap_y;
};

#define FUNC_NAME "smf_fix_metadata"

/* Indent for informational messages */
#define INDENT "   "

int smf_fix_metadata ( msglev_t msglev, smfData * data, int * status ) {

  double dateobs;            /* MJD UTC of observation start */
  AstFitsChan * fits = NULL; /* FITS header (FitsChan) */
  struct FitsHeaderStruct fitsvals; /* Quick access Fits header struct */
  int has_dhsver = 0;        /* Do we have DHSVER header? */
  int have_fixed = 0;        /* Did we fix anything? */
  smfHead *hdr = NULL;       /* Data header struct */
  size_t i;
  int missing_exp = 0;       /* Are we missing ACS_EXPOSURE? */
  int missing_off = 0;       /* Are we missing ACS_OFFEXPOSURE? */
  AstKeyMap * obsmap = NULL; /* Info from all observations */
  AstKeyMap * objmap = NULL; /* All the object names used */
  double steptime = 0.0;     /* Step time */
  JCMTState * tmpState = NULL; /* Pointer to allState */

  if (*status != SAI__OK) return have_fixed;

  /* Validate arguments - need smfFile and smfHead */
  smf_validate_smfData( data, 1, 1, status );
  if (*status != SAI__OK) return have_fixed;

  hdr = data->hdr;
  smf_validate_smfHead( hdr, 1, 1, status );
  if (*status != SAI__OK) return have_fixed;

  fits = hdr->fitshdr;
  tmpState = hdr->allState;

  /* Get the step time from the header if we have a hdr */
  if ( hdr->instrument!=INST__NONE  ) {
    steptime = VAL__BADD;
    smf_getfitsd( hdr, "STEPTIME", &steptime, status );
    if (*status == SMF__NOKWRD || ( *status == SAI__OK && 
                                    steptime == VAL__BADD ) ) {
      if (*status != SAI__OK) errAnnul( status );
      /* Attempt to calculate it from adjacent entries - it will not
         be correct but it might be close. The problem occurs if the
         state entries are derived from distinct sequences. Almost
         certainly to be the case for ACSIS in all cases except raster. */
      if (hdr->nframes > 1 && tmpState[0].rts_end != VAL__BADD
          && tmpState[1].rts_end != VAL__BADD) {
        steptime = tmpState[1].rts_end - tmpState[0].rts_end;
        steptime *= SPD;
        /* Correct for actual number of steps */
        steptime /= (tmpState[1].rts_num - tmpState[0].rts_num ); 
        msgSetd("STP", steptime);
        msgOutif(MSG__QUIET, " ", "WARNING: Determined step time to be ^STP"
                 " by examining state information", status );

        /* Update the FitsChan - the header should be present */
        smf_fits_updateD( hdr, "STEPTIME", steptime, NULL, status );

      } else {
        /* no idea - make this fatal for now */
        steptime = VAL__BADD;
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME ": Unable to determine step time from header or "
                " from state information", status );
      }
    }
    if (*status == SAI__OK && steptime != VAL__BADD && steptime < VAL__SMLD) {
      *status = SAI__ERROR;
      msgSetd( "STP", steptime);
      errRep( "", FUNC_NAME ": Determined a negative steptime (^STP). "
              "This can not happen", status);
      steptime = VAL__BADD;
    }
  }

  /* Only do something for ACSIS data - SCUBA-2 data is currently "perfect" */
  if (hdr->instrument != INST__ACSIS) return have_fixed;

  /* Print out summary of this observation - this may get repetitive if multiple files come
     from the same observation in one invocation but it seems better to describe each fix up
     separately and in context. */
  obsmap = astKeyMap( " " );
  objmap = astKeyMap( " " );
  smf_obsmap_fill( data, obsmap, objmap, status );
  smf_obsmap_report( msglev, obsmap, objmap, status );
  obsmap = astAnnul( obsmap );
  objmap = astAnnul( objmap );

  /* Get the MJD of the observation. Does not need to be accurate so do not care whether
     it is from DATE-OBS or JCMTSTATE */
  smf_find_dateobs( hdr, &dateobs, NULL, status );

  /* Determine that we have a DHSVER header. This can be used to decide whether
     we have already fixed up this header. Some things can be determined from the values,
     but others are just fixups involving change of sign convention. */
  has_dhsver = astTestFits( fits, "DHSVER", NULL);

  /* Read some header values that are likely to be useful. Use a struct for the results
     to stop bloat of variable names. KeyMap or FitsChan are too cumbersone for multiple
     accesses.*/
  smf_getfitsi( hdr, "OBSNUM", &(fitsvals.obsnum), status );
  smf_getfitsi( hdr, "UTDATE", &(fitsvals.utdate), status );
  smf_getfitsi( hdr, "NUM_CYC", &(fitsvals.num_cyc), status );
  smf_getfitsi( hdr, "JOS_MULT", &(fitsvals.jos_mult), status );
  smf_getfitsi( hdr, "JOS_MIN", &(fitsvals.jos_min), status );
  smf_getfitsi( hdr, "NREFSTEP", &(fitsvals.nrefstep), status );
  smf_getfitsi( hdr, "JIGL_CNT", &(fitsvals.jigl_cnt), status );
  smf_getfitsd( hdr, "OBSGEO-X", &(fitsvals.obsgeox), status );
  smf_getfitsd( hdr, "OBSGEO-Y", &(fitsvals.obsgeoy), status );
  smf_getfitsd( hdr, "OBSGEO-Z", &(fitsvals.obsgeoz), status );
  smf_getfitsd( hdr, "INSTAP_X", &(fitsvals.instap_x), status );
  smf_getfitsd( hdr, "INSTAP_Y", &(fitsvals.instap_y), status );

  /* FITS header fix ups */

  /* LOFREQS and LOFREQE can come from FE_LOFREQ. HARP had incorrect header tables
     for quite a long period of time. */
  if (!astTestFits( fits, "LOFREQS", NULL ) ) {
    /* undef or missing makes no difference */
    smf_fits_updateD( hdr, "LOFREQS", tmpState[0].fe_lofreq, "[GHz] LO Frequency at start of obs.", status );
    have_fixed = 1;
  }
  if (!astTestFits( fits, "LOFREQE", NULL ) ) {
    /* undef or missing makes no difference */
    smf_fits_updateD( hdr, "LOFREQE", tmpState[hdr->nframes - 1].fe_lofreq,
                      "[GHz] LO Frequency at end of obs.", status );
    have_fixed = 1;
  }

  /*
   * Telescope position
   *  Before 20061013 observations had inaccurate telescope position.
   *  Needs to be replaced with the version used at the time by the telescope.
   *  Sometimes an old position was used for testing even after 20061013 since the coordinates
   *  were not obtained directly from the telescope.
   *  After 20080323 the telescope position was updated again but those files are all correct.
   *  We do not adopt the new position since we have to be consistent with the AZEL numbers
   *  stored in JCMTSTATE.
   */
  if (fitsvals.utdate < 20080323) {
    if ( fitsvals.obsgeox == -5464545.0 ) {
      /* hard-coded OBSGEO-X implies -Y and -Z also wrong */
      msgOutif( msglev, "", INDENT "Correcting telescope coordinates.", status );
      smf_fits_updateD( hdr, "OBSGEO-X", -5464594.335493, NULL, status );
      smf_fits_updateD( hdr, "OBSGEO-Y", -2492695.151639, NULL, status );
      smf_fits_updateD( hdr, "OBSGEO-Z", 2150964.058506, NULL, status );
      smf_fits_updateD( hdr, "LONG-OBS", 19.82583335521, NULL, status );
      smf_fits_updateD( hdr, "LAT-OBS", -155.4797222301, NULL, status );
      have_fixed = 1;
    }
  }

  /* Instrument aperture changed sign convention when we started reading it from the TCS.
     See JCMT fault 20090330.009 */
  if (!has_dhsver && fitsvals.utdate < 20080508
      && ( fitsvals.instap_x != 0.0 || fitsvals.instap_y != 0.0) ) {
    msgOutif( msglev, "", INDENT "Fixing instrument aperture sign convention.", status );
    smf_fits_updateD( hdr, "INSTAP_X", -1.0 * fitsvals.instap_x, NULL, status );
    smf_fits_updateD( hdr, "INSTAP_Y", -1.0 * fitsvals.instap_y, NULL, status );
    have_fixed = 1;
  }

  /* JCMTSTATE fix ups */

  /* TCS_TAI is missing before 20061013 */

  if (*status == SAI__OK) {
    /* TCS_TAI can be missing with old data files */
    if (tmpState[0].tcs_tai == VAL__BADD) {
      /* need the step time - if we do not have it set
       tcs_tai to rts_end */
      double step = steptime;
      if (step == VAL__BADD) {
        msgOutif(MSG__DEBUG," ","Could not determine step time when correcting TCS_TAI from RTS_END", status );
        step = 0.0;
      }

      /* correct TCS_TAI by half step time corrected to days */
      msgOutiff( msglev, "", INDENT "Missing TCS_TAI. Estimating using RTS_END and STEPTIME of %g sec.",
                 status, step);
      step = 0.5 * step / SPD; 
      if (*status == SAI__OK) {
        for (i=0; i < hdr->nframes; i++) {
          tmpState[i].tcs_tai = tmpState[i].rts_end - step;
        }
      }
      have_fixed = 1;
    }
  }

  /* Off exposure time - depends on observing mode.

     o ACS_EXPOSURE and ACS_NO_ONS pops up occassionally
     before 20070123 but should not be trusted.

        - jiggle/chop stores STEPTIME rather than actual
          exposure time.
        - ACS_NO_ONS is filled in for SCAN but reflects
          the number of steps in each scan.

     o Everything seems to be complete by 20070123.

     o Off exposure will be an estimate in some cases.
   */

  /* Assumes that STEPTIME is correct... */
  if ( (tmpState[0].acs_exposure == VAL__BADR) || (tmpState[0].acs_exposure < (0.90 * steptime)) ) {
    missing_exp = 1;
    msgOutif( msglev, "", INDENT "Missing ACS_EXPOSURE", status );
  }
  if ( (tmpState[0].acs_offexposure == VAL__BADR) || (tmpState[0].acs_offexposure < (0.90 * steptime)) ) {
    missing_off = 1;
    msgOutif( msglev, "", INDENT "Missing ACS_OFFEXPOSURE", status );
  }

  if ( (missing_exp || missing_off) && (*status == SAI__OK) ) {
    float exp_time = 0.0;
    float off_time = 0.0;
    int utmax = 20070130;

    if (fitsvals.utdate > utmax) {
      *status = SAI__ERROR;
      errRepf( "", INDENT "ACS_*EXPOSURE missing or BAD but data are newer than expected (%d > %d)",
               status, fitsvals.utdate, utmax);
    }

    /* if we are missing OFF exposure but on exposure seems to be present
       we do not trust it (See above). Some of the above logic becomes
       superfluous once we assume this since we are declaring that presence
       of OFFEXPOSURE controls everything. */
    if (missing_off && !missing_exp ) {
      msgOutiff( msglev, "", INDENT "ACS_EXPOSURE is present but untrustworthy", status );
      missing_exp = 1;
    }

    /* On exposure is assumed to be identical for all spectra */
    switch (hdr->obsmode) {
      case SMF__OBS_SCAN:
        /* On exposure is simply the step time */
        exp_time = steptime;

        if (hdr->swmode == SMF__SWM_PSSW) {
          /* off exposure time depends on the position of the spectrum
             in the row, and varies from 1 to 2 times the actual off time.
             This information is hard to obtain in the general case where a row
             is split between multiple files. The only reliable way to do it
             would be to run this command on multiple files at once and effectively
             timesort all the samples looking for the row boundaries. For now
             the important thing is to get an esimate, rather than a perfect value,
             so choose 1.5.

             Note that in more recent observations the JOS calculates NREFSTEP
             dynamically for each row.
          */
          off_time = 1.5 * fitsvals.nrefstep * steptime;
          if (missing_off) {
            msgOutiff( MSG__QUIET, "", "WARNING: %d #%d: OFF exposure time has been estimated as %g sec",
                       status, fitsvals.utdate, fitsvals.obsnum, off_time );
          }
        } else {
          *status = SAI__ERROR;
          errRep( "", "Unsupported scan switch mode.", status );
        }
        break;
      case SMF__OBS_GRID:
        /* depends on switch mode */
        if (hdr->swmode == SMF__SWM_CHOP) {
          /* this is really jiggle/chop (2.0 is for nod) */
          exp_time = 2.0 * fitsvals.jos_mult * steptime;
          off_time = exp_time;
        } else if (hdr->swmode == SMF__SWM_PSSW) {
          /* For older data we use JOS_MIN and coadd online. For newer data we actually write out
             each spectrum separately and coadd off line. See jos_dr_control for whether this has been
             done. We should not really be dealing with cases where the latter is being done and we
              are missing exposure time information so ignore for now. */
          exp_time = fitsvals.jos_min * steptime;
          off_time = exp_time;
        } else {
          *status = SAI__ERROR;
          errRep( "", "Unsupported grid switch mode.", status );
        }
        break;
      case SMF__OBS_JIGGLE:
        /* Since this is for old data, we assume that we are talking about jiggle/chop here
           but add an explicit trap. */
        if (hdr->swmode == SMF__SWM_CHOP) {
          /* 2.0 is for nod */
          exp_time = 2.0 * fitsvals.jos_mult * steptime;

          /* off time depends on how the pattern is broken up since it is actually
           done as 2 * N_CYC_OFF per N_JIGS_ON. In the vast majority of cases
          N_JIGS_ON = JIGL_CNT and N_CYC_OFF = ceil(sqrt(JIGL_CNT)/2)
          In all cases observed in 2006 and Jan 2007 this is true.

          In continuum mode on time and off time are the same.

          Unfortunately the only way to tell if continuum mode is enabled is to
          look at the XML. Assume that POINTING is always continuum.

          */
          if (hdr->obstype == SMF__TYP_POINTING) {
            off_time = exp_time;
          } else {
            /* First 2.0 is for Nodding. Second 2.0 is for spreading the off jiggle over both
               sides of the on */
            off_time = 2.0 * ceil(sqrt((double)fitsvals.jigl_cnt)/2.0) * 2.0 * steptime;
          }
        } else if (hdr->swmode == SMF__SWM_PSSW) {
          *status = SAI__ERROR;
          errRep( "", "Not expecting to have to deal with missing exposure time in jiggle/pssw mode", status);
        } else {
          *status = SAI__ERROR;
          errRep( "", "Unsupported jiggle switch mode.", status );
        }
        break;
      default:
        *status = SAI__ERROR;
        errRep( "", "Unsupported observing mode.", status );
    }
    msgOutiff( msglev, "", INDENT "Calculating ON exposure = %g sec and OFF exposure = %g sec", status, exp_time,
               off_time);

    /* fix up the state structure - assume all values identical */
    for (i = 0; i < hdr->nframes; i++) {
      if (missing_exp) tmpState[i].acs_exposure = exp_time;
      if (missing_off) tmpState[i].acs_offexposure = off_time;
    }
    have_fixed = 1;

  }

  /* If we have fixed up the header, we need to record this by modifying the
     DHSVER header */
  if (have_fixed) {
    /* need to include date and consider not overwriting previous value */
    smf_fits_updateS( hdr, "DHSVER", "MOD", "Data Handling Version", status );
  }

  return have_fixed;
}
