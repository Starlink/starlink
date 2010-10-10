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

*  Copyright:
*     Copyright (C) 2009-2010 Science & Technology Facilities Council.
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
#include "star/one.h"

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


#define FUNC_NAME "smf_fix_metadata_scuba2"

/* Indent for informational messages */
#define INDENT "   "

int smf_fix_metadata_scuba2 ( msglev_t msglev, smfData * data, int have_fixed, int *ncards, int * status ) {

  AstFitsChan * fits = NULL; /* FITS header (FitsChan) */
  struct FitsHeaderStruct fitsvals; /* Quick access Fits header struct */
  smfHead *hdr = NULL;       /* Data header struct */
  AstKeyMap * obsmap = NULL; /* Info from all observations */
  AstKeyMap * objmap = NULL; /* All the object names used */

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

  /* Clock jitter and readout efficiencies mean we need to recalculate STEPTIME from the data.
     This is possible because we know that we have a continuous sequence in each file (unlike
     ACSIS). */
  if (hdr->allState) {
    /* it will be odd if it is not there */
    size_t nframes = hdr->nframes;
    double start_time = (hdr->allState)[0].rts_end;
    double end_time = (hdr->allState)[nframes-1].rts_end;
    double steptime = VAL__BADD;
    double newstep;

    smf_getfitsd( hdr, "STEPTIME", &steptime, status );
    newstep = steptime;

    /* it is possible for a file to contain only one step since
       the DA just dumps every N-steps. We can not recalculate the
       step time in that case. */
    if (nframes > 1) {

      /* duration of file in days */
      newstep = end_time - start_time;

      /* convert to seconds */
      newstep *= SPD;

      /* Convert to step time */
      newstep /= (nframes - 1);
    } else {
      /* work it out from RTS_END and TCS_TAI */
      JCMTState * onlystate = &((hdr->allState)[0]);
      if ( onlystate->tcs_tai != VAL__BADD &&
           onlystate->tcs_tai != onlystate->rts_end) {
        /* TCS_TAI is in the middle of the step */
        newstep = 2.0 * ( onlystate->rts_end - onlystate->tcs_tai ) * SPD;
      }
    }

    if (steptime != newstep) {
      msgOutiff( msglev, "", INDENT "Recalculated step time as %g sec from JCMTSTATE (was %g sec)",
                 status, newstep, steptime);
      smf_fits_updateD( hdr, "STEPTIME", newstep, NULL, status );
      have_fixed |= SMF__FIXED_FITSHDR;
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

  /* SEQ_TYPE header turned up in 200911xx but we do not need to fix that by copying
     OBS_TYPE since SMURF already handles it. If we want to fill in this header then
     we should copy the OBS_TYPE value except when the SHUTTER is closed and we are
     not doing a NOISE or FLATFIELD observation. Set it to "noise" in those cases.
     Some dark flatfields had an extra dark at the end so set the OBSEND=T dark
     to a "noise" in that case */

  return have_fixed;
}
