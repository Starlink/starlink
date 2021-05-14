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
*     o Works on ACSIS and SCUBA-2 data.  All other smfDatas cause the
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
*     2009-06-01 (EC):
*        Need to include strings.h for strncasecmp
*     2009-06-22 (TIMJ):
*        Check OCS Config before flipping sign of instrument aperture
*     2009-07-06 (TIMJ):
*        Add ability to look up information in table indexed by OBSID.
*     2009-10-28 (BRADC):
*        Check for missing FFT_WIN header.
*     2009-11-27 (TIMJ):
*        Call SCUBA-2 and ACSIS specialist fixup routines.
*     2014-03-28 (TIMJ):
*        Only run ACSIS and SCUBA-2 fixups with those specific instruments.
*     2014-04-11 (TIMJ):
*        Do not run STEPTIME guessing code unless we really are ACSIS or SCUBA-2.
*     2018-10-02 (DSB):
*        STEPTIME guess now ignores initial padding that has constant rts
*        values.
*     2019-03-19 (GSB):
*        Avoid reading beyond end of state array.  Correct sense of rts_end
*        comparison.
*     2020-2-24 (GSB):
*        Ensure the header structure always contains a steptime estimate
*        before calling smf_fix_metadata_scuba2.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     Copyright (C) 2009 University of British Columbia.
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

#define FUNC_NAME "smf_fix_metadata"

/* Indent for informational messages */
#define INDENT "   "

int smf_fix_metadata ( msglev_t msglev, smfData * data, int * status ) {

  AstFitsChan * fits = NULL; /* FITS header (FitsChan) */
  int have_fixed = 0;        /* Did we fix anything? */
  smfHead *hdr = NULL;       /* Data header struct */
  int ncards;                /* number of cards in FitsChan on entry */
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

  /* find out where the FITS header currently ends */
  ncards = astGetI( fits, "NCard" );

  /* Get the step time from the header if we have a hdr. These fixups are
     specifically for JCMT instruments which can access the OCS configuration XML.
     The fixups are not relevant for translated instrumentation which should be
     putting in a correct STEPTIME during translation.
  */
  if ( hdr->realinst == SMF__RINST_ACSIS ||
       hdr->realinst == SMF__RINST_SCUBA2 ) {
    /* for a handful of observations, the STEPTIME seems to be a clone
       of UEL header. These occur between 20080301 and 20090112
       (specifically 20080301#3, 20080316#55, 20090104#30, 20090111#10 and
       20090112#36 - the values on 20080726 seem to be okay).
       This is an example of a generalt problem (fixed on 20090112) where
       headers from a previous DRAMA get would arrive for the subsequent
       DRAMA get. So UEL -> STEPTIME or CHOP_FRQ -> STEPTIME. The general
       case of detecting shifted headers is tricky to code for since
       it needs some detailed analysis. In some cases the entire block
       of JOS headers (or END event CHOP_FRQ) are undef.
    */
    double uel = VAL__BADD;
    steptime = VAL__BADD;

    smf_getfitsd( hdr, "UEL", &uel, status );
    smf_getfitsd( hdr, "STEPTIME", &steptime, status );
    if (*status == SMF__NOKWRD || ( *status == SAI__OK &&
                                    ( steptime == VAL__BADD ||
                                      steptime < VAL__SMLD ||
                                      (uel != VAL__BADD && steptime == uel ) ) ) ) {
      if (*status != SAI__OK) errAnnul( status );
      steptime = VAL__BADD; /* reset to fixed state */

      /* First simply attempt to read it from the XML configuration file
         if it is available */
      if (hdr->ocsconfig) {
        int found;
        found = smf_pattern_extract( hdr->ocsconfig, "STEP_TIME=\"([0123456789\\.]+)\"", &steptime,
                                      NULL, 0, status );
        /* reset it on error */
        if (!found || steptime < VAL__SMLD) {
          steptime = VAL__BADD;
        } else {
          msgOutiff( msglev, "", INDENT "Determined step time from OCS configuration to be %g sec",
                     status, steptime);
        }
      }

      /* Attempt to calculate it from adjacent entries - it will not
         be correct but it might be close. The problem occurs if the
         state entries are derived from distinct sequences. Almost
         certainly to be the case for ACSIS in all cases except raster. */
      if (steptime == VAL__BADD) {
        if (hdr->nframes > 1 && tmpState[0].rts_end != VAL__BADD
            && tmpState[1].rts_end != VAL__BADD) {

          /* Skip any initial padding that will have a constant value for
             rts_num. */
          size_t islice = 1;
          while( islice < hdr->nframes-11 && tmpState[islice-1].rts_num >= tmpState[islice].rts_num ) {
             islice++;
          }

          steptime = tmpState[islice+10].rts_end - tmpState[islice].rts_end;
          steptime *= SPD;

          /* Correct for actual number of steps */
          if( tmpState[islice+10].rts_num > tmpState[islice].rts_num ){
             steptime /= (tmpState[islice+10].rts_num - tmpState[islice].rts_num );
             msgSetd("STP", steptime);
             msgOutif(MSG__VERB, " ", "WARNING: Determined step time to be ^STP"
                      " by examining state information", status );

          } else if( *status == SAI__OK ) {
             steptime = VAL__BADD;
             *status = SAI__ERROR;
             errRep( "", FUNC_NAME ": Unable to determine step time from header or "
                     " from state information", status );
          }

        } else if( *status == SAI__OK ){
          /* no idea - make this fatal for now */
          steptime = VAL__BADD;
          *status = SAI__ERROR;
          errRep( "", FUNC_NAME ": Unable to determine step time from header or "
                  " from state information", status );
        }
      }
      /* Update the FitsChan - the header should be present */
      if (steptime != VAL__BADD) {
        smf_fits_updateD( hdr, "STEPTIME", steptime, NULL, status );
        have_fixed |= SMF__FIXED_FITSHDR;
      }
    }
    if (*status == SAI__OK && steptime != VAL__BADD && steptime < VAL__SMLD) {
      *status = SAI__ERROR;
      msgSetd( "STP", steptime);
      errRep( "", FUNC_NAME ": Steptime must be positive but is ^STP sec. "
              "This can not happen", status);
      steptime = VAL__BADD;
    }

    /* Now we have a step time, store it in the header structure. */
    hdr->steptime = steptime;
  }

  /* Only do something for ACSIS and SCUBA-2 data. Not data matching the interface */
  if (hdr->realinst == SMF__RINST_ACSIS) {
    have_fixed = smf_fix_metadata_acsis( msglev, data, have_fixed, &ncards, status );
  } else if (hdr->realinst == SMF__RINST_SCUBA2) {
    have_fixed = smf_fix_metadata_scuba2( msglev, data, have_fixed, &ncards, status );
  }

  /* If we have fixed up the header, we need to record this by modifying the
     DHSVER header */
  if (have_fixed) {
    /* need to include date and consider not overwriting previous value */
    smf_fits_updateS( hdr, "DHSVER", "MOD", "Data Handling Version", status );
  }

  /* Get the number of cards in the header now. And if the header has grown we
     add a comment card indicating what the new block represent. This all assumes
     that we do not try to add new cards into their "proper" positions in the header
     when we fix it. */
  if (ncards < astGetI( fits, "NCard" ) ) {
    astSetI( fits, "Card", ncards+1 );
    astSetFitsCM( fits, "---- Metadata Fixups ----", 0 );
  }

  return have_fixed;
}
