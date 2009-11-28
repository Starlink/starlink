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

  /* Read some FITS headers, intialising the struct first */
  fitsvals.utdate = VAL__BADI;
  strcpy( fitsvals.instrume, "" );
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

  /* SEQ_TYPE header turned up in 200911xx but we do not need to fix that by copying
     OBS_TYPE since SMURF already handles it. If we want to fill in this header then
     we should copy the OBS_TYPE value except when the SHUTTER is closed and we are
     not doing a NOISE or FLATFIELD observation. Set it to "noise" in those cases.
     Some dark flatfields had an extra dark at the end so set the OBSEND=T dark
     to a "noise" in that case */

  return have_fixed;
}
