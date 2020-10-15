/*
*+
*  Name:
*     FLATFIELD

*  Purpose:
*     Flatfield SCUBA-2 data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_flatfield( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine flatfields SCUBA-2 data. Each
*     input file is propagated to a corresponding output file which is
*     identical but contains flatfielded data. If the input data are
*     already flatfielded, the user will be informed and no action
*     will be performed on the data.

*  Notes:
*     - Darks will be ignored.
*     - At the moment an output file is propagated regardless of
*     whether the input data are already flatfielded or not. This is
*     clearly a waste of disk space.

*  ADAM Parameters:
*     BBM = NDF (Read)
*          Group of files to be used as bad bolometer masks. Each data file
*          specified with the IN parameter will be masked. The corresponding
*          previous mask for a subarray will be used. If there is no previous
*          mask the closest following will be used. It is not an error for
*          no mask to match. A NULL parameter indicates no mask files to be
*          supplied. [!]
*     FLATMETH = _CHAR (Read)
*          Method to use to calculate the flatfield solution. Options
*          are POLYNOMIAL and TABLE. Polynomial fits a polynomial to
*          the measured signal. Table uses an interpolation scheme
*          between the measurements to determine the power. [POLYNOMIAL]
*     FLATORDER = _INTEGER (Read)
*          The order of polynomial to use when choosing POLYNOMIAL method.
*          [1]
*     FLATSNR = _DOUBLE (Read)
*          Signal-to-noise ratio threshold to use when filtering the
*          responsivity data to determine valid bolometers for the
*          flatfield. [3.0]
*     FLATUSENEXT = _LOGICAL (Read)
*          If true the previous and following flatfield will be used to
*          determine the overall flatfield to apply to a sequence. If false
*          only the previous flatfield will be used. A null default will
*          use both flatfields for data when we did not heater track
*          at the end, and will use a single flatfield when we did heater
*          track. The parameter value is not sticky and will revert to
*          the default unless explicitly over-ridden. [!]
*     IN = NDF (Read)
*          Input files to be uncompressed and flatfielded. Any darks provided
*          will be ignored.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          Output file(s).
*     OUTFILES = LITERAL (Write)
*          The name of text file to create, in which to put the names of
*          all the output NDFs created by this application (one per
*          line). If a NULL (!) value is supplied no file is created. [!]
*     RESIST = GROUP (Read)
*          A group expression containing the resistor settings for
*          each bolometer.  Usually specified as a text file using "^"
*          syntax. An example can be found in
*          $STARLINK_DIR/share/smurf/resist.cfg
*          [$STARLINK_DIR/share/smurf/resist.cfg]
*     RESPMASK = _LOGICAL (Read)
*          If true, responsivity data will be used to mask bolometer data
*          when calculating the flatfield. [TRUE]

*  Related Applications:
*     SMURF: CALCFLAT, RAWUNPRESS

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-11-04 (AGG):
*        Initial test version. Copy from smurf_extinction
*     2005-11-05 (AGG):
*        Factor out I/O code to smf_open_file
*     2005-11-07 (TIMJ):
*        Replace fits example code with call to smf_fits_getI
*     2005-11-28 (TIMJ):
*        Use smf_close_file
*     2005-12-12 (AGG):
*        Use smf_flatfield, remove need to include sc2da info, fix
*        loop counter bug
*     2005-12-14 (AGG/TIMJ):
*        Use smf_open_file on the output data. Note that
*        smf_close_file has been temporarily commented out until it is
*        also updated to take account of reference counting.
*     2006-01-24 (AGG):
*        Updated to use new smf_open_and_flatfield routine.
*     2006-01-27 (TIMJ):
*        Can now smf_close_file
*     2008-07-18 (TIMJ):
*        Use kaplibs routine to get in and out files so that the counts match
*        properly. Locate darks in input list.
*     2008-12-12 (TIMJ):
*        Fix reporting of whether or not the flatfield was applied.
*        Enable bad pixel masking.
*     2009-03-30 (TIMJ):
*        Add OUTFILES parameter.
*     2010-01-08 (AGG):
*        Change BPM to BBM.
*     2010-03-11 (TIMJ):
*        Support flatfield ramps.
*     2012-03-28 (TIMJ):
*        Ignore darks.
*     2013-08-21 (AGG):
*        Do not call grpList if no output files are generated. This
*        avoids a GRP__INVID error in such cases.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2010, 2012 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2006-2010,2013 University of British Columbia.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "par_err.h"

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smurf_flatfield"
#define TASK_NAME "FLATFIELD"

void smurf_flatfield( int *status ) {

  smfArray *bbms = NULL;    /* Bad bolometer masks */
  smfData *ffdata = NULL;   /* Pointer to output data struct */
  Grp *fgrp = NULL;         /* Filtered group, no darks */
  smfArray *flatramps = NULL;/* Flatfield ramps */
  AstKeyMap *heateffmap = NULL;    /* Heater efficiency data */
  size_t i = 0;             /* Counter, index */
  Grp *igrp = NULL;         /* Input group of files */
  Grp *ogrp = NULL;         /* Output group of files */
  size_t outsize;           /* Total number of NDF names in the output group */
  size_t size;              /* Number of files in input group */

  /* Main routine */
  ndfBegin();

  /* Get input file(s) */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Filter out darks */
  smf_find_science( NULL, igrp, &fgrp, 0, NULL, NULL, 1, 1, SMF__NULL, NULL,
                    &flatramps, &heateffmap, NULL, status );

  /* input group is now the filtered group so we can use that and
     free the old input group */
  size = grpGrpsz( fgrp, status );
  grpDelet( &igrp, status);
  igrp = fgrp;
  fgrp = NULL;

  if (size > 0) {
    /* Get output file(s) */
    kpg1Wgndf( "OUT", igrp, size, size, "More output files required...",
               &ogrp, &outsize, status );
  } else {
    msgOutif(MSG__NORM, " ","All supplied input frames were DARK,"
       " nothing to flatfield", status );
  }

  /* Get group of bolometer masks and read them into a smfArray */
  smf_request_mask( NULL, "BBM", &bbms, status );

  for (i=1; i<=size; i++ ) {
    int didflat;

    if (*status != SAI__OK) break;

    /* Call flatfield routine */
    didflat = smf_open_and_flatfield( NULL, igrp, ogrp, (int) i, NULL, flatramps,
                                      heateffmap, &ffdata, status);

    /* Report failure by adding a message indicating which file failed */
    msgSetk("I",i);
    if (*status != SAI__OK) {
      msgSetk("N",size);
      errRep(FUNC_NAME,	"Unable to flatfield data from file ^I of ^N", status);
      break;
    }

    /* in verbose mode report whether flatfielding occurred or not */
    if (!didflat) {
      msgOutif(MSG__VERB," ",
	     "Data from file ^I are already flatfielded", status);
    } else {
      msgOutif(MSG__VERB," ", "Flat field applied to file ^I", status);
    }

    /* Mask out bad bolometers - mask data array not quality array */
    smf_apply_mask( NULL, ffdata, bbms, SMF__BBM_DATA, 0, status );

    /* Free resources for output data */
    smf_close_file( NULL, &ffdata, status );
  }

  /* Write out the list of output NDF names, annulling the error if a null
     parameter value is supplied. */
  if( *status == SAI__OK && ogrp ) {
    grpList( "OUTFILES", 0, 0, NULL, ogrp, status );
    if( *status == PAR__NULL ) errAnnul( status );
  }

  /* Tidy up after ourselves: release the resources used by the grp routines  */
  if (igrp) grpDelet( &igrp, status);
  if (ogrp) grpDelet( &ogrp, status);
  if (bbms) smf_close_related( NULL, &bbms, status );
  if( flatramps ) smf_close_related( NULL, &flatramps, status );
  if (heateffmap) heateffmap = smf_free_effmap( heateffmap, status );
  ndfEnd( status );
}

