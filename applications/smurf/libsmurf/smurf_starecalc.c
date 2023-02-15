/*
*+
*  Name:
*     STARECALC

*  Purpose:
*     Calculate image for SCUBA-2 STARE observations

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_starecalc( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This command is used for reconstructing 2-D images from
*     STARE observations. Files containing data not taken in STARE
*     mode are ignored. The user has the option to specify the number
*     of frames to be averaged together, but the default is to
*     automatically calculate that number to give 1-second averages.

*  ADAM Parameters:
*     BBM = NDF (Read)
*          Group of files to be used as bad bolometer masks. Each data file
*          specified with the IN parameter will be masked. The corresponding
*          previous mask for a subarray will be used. If there is no previous
*          mask the closest following will be used. It is not an error for
*          no mask to match. A NULL parameter indicates no mask files to be
*          supplied. [!]
*     IN = NDF (Read)
*          Name of input data file(s).
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     NAVER = _INTEGER (Read)
*          Number of frames to average together in output images. If a
*          NULL value is given, NAVER is calculated dynamically for
*          each input file to give output images which are 1-second
*          averages. [!]
*     OUT = NDF (Write)
*          Name of output file containing STARE images.
*     OUTFILES = LITERAL (Write)
*          The name of a text file to create, in which to put the names of
*          all the output NDFs created by this application (one per
*          line). If a null (!) value is supplied no file is created. [!]

*  Related Applications:
*     SMURF: DREAMSOLVE;
*     KAPPA: WCSMOSAIC;
*     CCDPACK: MAKEMOS

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-10-26 (AGG):
*        Clone from smurf_dreamsolve
*     2008-07-22 (TIMJ):
*        Use kaplibs and support dark subtraction.
*     2008-11-13 (AGG):
*        Add NAVER as a parameter
*     2008-12-12 (TIMJ):
*        Add BPM parameter.
*     2009-03-30 (TIMJ):
*        Add OUTFILES parameter.
*     2013-08-21 (AGG):
*        Do not call grpList if no output files are generated. This
*        avoids a GRP__INVID error in such cases.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
*     Copyright (C) 2006-2008,2013 University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Standard incldues */
#include <string.h>
#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* SC2DA includes */
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2ast.h"
#include "sc2da/dream_par.h"

#define FUNC_NAME "smurf_starecalc"
#define TASK_NAME "STARECALC"

void smurf_starecalc ( int *status ) {

  /* Local Variables */
  smfArray *bbms = NULL;          /* Bad bolometer masks */
  smfArray *darks = NULL;         /* Dark data */
  smfData *data = NULL;           /* Input data */
  Grp *fgrp = NULL;               /* Filtered group, no darks */
  smfArray *flatramps = NULL;     /* Flatfield ramps */
  AstKeyMap *heateffmap = NULL;   /* Heater efficiency data */
  size_t i;                       /* Loop counter */
  Grp *igrp = NULL;               /* Input files */
  int naver;                      /* Averaging window, frames */
  Grp *ogrp = NULL;               /* Output files */
  size_t outsize;                 /* Size of output Grp */
  size_t size;                    /* Size of input Grp */

  /* Main routine */
  ndfBegin();

  /* Read the input file */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Filter out darks */
  smf_find_science( NULL, igrp, &fgrp, 0, NULL, NULL, 1, 0, SMF__NULL, &darks,
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
       " nothing to do", status );
  }

  /* Get group of bad bolometer masks and read them into a smfArray */
  smf_request_mask( NULL, "BBM", &bbms, status );

  /* Get number of frames to average over */
  if ( *status == SAI__OK ) {
    parGet0i( "NAVER", &naver, status );
    if ( *status == PAR__NULL ) {
      /* If the user has given us ! then we calculate the number of
	 frames to create 1-second images - smf_calc_stareimage knows
	 how to interpret a negative value */
      naver = -1;
      errAnnul( status );
      msgOutif(MSG__VERB, "", "Autocalculating NAVER to give 1-second images",
	       status);
    }
  }

  /* Loop over number of files */
  for ( i=1; i<=size; i++) {
    /* Open file and flatfield the data */
    smf_open_and_flatfield( NULL, igrp, ogrp, i, darks, flatramps,
                            heateffmap, &data, status );

    /* Mask out bad bolometers - mask data array not quality array */
    smf_apply_mask( NULL, data, bbms, SMF__BBM_DATA, 0, status );

    smf_calc_stareimage( data, naver, status );

    /* Check status to see if there was a problem */
    if (*status != SAI__OK) {
      /* Tell the user which file it was... */
      msgSetk("I",i);
      msgSetk("N",size);
      errRep(FUNC_NAME,
	     "Unable to produce STARE images for data from file ^I of ^N", status);
    }
    /* Free resources for output data */
    smf_close_file( NULL, &data, status );
  }

  /* Write out the list of output NDF names, annulling the error if a null
     parameter value is supplied. */
  if( *status == SAI__OK && ogrp ) {
    grpList( "OUTFILES", 0, 0, NULL, ogrp, status );
    if( *status == PAR__NULL ) errAnnul( status );
  }

  /* Free up resources */
  if (darks) smf_close_related( NULL, &darks, status );
  if (bbms) smf_close_related( NULL, &bbms, status );
  if (flatramps) smf_close_related( NULL, &flatramps, status );
  if (heateffmap) heateffmap = smf_free_effmap( heateffmap, status );
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
