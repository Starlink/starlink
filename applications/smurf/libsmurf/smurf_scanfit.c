/*
*+
*  Name:
*     SCANFIT

*  Purpose:
*     Fit DC level to each bolometer

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_scanfit( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*    A list of NDF files containing scans is read. In turn each NDF is
*    opened, a sky fit determined for each bolometer, and the coefficients
*    of the fit added to the NDF.

*  ADAM Parameters:
*     BBM = NDF (Read)
*          Group of files to be used as bad bolometer masks. Each data file
*          specified with the IN parameter will be masked. The corresponding
*          previous mask for a subarray will be used. If there is no previous
*          mask the closest following will be used. It is not an error for
*          no mask to match. A NULL parameter indicates no mask files to be
*          supplied. [!]
*     IN = NDF (Read)
*          Input file(s). Can include darks. If relevant darks are located
*          they will be subtracted from the non-dark observations.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     ORDER = INTEGER (Read)
*          Order of the polynomial to fit. Default 1 (linear).
*     OUT = NDF (Write)
*          Dark subtracted and flatfielded output file for each input file.
*     OUTFILES = LITERAL (Write)
*          The name of text file to create, in which to put the names of
*          all the output NDFs created by this application (one per
*          line). If a null (!) value is supplied no file is created. [!]

*  Notes:
*     This routine should not be used for the iterative map-maker but can be
*     used for QLMAKEMAP and the simple rebinning map-maker.

*  Related Applications:
*     SMURF: QLMAKEMAP, MAKEMAP

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-04-26 (AGG):
*        Initial test version, modified from simulator written by BDK.
*     2008-07-22 (TIMJ):
*        Use kaplibs and do dark subtraction.
*     2008-12-12 (TIMJ):
*        Add bad pixel masking. Do not trap SMF__FLATN.
*     2009-03-30 (TIMJ):
*        Add OUTFILES parameter.
*     2010-01-08 (AGG):
*        Change BPM to BBM.
*     2010-03-11 (TIMJ):
*        Support flatfield ramps.
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2008, 2010 Science and Technology Facilities Council.
*     Copyright (C) 2006, 2010 University of British Columbia. All Rights
*     Reserved.

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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* Starlink includes */
#include "prm_par.h"
#include "sae_par.h"
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "par.h"
#include "star/ndg.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "smurflib.h"
#include "smurf_par.h"
#include "libsmf/smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smurf_scanfit"
#define TASK_NAME "SCANFIT"

void smurf_scanfit( int * status ) {

  smfArray *bbms = NULL;     /* Bad bolometer masks */
  smfArray *darks = NULL;    /* Dark data */
  smfData *ffdata = NULL;    /* Pointer to output data struct */
  Grp *fgrp = NULL;          /* Filtered group, no darks */
  smfArray *flatramps = NULL;/* Flatfield ramps */
  size_t i = 0;              /* Loop counter */
  Grp *igrp = NULL;          /* Input group */
  Grp *ogrp = NULL;          /* Input group */
  int order;                 /* Order of polynomial to fit */
  size_t outsize;            /* Total number of NDF names in the output group */
  size_t size;               /* Number of files in input group */

  /* Main routine */
  ndfBegin();

  /* Read the input file */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Filter out darks */
  smf_find_science( igrp, &fgrp, 0, NULL, NULL, 1, 1, SMF__NULL, &darks,
                    &flatramps, status );

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
       " nothing to do.", status );
  }

  /* Get order of polynomial */
  parGet0i( "ORDER", &order, status);

  /* Get group of bad bolometer masks and read them into a smfArray */
  smf_request_mask( "BBM", &bbms, status );

  /* Loop over all files */
  for (i=1; i<=size; i++) {
    if (*status != SAI__OK) break;

    /* Flatfield if necessary */
    smf_open_and_flatfield( igrp, ogrp, i, darks, flatramps, &ffdata, status );

    if (*status != SAI__OK) {
      msgSeti("I",i);
      msgSeti("N",size);
      errRep(FUNC_NAME, "Unable to flatfield data from file ^I of ^N", status);
    }

    /* Mask out bad bolometers - mask data array not quality array */
    smf_apply_mask( ffdata, NULL, bbms, SMF__BBM_DATA, status );

    smf_scanfit( ffdata, NULL, order, status );

    /* Free resources for output data */
    smf_close_file( &ffdata, status );
  }

  /* Write out the list of output NDF names, annulling the error if a null
     parameter value is supplied. */
  if( *status == SAI__OK ) {
    grpList( "OUTFILES", 0, 0, NULL, ogrp, status );
    if( *status == PAR__NULL ) errAnnul( status );
  }

  /* Tidy up after ourselves */
  if (darks) smf_close_related( &darks, status );
  if (bbms) smf_close_related( &bbms, status );
  if( flatramps ) smf_close_related( &flatramps, status );
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
