/*
*+
*  Name:
*     SCANFIT

*  Purpose:
*     Top-level SCANFIT implementation

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
*     IN = NDF (Read)
*          Input file(s). Can include darks. If relevant darks are located
*          they will be subtracted from the non-dark observations.
*     ORDER = INTEGER (Read)
*          Order of the polynomial to fit. Default 1 (linear).
*     OUT = NDF (Write)
*          Dark subtracted and flatfielded output file for each input file.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-04-26 (AGG):
*        Initial test version, modified from simulator written by BDK.
*     2008-07-22 (TIMJ):
*        Use kaplibs and do dark subtraction.

*  Notes:

*  Copyright: 
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006 University of British Columbia. All Rights
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

  smfArray *darks = NULL;   /* Dark data */
  smfData *ffdata = NULL;    /* Pointer to output data struct */
  Grp *fgrp = NULL;          /* Filtered group, no darks */
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
  smf_find_darks( igrp, &fgrp, NULL, 1, SMF__NULL, &darks, status );

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

  /* Loop over all files */
  for (i=1; i<=size; i++) {
    /* Flatfield - if necessary */
    smf_open_and_flatfield( igrp, ogrp, i, darks, NULL, &ffdata, status );
    if (*status == SMF__FLATN) {
      errAnnul( status );
      msgOutif(MSG__VERB, "",
	     FUNC_NAME ": Data are already flatfielded", status);
    } else if ( *status == SAI__OK) {
      msgOutif(MSG__VERB," ","Flatfield applied", status);
    } else {
      /* Tell the user which file it was... */
      /* Would be user-friendly to trap 1st etc... */
      errFlush( status );
      msgSeti("I",i);
      msgSeti("N",size);
      errRep(FUNC_NAME, "Unable to flatfield data from file ^I of ^N", status);
    }

    smf_scanfit( ffdata, NULL, order, status );

    /* Free resources for output data */
    smf_close_file( &ffdata, status );
  }

  /* Tidy up after ourselves */
  if (darks) smf_close_related( &darks, status );
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);
  ndfEnd( status );
}
