/*
*+
*  Name:
*     STARECALC

*  Purpose:
*     Top-level STARE image generator

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
*     This is the main routine for reconstructing 2-D images from
*     STARE observations. Files containing data not taken in STARE
*     mode are ignored. The user has the option to specify the number
*     of frames to be averaged together, but the default is to
*     automatically calculate that number to give 1-second averages.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Name of input data files
*     OUT = NDF (Write)
*          Name of output file containing STARE images
*     NAVER = _INTEGER (Read)
*          Number of frames to average together in output images. If a
*          NULL value is given, NAVER is calculated dynamically for
*          each input file to give output images which are 1-second
*          averages. [!]

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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006-2008 University of British Columbia. All Rights
*     Reserved.

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
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

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
  smfArray *darks = NULL;         /* Dark data */
  smfData *data = NULL;           /* Input data */
  Grp *fgrp = NULL;               /* Filtered group, no darks */
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
       " nothing to do", status );
  }

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
    smf_open_and_flatfield( igrp, ogrp, i, darks, &data, status );

    smf_calc_stareimage( data, naver, status );

    /* Check status to see if there was a problem */
    if (*status != SAI__OK) {
      /* Tell the user which file it was... */
      msgSeti("I",i);
      msgSeti("N",size);
      errRep(FUNC_NAME, 
	     "Unable to produce STARE images for data from file ^I of ^N", status);
    }
    /* Free resources for output data */
    smf_close_file( &data, status );
  }

  /* Free up resources */
  if (darks) smf_close_related( &darks, status );
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
