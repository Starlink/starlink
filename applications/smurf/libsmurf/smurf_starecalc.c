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
*     mode are ignored.

*  Notes:
*     The averaging window, naver, should be calculated automatically.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Name of input data file
*     OUT = NDF (Write)
*          Name of output file containing STARE images

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-10-26 (AGG):
*        Clone from smurf_dreamsolve
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 University of British Columbia. All Rights
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
  size_t i;                        /* Loop counter */
  int flag;                        /* Grp flag */
  Grp *igrp = NULL;                /* Input files */
  Grp *ogrp = NULL;                /* Output files */
  size_t isize;                    /* Size of input Grp */
  int naver;                       /* Averaging window */
  size_t osize;                    /* Size of output Grp */
  smfData *data = NULL;            /* Input data */

  /* Main routine */
  ndfBegin();
  
  /* Get group of input NDFs */
  ndgAssoc( "IN", 1, &igrp, &isize, &flag, status);

  /* Propagate input NDFs to output */
  ndgCreat("OUT", igrp, &ogrp, &osize, &flag, status);

  /* Loop over number of files */
  for ( i=1; i<=isize; i++) {
    /* Open file and flatfield the data */
    smf_open_and_flatfield( igrp, ogrp, i, &data, status );

    naver = 200; /* Average every 1 second */

    smf_calc_stareimage( data, naver, status );

    /* Check status to see if there was a problem */
    if (*status != SAI__OK) {
      /* Tell the user which file it was... */
      msgSeti("I",i);
      msgSeti("N",isize);
      errRep(FUNC_NAME, 
	     "Unable to produce STARE images for data from file ^I of ^N", status);
    } else {
      msgSeti("N",naver);
      msgOutif(MSG__VERB," ", 
	       "STARE images written, averaged over ^N frames", status);
    }
    /* Free resources for output data */
    smf_close_file( &data, status );
  }

  /* Free up resources */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
