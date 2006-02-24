/*
*+
*  Name:
*     smurf_remsky

*  Purpose:
*     Top-level SKY removal implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_remsky( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine implementing the sky removal task.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input file(s)
*     METHOD = CHAR (Read)
*          Sky removal method, either POLY or PLANE
*     OUT = NDF (Write)
*          Output file(s)

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-02-16 (AGG):
*        Initial test version
*     {enter_further_changes_here}

*  Notes:
*     At the moment no check is made on whether the extinction
*     correction has already been applied.

*  Copyright:
*     Copyright (C) 2006 University of British Columbia. All Rights
*     Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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
#define FUNC_NAME "smurf_remsky"
#define TASK_NAME "REMSKY"
#define LEN__METHOD 20

void smurf_remsky( int * status ) {

  /* Local Variables */
  int flag;                  /* Flag for how group is terminated */
  int i;                     /* Loop counter */
  Grp *igrp = NULL;          /* Input group */
  smfData *odata = NULL;     /* Output data struct */
  Grp *ogrp = NULL;          /* Output group */
  int outsize;               /* Total number of NDF names in the output group */
  int size;                  /* Number of files in input group */
  int remsky = 0;

  char method[LEN__METHOD];  /* String for sky subtraction method */
  char fittype[LEN__METHOD]; /* String for PLANE method fit type */

  /* Main routine */
  ndfBegin();

  /* Read the input file */
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Get output file(s): assumes a 1:1 correspondence between input
     and output files */
  ndgCreat( "OUT", igrp, &ogrp, &outsize, &flag, status );

  /* Get METHOD */
  parChoic( "METHOD", "PLANE", "Plane, Polynomial", 1,  method, 
	    LEN__METHOD, status);

  for (i=1; i<=size; i++) {
    /* Flatfield - if necessary */
    smf_open_and_flatfield( igrp, ogrp, i, &odata, status );

    if (*status == SMF__FLATN) {
      errAnnul( status );
      msgOutif(MSG__VERB, "",
	     TASK_NAME ": Data are already flatfielded", status);
    } else if ( *status == SAI__OK) {
      msgOutif(MSG__VERB," ","Flatfield applied", status);
    } else {
      /* Tell the user which file it was... */
      /* Would be user-friendly to trap 1st etc... */
      msgSeti("I",i);
      errRep(TASK_NAME, "Unable to flatfield data from the ^I th file", status);
    }

    /* Check if REMSKY has already been run */
    /*    remsky = smf_history_check( odata, TASK_NAME, status );*/
    if ( remsky ) {
      if ( *status == SAI__OK) {
	msgOut(TASK_NAME, TASK_NAME " has already been run, skipping to next file", status);
      } else {
	*status = SAI__ERROR;
	errRep("", "Error in checking history. Possible programming error", status);
      }
    } else {

      if ( *status == SAI__OK ) {
	if ( strncmp( method, "POLY", 4 ) == 0 ) {
	  /* Bolometer-based sky removal */
	  smf_subtract_poly( odata, status );
	  /* Update history */
	  /*	  smf_history_write( odata, TASK_NAME, "Blether....", status );*/
	  /* Check status */
	} else if ( strncmp( method, "PLAN", 4 ) == 0 ) {
	  /* Timeslice-based sky removal */
	  parChoic( "FIT", "SLOPE", "Mean, Slope, Plane", 
		    1, fittype, LEN__METHOD, status);
	  smf_subtract_plane( odata, fittype, status );
	} else {
	  *status = SAI__ERROR;
	  msgSetc("M", method);
	  errRep("", "Unsupported method, ^M. Possible programming error.", status);
	}
      }
    }

  }

  /* Tidy up after ourselves: release the resources used by the grp routines  */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
