/*
*+
*  Name:
*     smurf_extinction

*  Purpose:
*     Top-level EXTINCTION implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_extinction( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine implementing the EXTINCTION task.

*  ADAM Parameters:


*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (TIMJ):
*        Initial test version
*     2005-09-27 (AGG):
*        Now uses smurf_par.h
*        Factored out extinction correction
*     2005-09-27 (EC)
*        Trap memmove when status is bad
*     2005-11-09 (AGG)
*        Allow user to specify the optical depth at the start and end
*        of a scan - could be useful if we supply a group of files
*     2005-11-10 (AGG)
*        Perform check for dimensionality of input file and prompt
*        user only for 2-D image data. Now uses Grp interface for
*        setting input/output files and stores data in a smfData struct.
*     2005-12-20 (AGG):
*        Calls smf_flatfield to automatically flatfield data if necessary.
*     2005-12-21 (AGG):
*        Now deals with timeseries data
*     2006-01-10 (AGG):
*        Now reads the tau from the header for timeseries data
*     2006-01-24 (AGG):
*        Tau is now double, rather than float.
*     {enter_further_changes_here}

*  Notes:
*     At the moment no check is made on whether the extinction
*     correction has already been applied.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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
#define FUNC_NAME "smurf_extinction"
#define LEN__MODE 20

void smurf_extinction( int * status ) {

  /* Local Variables */
  int flag;                  /* Flag for how group is terminated */
  int i;                     /* Loop counter */
  Grp *igrp = NULL;          /* Input group */
  smfData *odata = NULL;     /* Output data struct */
  Grp *ogrp = NULL;          /* Output group */
  smfHead *ohdr;             /* Pointer to header in odata */
  int outsize;               /* Total number of NDF names in the output group */
  int size;                  /* Number of files in input group */
  double tau = 0.0;           /* Zenith tau at this wavelength */
  long filter;

  char mode[LEN__MODE];      /* String for optical depth mode */
  double deftau = 0.0;       /* Default value for the zenith tau */

  /* Main routine */

  ndfBegin();

  /*  msgOut( FUNC_NAME, "Inside EXTINCTION", status );*/

  /* Read the input file */
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Get output file(s) : assumes a 1:1 correspondence between input
     and output files */
  ndgCreat( "OUT", igrp, &ogrp, &outsize, &flag, status );

  /* Get MODE */
  parChoic( "MODE", "CSOTAU", 
	    "CSOtau,Filtertau,WVMraw,WVMsmooth,Polynomial,Data", 1,
	    &mode, LEN__MODE, status);

  for (i=1; i<=size; i++) {

    /* Flatfield - if necessary */
    smf_open_and_flatfield( igrp, ogrp, i, &odata, status );

    if (*status == SMF__FLATN) {
      errAnnul( status );
      msgOutif(MSG__VERB, "smurf_flatfield",
	     "smurf_flatfield: Data are already flatfielded", status);
    } else if ( *status == SAI__OK) {
      msgOutif(MSG__VERB," ","Flatfield applied", status);
    } else {
      /* Tell the user which file it was... */
      /* Would be user-friendly to trap 1st etc... */
      msgSeti("I",i);
      errRep("smurf_flatfield",
	     "Unable to flatfield data from the ^Ith file", status);
    }

    /* What next if status is bad? */

    /* Remove polynomials */

    /* Tell user if polynomials have already been removed */

    /* Check (and tell user) if extinction has already been done */

    /* If status is OK, make decisions on mode keywords */
    if ( *status == SAI__OK ) {
      if ( strncmp( mode, "CSOT", 4 ) == 0 ) {
	/* Now ask for desired CSO tau */
	/* Define the default value first time round */
	if ( i == 1 ) {
	  ohdr = odata->hdr;
	  smf_fits_getD( ohdr, "MEANWVM", &deftau, status );
	  parDef0d( "CSOTAU", deftau, status );
	}
	parGet0d( "CSOTAU", &tau, status);
      } else if ( strncmp( mode, "FILT", 4) == 0 ) {
	/* Now ask for desired Filter-based tau */
	/* Define the default value first time round */
	if ( i == 1 ) {
	  ohdr = odata->hdr;
	  smf_fits_getD( ohdr, "MEANWVM", &deftau, status );
	  deftau = smf_scale_tau( deftau, filter, status );
	  parDef0d( "FILTERTAU", deftau, status );
	}
	parGet0d( "FILTERTAU", &tau, status );
      } else if ( strncmp( mode, "WVMR", 4) == 0 ) {
	*status = SAI__ERROR;
	msgSetc("MODE", "WVMR");
	errRep("", "Sorry, mode, ^MODE, not supported yet", status);
      } else if ( strncmp( mode, "WVMS", 4) == 0 ) {
	*status = SAI__ERROR;
	msgSetc("MODE", "WVMS");
	errRep("", "Sorry, mode, ^MODE, not supported yet", status);
      } else if ( strncmp( mode, "POLY", 4) == 0 ) {
	*status = SAI__ERROR;
	msgSetc("MODE", "POLY");
	errRep("", "Sorry, mode, ^MODE, not supported yet", status);
      } else if ( strncmp( mode, "DATA", 4) == 0 ) {
	*status = SAI__ERROR;
	msgSetc("MODE", "DATA");
	errRep("", "Sorry, mode, ^MODE, not supported yet", status);
      } else {
	*status = SAI__ERROR;
	errRep("", "Unsupported mode. Possible programming error.", status);
      }
    }
    /* Apply extinction correction */
    smf_correct_extinction( odata, mode, tau, status );

    /* Free resources for output data */
    smf_close_file( &odata, status );
  }
  /* Tidy up after ourselves: release the resources used by the grp routines  */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
