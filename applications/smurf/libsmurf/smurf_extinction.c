/*
*+
*  Name:
*     EXTINCTION

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
*     IN = NDF (Read)
*          Input file(s)
*     METHOD = CHAR (Read)
*          Optical depth method. Valid methods are WVMRAW, CSOTAU,
*          FILTERTAU.
*     QUICK = LOGICAL (Read)
*          Flag for applying the Quick method
*     CSOTAU = REAL (Read)
*          Value of the 225 GHz zenith optical depth. Only used if
*          METHOD = CSOTAU. If a null (!) value is given, the task
*          will use the value of the MEANWVM FITS header for each
*          file. Note that if a value is entered by the user, that
*          value is used for all input files.
*     FILTERTAU = REAL (Read)
*          Value of the zenith optical depth for the current
*          wavelength. Only used if METHOD = FILTERTAU.
*     OUT = NDF (Write)
*          Output file(s)
*     HASSKYREM = LOGICAL (Read)
*          Indicate that the data have been sky removed even if the
*          fact can not be verified. This is useful for the case where you
*          have removed the sky background using an application
*          other than SMURF REMSKY. Default is false.

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
*     2006-01-25 (AGG):
*        Mode keyword changed to Method.
*     2006-02-03 (AGG):
*        Filter now a string
*     2006-02-07 (AGG):
*        Can now use the WVMRAW method for getting tau
*     2006-04-21 (AGG):
*        Add call to smf_subtract_plane for sky removal
*     2008-03-05 (EC):
*        Changed smf_correct_extinction interface
*     2008-04-29 (AGG):
*        Remove sky subtraction call, remove placeholder code for future
*        methods
*     2008-05-01 (TIMJ):
*        Add title to output file.
*     2008-05-09 (TIMJ):
*        Enable ability to override remsky nannying.
*     2008-06-10 (AGG):
*        Allow null value for CSO tau to use MEANWVM from current header
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2005 Particle Physics and Astronomy Research
*     Council. Copyright (C) 2005-2008 University of British
*     Columbia. All Rights Reserved.

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
#define FUNC_NAME "smurf_extinction"
#define TASK_NAME "EXTINCTION"
#define LEN__METHOD 20

void smurf_extinction( int * status ) {

  /* Local Variables */
  double deftau = 0.0;       /* Default value for the zenith tau */
  char filter[81];           /* Name of filter */
  int flag;                  /* Flag for how group is terminated */
  int has_been_sky_removed = 0;/* Data are sky-removed */
  size_t i;                  /* Loop counter */
  Grp *igrp = NULL;          /* Input group */
  char method[LEN__METHOD];  /* String for optical depth method */
  smfData *odata = NULL;     /* Output data struct */
  Grp *ogrp = NULL;          /* Output group */
  smfHead *ohdr = NULL;      /* Pointer to header in odata */
  size_t outsize;            /* Total number of NDF names in the output group */
  int quick;                 /* Flag to denote whether to assume a
                                single airmass for all bolometers */
  size_t size;               /* Number of files in input group */
  double tau = 0.0;          /* Zenith tau at this wavelength */

  /* Main routine */
  ndfBegin();

  /* Read the input file */
  ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

  /* Get output file(s): assumes a 1:1 correspondence between input
     and output files */
  ndgCreat( "OUT", igrp, &ogrp, &outsize, &flag, status );

  /* Get METHOD */
  parChoic( "METHOD", "CSOTAU", 
	    "CSOtau, Filtertau, WVMraw", 1,
	    method, LEN__METHOD, status);

  /* Get QUICK flag */
  parGet0l( "QUICK", &quick, status);

  for (i=1; i<=size && ( *status == SAI__OK ); i++) {

    /* Flatfield - if necessary */
    smf_open_and_flatfield( igrp, ogrp, i, &odata, status );
    /* Check flatfield status */
    if (*status == SMF__FLATN) {
      errAnnul( status );
      msgOutif(MSG__VERB, "",
	     TASK_NAME ": Data are already flatfielded", status);
    } else if ( *status == SAI__OK) {
      msgOutif(MSG__VERB," ","Flatfield applied", status);
    } else {
      /* Error flatfielding: tell the user which file it was */
      msgSeti("I",i);
      msgSeti("N",size);
      errRep(TASK_NAME, "Unable to flatfield data from file ^I of ^N", status);
    }

    /* Now check that the data are sky-subtracted */
    if ( !smf_history_check( odata, "smf_subtract_plane", status ) ) {

      /* Should we override remsky check? */
      parGet0l("HASSKYREM", &has_been_sky_removed, status);

      if ( !has_been_sky_removed && *status == SAI__OK ) {
        *status = SAI__ERROR;
        msgSeti("I",i);
        errRep("", "Input data from file ^I are not sky-subtracted", status);
      }
    }

    /* If status is OK, make decisions on method keywords */
    if ( *status == SAI__OK ) {
      if ( strncmp( method, "CSOT", 4 ) == 0 ) {
	/* Now ask for desired CSO tau */
	/* Define the default value first time round */
	if ( i == 1 ) {
	  ohdr = odata->hdr;
	  smf_fits_getD( ohdr, "MEANWVM", &deftau, status );
	  parDef0d( "CSOTAU", deftau, status );
	}
	parGet0d( "CSOTAU", &tau, status);
	/* If a null value was given, use the MEANWVM from the header
	   for this file */
	if ( *status == PAR__NULL ) {
	  errAnnul(status);
	  ohdr = odata->hdr;
	  smf_fits_getD( ohdr, "MEANWVM", &tau, status );
	}
      } else if ( strncmp( method, "FILT", 4) == 0 ) {
	/* Now ask for desired Filter-based tau */
	/* Define the default value first time round */
	if ( i == 1 ) {
	  ohdr = odata->hdr;
	  smf_fits_getS( ohdr, "FILTER", filter, 81, status);
	  smf_fits_getD( ohdr, "MEANWVM", &deftau, status );
	  deftau = smf_scale_tau( deftau, filter, status );
	  parDef0d( "FILTERTAU", deftau, status );
	}
	parGet0d( "FILTERTAU", &tau, status );
      } else if ( strncmp( method, "WVMR", 4) == 0 ) {
	msgOutif(MSG__VERB," ", "Using Raw WVM data", status);
      } else {
	*status = SAI__ERROR;
	errRep("", "Unsupported method. Possible programming error.", status);
      }
    }
    /* Apply extinction correction - note that a check is made to
       determine whether the data have already been extinction
       corrected */
    smf_correct_extinction( odata, method, quick, tau, NULL, status );

    /* Set character labels */
    smf_set_clabels( "Extinction corrected",NULL, NULL, odata->hdr, status);
    smf_write_clabels( odata, status );

    /* Free resources for output data */
    smf_close_file( &odata, status );
  }
  /* Tidy up after ourselves: release the resources used by the grp routines  */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
