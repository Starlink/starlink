/*
*+
*  Name:
*     DREAMWEIGHTS

*  Purpose:
*     Top-level DREAM weight matrix generation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_dreamweights( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine for (re)calculating the DREAM weights array
*     and inverse matrix.

*  Notes:
*     - Raw data MUST be passed in at present (this is due to a 
*       limitation in smf_open_file)
*     - Should allow for a list of bad (dead) bolometers

*  ADAM Parameters:
*     NDF = NDF (Read)
*          Raw DREAM data file(s)
*     CONFIG = Literal (Read)
*          Name of config file. If CONFIG=! some default parameters will be
*          used.
*     OUT = NDF (Write)
*          Output weights file(s)

*  Authors:
*     Andy Gibb (UBC)
*     Ed Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-08-22 (AGG):
*        Initial version, copied from calcmapwts written by BDK
*     2006-09-15 (AGG):
*        Factor out determining the grid parameters into
*        smf_dream_getgrid
*     2006-09-15 (AGG):
*        Factor out remaining code into smf_dream_calcweights
*     2006-11-10 (AGG):
*        Set some default values if a config file is not specified
*     2007-10-29 (EC):
*        Modified interface to smf_open_file.
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-05-28 (TIMJ):
*        Allow CONFIG=!
*     2008-07-11 (AGG):
*        Tidy up, ensure all pointers are freed
*     2008-07-14 (AGG):
*        Make sure Grp for config is always freed
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006-2008 the University of British Columbia. All
*     Rights Reserved.

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

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"
#include "smurf_typ.h"

#define FUNC_NAME "smurf_dreamwweights"
#define TASK_NAME "DREAMWEIGHTS"

void smurf_dreamweights ( int *status ) {

  /* Local Variables */
  Grp *confgrp = NULL;        /* Group containing configuration file */
  smfData *data = NULL;       /* Input data */
  int flag;                   /* Flag for Grp handling */
  int *gridminmax = NULL;     /* Extent of grid points array */
  int gridpts[DREAM__MXGRID][2]; /* Array of points for reconstruction grid */
  double gridstep;            /* Size of reconstruction grid in arcsec */
  size_t i;                   /* Loop counter */
  Grp *igrp = NULL;           /* Input group of NDFs */
  size_t isize;               /* Size of input Grp of files */
  AstKeyMap *keymap = NULL;   /* Pointer to keymap of config settings */
  size_t ksize;               /* Size of group containing CONFIG file */
  int ngrid;                  /* Number of points in reconstruction grid */
  Grp *ogrp = NULL;           /* Group of output weights files */
  size_t osize;               /* Size of output Grp of files */

  /* Main routine */
  ndfBegin();
  
  /* Get group of input raw data NDFs */
  ndgAssoc( "NDF", 1, &igrp, &isize, &flag, status );

  /* Get group of output files from user: assume 1 output file for
     every input file */
  ndgCreat( "OUT", igrp, &ogrp, &osize, &flag, status );
  if ( osize != isize ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti("^I",isize);
      msgSeti("^O",osize);
      errRep(FUNC_NAME, "Number of output files, ^O, not equal to number of input files, ^I",
	     status);
    }
  }

  /* Read configuration settings into keymap */
  if (*status == SAI__OK) {
    kpg1Gtgrp( "CONFIG", &confgrp, &ksize, status );
    if (*status == PAR__NULL) {
      /* NULL value so provide defaults */
      errAnnul( status );
      msgOutif(MSG__VERB, " ", "No config file specified - assuming default configuration parameters", status);
      keymap = astKeyMap("");
      astMapPut0I( keymap, "GRIDXMIN", -4, "" );
      astMapPut0I( keymap, "GRIDXMAX", 4, "" );
      astMapPut0I( keymap, "GRIDYMIN", -4, "" );
      astMapPut0I( keymap, "GRIDYMAX", 4, "" );
      astMapPut0D( keymap, "GRIDSTEP", 6.28, "" );
    } else {
      kpg1Kymap( confgrp, &keymap, status );
    }
    if( confgrp ) grpDelet( &confgrp, status );      
  }

  /* Determine grid parameters from inputs given above */
  smf_dream_getgrid( keymap, &gridstep, &ngrid, &gridminmax, gridpts, status);
  /* Annul keymap immediately as it is no longer required */
  if (keymap) keymap = astAnnul( keymap );

  /* Loop over number of files */
  for ( i=1; (i<= isize) && (*status == SAI__OK); i++) {
    /* Open file */
    smf_open_file( igrp, i, "READ", 0, &data, status );

    /* Calculate weights based on this file */
    smf_dream_calcweights( data, ogrp, i, gridstep, ngrid, gridminmax, 
			   &(gridpts[0]), status);

    /* Immediately check status on return and abort if an error occured */
    if ( *status != SAI__OK ) {
      msgSeti("I",i);
      msgSeti("N",isize);
      errRep(FUNC_NAME,	"Unable to determine DREAM weights for file ^I of ^N", 
	     status);
    }

    smf_close_file( &data, status );
  }

  /* Free up resources */
  if ( gridminmax != NULL ) {
    gridminmax = smf_free( gridminmax, status);
  }
  if ( ogrp != NULL ) {
    grpDelet( &ogrp, status);
  }
  if ( igrp != NULL ) {
    grpDelet( &igrp, status);
  }
  ndfEnd( status );
  
  msgOutif(MSG__VERB," ", "DREAM weights calculation completed successfully", 
	   status);
}
