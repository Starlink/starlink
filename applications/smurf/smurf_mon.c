

/*
*+
*  Name:
*     smurf_mon

*  Purpose:
*     Top-level SMURF subroutine for A-task monolith on UNIX.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_mon( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the top-level A-task monolith subroutine for the SMURF
*     suite of A-tasks.  Each SMURF command is an alias to a softlink
*     that points to this monolith.  The chosen command is obtained
*     from the ADAM routine TASK_GET_NAME.  The command may be specified
*     from the shell or ICL.  Given the command, the requested A-task
*     is called after a successful matching of the input string with a
*     valid task name.  If there is no match, an error report is made.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2005-09-26 (TIMJ):
*        Initial test version
*     2005-10-05 (TIMJ):
*        Register inherited status pointer with AST
*     2006-01-24 (TIMJ):
*        Use NDF__SZAPP.
*        Check for GRP leaks
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "sae_par.h"
#include "par_par.h"
#include "mers.h"
#include "f77.h"
#include "ndf.h"
#include "star/grp.h"

#include "libsmurf/smurflib.h"

F77_SUBROUTINE(task_get_name)(char *, int*, int);

void smurf_mon( int * status ) {

  /* Local variables */
  char taskname[PAR__SZNAM+1];
  char appname[NDF__SZAPP+1];
  int ngrp0;                   /* Number of grp ids at start */
  int ngrp1;                   /* Number of grp ids at end */

  if ( *status != SAI__OK ) return; 

  /* Register our status variable with AST */
  astWatch( status );

  /* Get the GRP status for leak checking */
  grpInfoI( NULL, 0, "NGRP", &ngrp0, status );

  /* Find out the task name we were invoked with */
  memset( taskname, ' ', PAR__SZNAM );
  taskname[PAR__SZNAM] = '\0';
  F77_CALL(task_get_name)(taskname,  status, PAR__SZNAM);
  cnfImprt( taskname, PAR__SZNAM, taskname);

  /* Update the application name in the NDF history recording
     to include the version number of the application */
  snprintf( appname, NDF__SZAPP, "%-*s (%s V%s)", PAR__SZNAM,
	    taskname, PACKAGE_UPCASE, PACKAGE_VERSION);
  ndfHappn( appname, status );
  msgIfget("MSG_FILTER", status);

  /* Call the subroutine associated with the requested task */
  if (strcmp( taskname, "EXTINCTION" ) == 0 ) {
    smurf_extinction( status );
  } else if (strcmp( taskname, "FLATFIELD" ) == 0 ) {
    smurf_flatfield( status );
  } else if (strcmp( taskname, "MAKEMAP" ) == 0 ) {
    smurf_makemap( status );
  } else {
    *status = SAI__ERROR;
    msgSetc( "TASK", taskname );
    errRep( "smurf_mon", "Unrecognized taskname: ^TASK", status);
  }

  /* Check for GRP leaks Do this in a new error reporting context so
   * that we get the correct value even if an error has occurred. */
  errBegin( status );
  grpInfoI( NULL, 0, "NGRP", &ngrp1, status );

  /* If there are more active groups now than there were on entry,
   * there must be a problem (GRP identifiers are not being freed
   * somewhere). So report it. */
  if (*status == SAI__OK && ngrp1 > ngrp0) {
    msgBlank( status );
    msgSetc( "NAME", taskname );
    msgSeti( "NGRP0", ngrp0 );
    msgSeti( "NGRP1", ngrp1 );
    msgOut( "SMURF_NGRP", "WARNING: The number of active "
	    "GRP identifiers increased from ^NGRP0 to ^NGRP1 "
	    "during execution of ^NAME (" PACKAGE_UPCASE " programming "
	    " error).", status);
    msgBlank(status);
  }
  errEnd( status );


}

