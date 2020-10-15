/*
*+
*  Name:
*     RAWFIXMETA

*  Purpose:
*     Fix metadata associated with a raw data file.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_rawfixmeta( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Report any issues associated the metadata of a particular
*     file. In most cases SMURF applications will automatically apply
*     these corrections but the command can be used to investigate
*     issues prior to making a map.

*  Notes:
*     - Supports ACSIS raw data files
*     - In the future this command may gain the ability to fix the data files.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be checked.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     STEPTIME = _DOUBLE (Write)
*          Average steptime for the given file in seconds. Only
*          written if a single file is given.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2009-05-18 (TIMJ):
*        Original version
*     2012-05-10 (AGG):
*        Write steptime into ADAM parameter

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
*     Copyright (C) 2012 University of British Columbia
*     All Rights Reserved.

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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include "star/grp.h"
#include "star/kaplibs.h"
#include "sae_par.h"
#include "mers.h"
#include "par.h"
#include "ast.h"

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"

void smurf_rawfixmeta( int * status ) {

  smfData *data=NULL;        /* Data file to be tested */
  size_t i;
  Grp *igrp = NULL;          /* Input group */
  size_t size;               /* Number of files in input group */


  if (*status != SAI__OK) return;

  ndfBegin();

  /* Read the input file group */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  for (i=1; i<=size && ( *status == SAI__OK ); i++) {

    /* First open the file for READ to see whether we need to modify it */
    smf_open_file( NULL, igrp, i, "READ", SMF__NOCREATE_DATA | SMF__NOFIX_METADATA, &data, status );

    /* see if things need fixing */
    (void) smf_fix_metadata( MSG__NORM, data, status );

    /* if we have modified the smfData we need to write out the results.
       Might be handy to know which bit has been changed (FitsChan,
       JCMTState, ACSIS extension).

       Also need to know whether to update in place or to create a new file.
       We do not know how many output files we need so may need to do this in two
       loops.
     */

    /* If given only a single file, write out the STEPTIME */
    if (size == 1) {
      double steptime;
      astGetFitsF(data->hdr->fitshdr, "STEPTIME", &steptime);
      parPut0d("STEPTIME", steptime, status);
    }

    smf_close_file( NULL, &data, status );
  }

  /* Cleanup */
  grpDelet( &igrp, status);
  ndfEnd( status );
}
