/*
*+
*  Name:
*     smf_history_write

*  Purpose:
*     Write an extra history comment to a file

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_history_write( const smfData * data, const char * appl,
*                        const char * text, int * status );

*  Arguments:
*     data = const smfData* (Given)
*        Data struct to obtain file identifier from.
*     appl = const char * (Given)
*        Name of "application" to store in file. If NULL, the 
*        default application name will be used.
*     text = const char * (Given)
*        Descriptive text to write to the file. MERS message tokens
*        can be included. Text will be truncated at NDF__SZHMX characters.
*     status = int* (Given and Returned)
*        Pointer to global status. Will be bad on return if this smfData
*        is not associated with a file.

*  Description:
*     This function stores additional history information to a file.
*     It should be used primarily to tag activities that have been
*     performed on the data which can not be derived from the application
*     name. For example, since extinction correction may be performed
*     by both EXTINCTION and MAKEMAP it is important that a flag
*     can be written to the file to indicate that the data have been
*     extinction corrected.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-01-24 (TIMJ):
*        Initial version.
*     2006-05-05 (AGG):
*        Add called to ndfHend to allow multiple distinct history writes
*     2006-07-05 (AGG):
*        Check for presence of history component before attempting to
*        write
*     2008-07-08 (TIMJ):
*        Need to copy input text to internal buffer to avoid const warning
*        from NDF API.

*  Notes:
*     - SMURF subroutines should choose history "application" names
*       that are distinct from actual application names but describe
*       functionality. eg SMF_EXTINCTION, SMF_FLATFIELD rather than
*       "EXTINCTION" and "FLATFIELD".
*     - See also smf_history_check
*     - An error will occur if the file is opened read-only.
*     - It is an error to associate HISTORY with raw data files.
*     - HISTORY is only written to the file when it is closed. Therefore
*       do not expect to be able to call smf_history_check on the
*       same file that you just called smf_history_write on.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council
*     and University of British Coulmbia. All Rights Reserved.

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

/* System includes */
#include <string.h>

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"
#include "star/one.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_history_write"

void smf_history_write( const smfData* data, const char * appl, 
			const char * text, int *status) {
  smfFile *file = NULL;  /* data->file */
  char *linarr[1];       /* Pointer to char * text */
  int state;             /* State of history component in NDF */
  char buffer[NDF__SZHMX+1]; /* Temp buffer for copy of const text */

  /* Check entry status */
  if (*status != SAI__OK) return;

  /* check that we have a smfData */
  if ( data == NULL ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
	    "Supplied smfData is a NULL pointer. Possible programming error.",
	    status);
    return;
  }

  /* Check that we have a file */
  file = data->file;
  if ( file == NULL ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
	    "Supplied smfData is not associated with a file. Unable to write history", status );
    return;
  }

  /* Special case sc2store file */
  if (file->isSc2store && *status == SAI__OK) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "Writing HISTORY information to raw data is not allowed",
	    status);
  }

  /* Check that we have an NDF */
  if (file->ndfid == NDF__NOID) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
	    "Supplied smfData is associated with a file but it isn't an NDF",
	    status );
    return;
  }

  /* Check that we have a history component to write to */
  ndfState( file->ndfid, "HISTORY", &state, status );
  if ( state == 1 ) {
    /* If present, write the information to the file */
    one_strlcpy( linarr[0], text, sizeof(buffer), status);
    ndfHput("NORMAL", appl, 0, 1, linarr, 1, 1, 1, file->ndfid, status );
    /* Needed to write a separate line for each call of ndfHput */
    ndfHend( status ); 
  } else {
    /* Inform user if no history */
    msgOutif(MSG__VERB," ", "No history component present to write to. Continuing but this may cause problems later.", status);

  }

}
