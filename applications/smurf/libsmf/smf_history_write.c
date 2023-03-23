/*
*+
*  Name:
*     smf_history_write

*  Purpose:
*     Write SMURF processing history to file

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_history_write( const smfData * data, int *status );

*  Arguments:
*     data = const smfData* (Given)
*        Data struct to obtain file identifier from and history keymap
*     status = int* (Given and Returned)
*        Pointer to global status. Will be bad on return if this smfData
*        is not associated with a file.

*  Description:
*     This function synchronizes the history information stored in
*     the smfData with the file on disk. Does nothing if the smfData
*     is not open for write access. Does not affect the main NDF
*     HISTORY record.
*
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
*     2009-09-17 (TIMJ):
*        No longer use NDF HISTORY.

*  Notes:
*     - SMURF subroutines should choose history "application" names
*       that are distinct from actual application names but describe
*       functionality. eg SMF_EXTINCTION, SMF_FLATFIELD rather than
*       "EXTINCTION" and "FLATFIELD".
*     - See also smf_history_check
*     - An error will occur if the file is opened read-only.
*     - It is an error to associate HISTORY with raw data files.
*     - The file is updated immediately.

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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

#pragma GCC diagnostic ignored "-Wcast-qual"

void smf_history_write( const smfData* data, int *status) {

  smfFile *file = NULL;  /* data->file */
  int there;             /* Is component there? */

  /* Check entry status */
  if (*status != SAI__OK) return;

  /* check that we have a smfData */
  if (!smf_validate_smfData( data, 0, 1, status) ) return;

  /* Special case sc2store file */
  file = data->file;
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
  ndfIsacc( file->ndfid, "WRITE", &there, status );
  if ( there ) {
    HDSLoc *sloc = NULL;  /* Locator to SMURF extension */

    /* Locate the SMURF extension */
    sloc = smf_get_smurfloc( data, "UPDATE", status );
    datThere( sloc, SMURF__HISTEXT, &there, status );
    if (there) datErase( sloc, SMURF__HISTEXT, status );
    /* only write history if we have any keys in the keymap */
    if (data->history) {
      HDSLoc * shloc = NULL; /* Locator to SMURHIST component */

      dim_t nrec = astMapSize( data->history );
      if (nrec > 0) {
	dim_t maxlen = 0;
	int i;
	char ** array = NULL;
	/* Create a char** array and attach strings. Also find the
	   longest string as we go */
	array = astMalloc( nrec*sizeof(*array) );
	if (array) {
	  for ( i=0; i<nrec; i++ ) {
	    const char *key = NULL;
	    dim_t len = 0;
	    key = astMapKey( data->history, i );
	    len = strlen( key );
	    if ( len > maxlen ) maxlen = len;

	    array[i] = astMalloc( 1*(len+1)*sizeof(**array) );
	    one_strlcpy( array[i], key, len+1, status );
	  }
	}

	/* Create new history array and copy in the values */
	datNew1C( sloc, SMURF__HISTEXT, maxlen, nrec, status );
	datFind( sloc, SMURF__HISTEXT, &shloc, status );
	datPut1C( shloc, nrec, (const char**)array, status );

	/* free everything */
	for ( i=0; i<nrec; i++ ) {
	  array[i] = astFree( array[i] );
	}
	array = astFree( array );
	datAnnul( &shloc, status );
      }
    }
    datAnnul( &sloc, status );

  } else {
    /* Inform user if no history */
    msgOutif(MSG__VERB," ",
	     "NDF does not have write permission enabled so can not update processing history."
	     " This may cause problems later.", status);
  }

}
