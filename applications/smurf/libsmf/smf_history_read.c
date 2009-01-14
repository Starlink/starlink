/*
*+
*  Name:
*     smf_history_read

*  Purpose:
*     Reads the history from an NDF and stores it for querying

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_history_read( const smfData * data, int * status );

*  Arguments:
*     data = const smfData* (Given)
*        Data struct to check. This must be associated with an open
*        file.
*     status = int* (Given and Returned)
*        Pointer to global status. Will be bad on return if this smfData
*        is not associated with a file.

*  Description:
*     This routine reads the history component for a file associated
*     with the current smfData and stores it in an AstKeyMap. This can
*     then be queried by routines checking whether a particular step
*     has been carried out. Note that the only entries stored in the
*     history are those written by SMURF (starting with SMF_), not the
*     application-level NDF history entries.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-04-19 (AGG):
*        Initial version, copied from smf_history_check
*     2006-07-05 (AGG):
*        Check for presence of history component before attempting to
*        read

*  Notes:
*     - Checks are made assuming the subroutine names are lower case
*     - See also smf_history_write

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council and the University of British Columbia. All Rights
*     Reserved.

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

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_history_read"

void smf_history_read( smfData* data,int *status) {

  smfFile *file = NULL;       /* data->file */
  int i = 0;                  /* Loop counter */
  int nrec = 0;               /* Number of history records */
  char refappl[NDF__SZAPP+1]; /* Name of application from header record */
  AstKeyMap *history = NULL;  /* History to be stored in the smfData */
  int state;                  /* State of history component */

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
	    "Supplied smfData is not associated with a file. Unable to query history", status );
    return;
  }

  /* Special case sc2store file */
  if (file->isSc2store) {
    return;
  }

  /* Check that we have an NDF */
  if (file->ndfid == NDF__NOID) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
	    "Supplied smfData is associated with a file but it isn't an NDF",
	    status );
    return;
  }

  /* Check the state of the history component */
  ndfState( file->ndfid, "HISTORY", &state, status );

  /* If it exists, then continue to populate the AstKeyMap */
  if ( state == 1 ) {
    /* Found out how many history records we have */
    ndfHnrec( file->ndfid, &nrec, status );

    if ( *status == SAI__OK ) {
      /* Create AstKeyMap */
      history = astKeyMap(" " );
      /* Stop with an error if the history could not be created */
      if ( history == AST__NULL) {
	if ( *status == SAI__OK ){
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Unable to create history AstKeyMap", status);
	}
      }

      /* History records start at 1 */
      for (i = 1; i <= nrec; i++ ) {
	/* Retrieve history record */
	ndfHinfo(file->ndfid, "APPLICATION", i, refappl, NDF__SZAPP, status);
	/* Pick out the SMF_ routines which write their own history entries */
	if ( strncmp( refappl, "smf_", 4 ) == 0 ) {
	  /* Insert record into history. Note that previous values are
	     overwritten, thus avoiding duplicate entries. */
	  astMapPut0I( history, refappl, 1, " " );
	}
      }
    }
  } else {
    /* Inform user if no history */
    msgOutif(MSG__VERB," ", "No history component present. Continuing but this may cause problems later.", status);
  }

  /* Store history in smfData */
  data->history = history;
  /*astShow(history);*/

}
