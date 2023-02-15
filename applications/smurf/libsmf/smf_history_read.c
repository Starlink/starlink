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
*     This routine reads the SMURF history component for a file associated
*     with the current smfData and stores it in an AstKeyMap. This can
*     then be queried by routines checking whether a particular step
*     has been carried out. Note that SMURF uses a private scheme for
*     tracking activity relating to the data because a single SMURF command
*     can add multiple history items.

*  Authors:
*     AGG: Andy Gibb (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-04-19 (AGG):
*        Initial version, copied from smf_history_check
*     2006-07-05 (AGG):
*        Check for presence of history component before attempting to
*        read
*     2009-09-17 (TIMJ):
*        Use private HISTORY rather than standard NDF history. This gives
*        us more control and stops contamination of the main history. Main
*        motivation is that recently NDF started using the registered
*        application name rather than the override value given to.
*        ndfHput. NDF history is more than we need for a simple
*        keymap where we only need the keys.
*     2009-09-28 (TIMJ):
*        No longer warn people if history information is missing.

*  Notes:
*     - Checks are made assuming the subroutine names are lower case
*     - See also smf_history_write

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
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

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_history_read"

void smf_history_read( smfData* data,int *status) {

  char * buffer = NULL;       /* char buffer read from history info */
  dim_t bufelem = 0;          /* Size of one element in buffer */
  size_t clen = 0;            /* String length in _CHAR*X */
  smfFile *file = NULL;       /* data->file */
  size_t i = 0;               /* Loop counter */
  size_t nrec = 0;            /* Number of history records */
  AstKeyMap *history = NULL;  /* History to be stored in the smfData */
  char **pntrs = NULL;        /* Pointers to strings in buffer */
  HDSLoc * shloc = NULL;      /* Locator to smurf history extension */
  HDSLoc * sloc = NULL;       /* Locator to smurf extension */
  int there;                  /* Presence of components */

  /* Check entry status */
  if (*status != SAI__OK) return;

  /* check that we have a smfData */
  if (!smf_validate_smfData( data, 0, 1, status )) return;

  /* Special case sc2store file */
  file = data->file;
  if (file->isSc2store) return;

  /* Check that we have an NDF */
  if (file->ndfid == NDF__NOID) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
	    "Supplied smfData is associated with a file but it isn't an NDF",
	    status );
    return;
  }

  /* Need the SMURF extension */
  sloc = smf_get_smurfloc( data, "READ", status );
  if (!sloc) return;

  datThere( sloc, SMURF__HISTEXT, &there, status );

  /* If it exists, then continue to populate the AstKeyMap */
  if ( *status == SAI__OK && there ) {
    /* Create AstKeyMap */
    history = astKeyMap(" " );
    /* Stop with an error if the history could not be created */
    if ( history == AST__NULL) {
      if ( *status == SAI__OK ){
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "Unable to create history AstKeyMap", status);
      }
    }

    /* Simplest possible history is just an array of strings */
    datFind( sloc, SMURF__HISTEXT, &shloc, status );
    datClen( shloc, &clen, status );
    datSize( shloc, &nrec, status );

    pntrs = astMalloc( nrec*sizeof(*pntrs) );
    bufelem = (clen+1)*sizeof(*buffer);
    buffer = astCalloc( nrec, bufelem );

    datGet1C( shloc, nrec, bufelem*nrec, buffer, pntrs, &nrec, status );
    datAnnul( &shloc, status );

    /* Now copy out the buffer into the keymap */
    for (i = 0; i < nrec; i++ ) {
      /* Retrieve history record */
      astMapPut0I( history, pntrs[i], 1, " " );
    }

    pntrs = astFree( pntrs );
    buffer = astFree( buffer );

  }

  datAnnul( &sloc, status );

  /* Store history in smfData */
  data->history = history;
  /*astShow(history);*/

}
