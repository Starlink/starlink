/*
*+
*  Name:
*     smf_get_ndfid

*  Purpose:
*     Return an HDS locator for an NDF extension

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     ndfid = int smf_get_ndfid ( const HDSLoc *loc, const char *name,
*                   const char *accmode,
*		    const char *state, const char *dattype, const int ndims,
*		    const dim_t *lbnd, const dim_t *ubnd, int *status )

*  Arguments:
*     loc = HDSLoc* (Given)
*        HDS locator for NDF extension
*     name = const char* (Given)
*        Name of extension
*     accmode = const char* (Given)
*        Access mode for locator (READ, WRITE or UPDATE)
*     state = const shar* (Given)
*        State of NDF (NEW, OLD or UNKNOWN)
*     dattype = const char* (Given)
*        Data type to be stored in NDF
*     ndims = const int (Given)
*        Number of dimensions in new locator
*     lbnd = const dim_t * (Given)
*        Pointer to array containing lower bounds for each axis
*     ubnd = const dim_t * (Given)
*        Pointer to array containing upper bounds for each axis
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Returns an NDF identifier for the specified NDF. A new NDF is
*     created if necessary. If an error occurs a value of NDF__NOID is
*     returned. It is up to the user to annul the NDF returned by this
*     routine.
*
*     For accessing existing NDFs the dattype, ndims, lbnd and ubnd
*     can all be left blank ("", 0, 0, 0 respectively).

* Notes:
*     - Note that if the named NDF exists, opening it with WRITE access
*       will clear the contents.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-05-09 (AGG):
*        Initial version
*     2006-05-16 (AGG):
*        Add status checking, check if placeholder returned from ndfOpen
*     2006-07-24 (AGG):
*        Update prologue for dealing with existing NDFs
*     2006-10-16 (AGG):
*        Rejig checking and feedback to user
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 University of British Columbia. All Rights
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


#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* Starlink includes */
#include "sae_par.h"
#include "star/ndg.h"
#include "ndf.h"
#include "ast.h"
#include "mers.h"
#include "star/kaplibs.h"
#include "kpg_err.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_get_ndfid"

int smf_get_ndfid ( const HDSLoc *loc, const char *name, const char *accmode,
		    const char *state, const char *dattype, const int ndims,
		    const dim_t *lbnd, const dim_t *ubnd, int *status ) {

  /* Local variables */
  int ndfid;                /* NDF identifier for named component */
  int place;                /* Placeholder for NDF */

  if ( *status != SAI__OK ) return NDF__NOID;

  /* Retrieve NDF identifier from smfFile */
  if ( loc == NULL ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
	    "Supplied HDS locator is NULL. Possible programming error.",
	    status);
    return NDF__NOID;
  }

  /* Note: write access clears the contents of the NDF */
  if ( strncmp( accmode, "WRITE", 5 ) == 0
       || strncmp( accmode, "UPDATE", 6 ) == 0 ) {
    /* Does the NDF already exist? If so tell the user that it's going
       to be overwritten. */
    msgSetc("N",name);
    msgSetc("A",accmode);
    if ( (strncmp( state, "NEW", 3 ) == 0) ||
         (strncmp( state, "UNKNOWN", 3 ) == 0 ) ) {
      msgOutif(MSG__DEBUG," ", "Opening new NDF ^N with ^A access", status);
    } else {
      msgOutif(MSG__DEBUG," ", "Opening NDF ^N with ^A access"
		"NDF ^N may already exist: opening this NDF will clear the current contents.", status);
    }
  }
  ndfOpen( loc, name, accmode, state, &ndfid, &place, status );
  if ( *status != SAI__OK ) {
    errRep( FUNC_NAME,
	    "Call to ndfOpen failed: unable to obtain an NDF identifier",
	    status );
    return NDF__NOID;
  }

  /* No placeholder => NDF exists */
  if ( place != NDF__NOPL ) {
    /* Define properties of NDF */
    ndfNew( dattype, ndims, lbnd, ubnd, &place, &ndfid, status );
    if ( *status != SAI__OK ) {
      errRep( FUNC_NAME, "Unable to create a new NDF", status );
      return NDF__NOID;
    }
  }

  return ndfid;
}
