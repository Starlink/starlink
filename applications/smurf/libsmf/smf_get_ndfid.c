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
*		    const int *lbnd, const int *ubnd, int *status ) 

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
*     lbnd = const int* (Given)
*        Pointer to array containing lower bounds for each axis
*     ubnd = const int* (Given)
*        Pointer to array containing upper bounds for each axis
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Returns an NDF identifier for the specified NDF. A new NDF is
*     created if necessary. If an error occurs a value of NDF__NOID is
*     returned. It is up to the user to annul the NDF returned by this
*     routine.

* Notes:
*     - Note that if the named NDF exists, opening it with WRITE access
*       will clear the contents.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-05-09 (AGG):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 University of British Columbia. All Rights
*     Reserved.

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

#include <string.h>
#include <stdio.h>

#include "sae_par.h"
#include "star/ndg.h"
#include "ndf.h"
#include "ast.h"
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"
#include "mers.h"
#include "star/kaplibs.h"
#include "kpg_err.h"

#include "smf.h"
#include "smurf_par.h"
#include "libsmurf/smurflib.h"
#include "smf_err.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2math.h"

#define FUNC_NAME "smf_get_ndfid"

int smf_get_ndfid ( const HDSLoc *loc, const char *name, const char *accmode, 
		    const char *state, const char *dattype, const int ndims, 
		    const int *lbnd, const int *ubnd, int *status ) {

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
  if ( strncmp( accmode, "WRITE", 5 ) == 0 ) {
    msgOutif( MSG__VERB, FUNC_NAME, "Opening NDF with WRITE access: this will clear the current contents if the NDF exists.", status);
  }
  ndfOpen( loc, name, accmode, state, &ndfid, &place, status );

  /* What if accmode = READ? */

  /* Define properties of NDF */
  ndfNew( dattype, ndims, lbnd, ubnd, &place, &ndfid, status );

  return ndfid;
}
