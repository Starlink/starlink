/*
*+
*  Name:
*     smf_get_xloc

*  Purpose:
*     Return an HDS locator for an NDF extension

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     xloc = smf_get_xloc( const smfData *data, const char *extname, 
*			const char *extype, const char *accmode, 
*			const int ndims, const int *dims, int *status );

*  Arguments:
*     data = const smfData* (Given)
*        smfData struct
*     extname = const char* (Given)
*        Name of extension
*     extype = const char* (Given)
*        Descriptor for locator
*     accmode = const char* (Given)
*        Access mode for locator (READ, WRITE or UPDATE)
*     ndims = const int (Given)
*        Number of dimensions in new locator
*     dims = const int* (Given)
*        Pointer to array containing size of each axis
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Check the existence of the specified HDS locator, creating it if
*     it does not exist. Returns a pointer to an HDS locator If an
*     error occurs a NULL pointer is returned.

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

#define FUNC_NAME "smf_get_xloc"

HDSLoc * smf_get_xloc ( const smfData *data, const char *extname, 
			const char *extype, const char *accmode, 
			const int ndims, const int *dims, int *status ) {

  smfFile *file;            /* Pointer to input file struct */
  int indf;                 /* NDF identifier for input file */
  HDSLoc *loc = NULL;       /* Locator to return */
  int itexists;             /* Flag to denote whether extension exists */

  if ( *status != SAI__OK ) return NULL;

  /* Retrieve NDF identifier from smfFile */
  if ( data == NULL ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
	    "Supplied smfData is a NULL pointer. Possible programming error.",
	    status);
    return NULL;
  }
  file = data->file;
  if ( file == NULL ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Supplied smfData is not associated with a file, unable to create a locator", status);
    return NULL;
  }
  indf = file->ndfid;
  if ( indf == NDF__NOID ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Supplied smfData is associated with a file but it isn't an NDF", status);
    return NULL;
  }

  /* See if desired extension exists */
  ndfXstat( indf, extname, &itexists, status );
  if ( itexists ) {
    /* OK, get a locator for it */
    ndfXloc( indf, extname, accmode, &loc, status );
    if ( loc == NULL ) {
      if ( *status == SAI__OK) {
	msgSetc("E", extname);
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "Unable to obtain an HDS locator to the ^E extension, despite its existence", status);
      }
    } else {
      msgSetc("E", extname);
      msgOutif(MSG__VERB, FUNC_NAME, 
	       "Found extension named ^E.", status);
    }
  } else {
    msgSetc("E", extname);
    msgOutif(MSG__VERB, FUNC_NAME, 
	     "Warning: no extension named ^E. Creating it now.", status);
    /* Create scu2red extension */
    ndfXnew( indf, extname, extype, ndims, dims, &loc, status );
  }

  return loc;
}
