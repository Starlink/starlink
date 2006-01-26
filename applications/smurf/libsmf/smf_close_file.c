/*
*+
*  Name:
*     smf_close_file

*  Purpose:
*     Free resources associated with a file

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_close_file( smfData **data, int * status );

*  Arguments:
*     data = smfData ** (Returned)
*        Pointer to Pointer to smfData struct containing file info and data.
*        Will be NULLed on exit.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function should be used to free resources associated with
*     a smfData struct, usually created by a call to smf_open_file.
*     Resources will be freed (or an attempt will be made) even if
*     status is bad on entry. This means that all pointers should be
*     initialised to NULL by the caller to protect against bad frees.
*     Only frees resources if the reference count on entry is 1.
*     Reference counts are always decremented even if status is bad.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-11-28 (TIMJ):
*        Initial test version
*     2005-12-02 (EC):
*        Fixed memory unallocation problems
*     2005-12-05 (TIMJ):
*        Tidy up logic from Ed's patch
*     2005-12-15 (TIMJ):
*        Add check for reference count.
*     2006-01-26 (TIMJ):
*        sc2head is now embedded in the struct
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

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

#include "smf.h"
#include "smf_typ.h"
#include "sae_par.h"
#include "star/hds.h"
#include "ndf.h"
#include "mers.h"
#include "sc2da/sc2store.h"

#define ERRFUNC "smf_close_file"

void smf_close_file( smfData ** data, int * status ) {

  smfHead * hdr;          /* pointer to smfHead in smfData */
  int       i;            /* loop counter */
  smfFile * file;         /* pointer to smfFile in smfData */
  int       freedata = 0; /* should the data arrays be freed? */

  if ( *status != SAI__OK) return;

  if ( *data == NULL ) {
    if ( *status == SAI__OK ) {
      /* Status is good so we have a problem */
      *status = SAI__ERROR;
      errRep( ERRFUNC, "Attempt to close file when smfData pointer is NULL (possible programming error)", 
	      status );
    }
    /* null pointer so just return */
    return;
  }

  /* Process reference count */
  /* Only proceed if the decremented reference count is 0 */
  (*data)->refcount--;
  if ((*data)->refcount > 0 ) return;

  /* Tidy up the header */
  hdr = (*data)->hdr;
  if (hdr != NULL) {
    if (hdr->wcs != NULL) astAnnul( hdr->wcs );
    if (hdr->fitshdr != NULL) astAnnul( hdr->fitshdr );
    smf_free( hdr, status );
  }

  /* now file information */
  file = (*data)->file;
  if (file != NULL) {

    if ( file->isSc2store ) {
      /* opened by DA library */
      sc2store_free( status );
    } else if ( file->ndfid != NDF__NOID ) {

      /* if this is a time series we need to free
	 the header structure if we have a smfHead */
      if (file->isTstream && hdr != NULL) sc2store_headunmap( status );

      /* Annul the NDF and locators (which will unmap things) */
      if ( file->xloc ) datAnnul( &(file->xloc), status );
      ndfAnnul( &(file->ndfid), status );

    } else {
      /* No file so free the data */
      freedata = 1;
    }

    smf_free( file, status );
  }

  /* Now the smfData itself */
  if ( (*data)->da != NULL ) smf_free( (*data)->da, status );

  /* Free the data arrays if they are non-null (they should have been
     freed if they were mapped to a file but not if they were stored
     in a separate action as temp storage */
  if (freedata) {
    for (i = 0; i < 3; i++ ) {
      if ( ((*data)->pntr)[i] != NULL ) smf_free( ((*data)->pntr)[i], status );
    }
  }

  /* finally free smfData */
  smf_free( *data, status );
  *data = NULL;

}
