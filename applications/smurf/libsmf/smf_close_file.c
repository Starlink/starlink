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
*     David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  Notes:
*     - Attempts to free resources even if status is bad on entry.

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
*     2006-01-27 (TIMJ):
*        - Free the data array from sc2store
*        - Free new allsc2heads component, not sc2head
*        - Free flatfield information if isSc2store
*        - respect new isCloned flag in smfHead
*     2006-06-12 (EC):
*        Free resources associated with .SMURF.MAPCOORD extension if needed
*     2006-06-25 (EC):
*        Now put MAPCOORD into SCU2RED extension
*     2006-07-26 (TIMJ):
*        sc2head no longer used. Use JCMTState instead.
*     2006-08-02 (TIMJ):
*        Free fplane coordinates.
*     2006-08-08 (TIMJ):
*        Do not return immediately if status is bad.
*        Annul tswcs.
*     2006-08-24 (AGG):
*        Free smfDream if present
*     2006-10-02 (DSB):
*        Free detpos coordinates.
*     2006-11-06 (DSB):
*        Free detname memory.
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

  smfDA   * da;           /* pointer to smfDA in smfData */
  smfHead * hdr;          /* pointer to smfHead in smfData */
  int       i;            /* loop counter */
  smfFile * file;         /* pointer to smfFile in smfData */
  int       freedata = 0; /* should the data arrays be freed? */
  int       isSc2store = 0; /* is this sc2Store data */
  smfDream *dream = NULL; /* Pointer to smfDream in smfData */

  /* we need to be able to clean up even if input status is bad -
     this requires some defensive programming. */

  if ( *data == NULL ) {
    if ( *status == SAI__OK ) {
      /* Status is good so we have a problem */
      *status = SAI__ERROR;
      errRep( ERRFUNC, "Attempt to close file when smfData pointer is NULL (possible programming error)", 
	      status );
    }
    /* null pointer so just return since there is nothing to free */
    return;
  }

  /* Process reference count */
  /* Only proceed if the decremented reference count is 0 */
  (*data)->refcount--;
  if ((*data)->refcount > 0 ) return;
  
  /* Get the header and file, since we need them for checking */
  hdr = (*data)->hdr;

  /* Before annulling close NDF try closing SCU2RED.MAPCOORD */
  smf_close_mapcoord( *data, status );

  /* now file information */
  file = (*data)->file;
  if (file != NULL) {

    if ( file->isSc2store ) {
      /* Nothing to free here but we do need to free the data array
	 using free() not smf_free [since sc2store uses native malloc]
	 and also free the hdr->allState */
      isSc2store = 1;

    } else if ( file->ndfid != NDF__NOID ) {
      /* Annul the NDF (which will unmap things) */
      ndfAnnul( &(file->ndfid), status );
      	
    } else {
      /* No file so free the data */
      freedata = 1;
    }

    smf_free( file, status );
  } else {
    /* no file - data is ours to free */
    freedata = 1;
  }

  /* Tidy up the header */
  if (hdr != NULL) {
    if (hdr->wcs != NULL) astAnnul( hdr->wcs );
    if (hdr->tswcs != NULL) astAnnul( hdr->tswcs );
    if (hdr->fitshdr != NULL) astAnnul( hdr->fitshdr );
    if (!hdr->isCloned) {
      /* We are responsible for this memory - although what happens
	 when we are cloned and the original is freed first? Need
	 to think carefully about memory management. */
      if (hdr->allState != NULL) {
	if (isSc2store) {
	  /* make sure we use free() */
	  free(hdr->allState);
	} else {
	  smf_free(hdr->allState, status);
	}
      }
      if (hdr->fplanex) smf_free( hdr->fplanex, status );
      if (hdr->fplaney) smf_free( hdr->fplaney, status );
      if (hdr->detpos) smf_free( hdr->detpos, status );
      if (hdr->detname) smf_free( hdr->detname, status );
    }
    smf_free( hdr, status );
  }

  /* Now the smfDA itself */
  if ( (*data)->da != NULL ) {
    da = (*data)->da;
    smf_free( da->flatcal, status );
    smf_free( da->flatpar, status );
    smf_free( da, status );
  }

  /* Free smfDream */
  if ( (*data)->dream != NULL ) {
    dream = (*data)->dream;
    if ( dream->gridwts != NULL) 
      smf_free( dream->gridwts, status );
    if ( dream->invmatx != NULL) 
      smf_free( dream->invmatx, status );
    smf_free( dream, status );
  }

  /* Free the data arrays if they are non-null (they should have been
     freed if they were mapped to a file but not if they were stored
     in a separate action as temp storage */
  if (freedata) {
    for (i = 0; i < 3; i++ ) {
      if ( ((*data)->pntr)[i] != NULL ) 
        smf_free( ((*data)->pntr)[i], status );
    }
  } else if (isSc2store) {
    /* just the data array */
    free( ((*data)->pntr)[0] );
  }

  /* finally free smfData */
  smf_free( *data, status );
  *data = NULL;

}
