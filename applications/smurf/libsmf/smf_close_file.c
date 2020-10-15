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
*     smf_close_file( ThrWorkForce *wf, smfData **data, int * status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
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
*     COBA: Coskun Oba (UoL)
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
*     2006-1-19 (DSB):
*        Free tsys memory.
*     2007-06-13 (EC)
*        If smfData associated with DIMM component, free resources
*     2007-06-14 (EC)
*        Moved DIMM file info into smfFile, so handle closing there
*     2007-06-25 (EC)
*        Check for file descriptor if unmap is required
*     2007-12-14 (EC)
*        Unmap the header portion of DIMM files
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-01-04 (TIMJ):
*        Remove debugging printf statements.
*     2008-5-01 (DSB):
*        Display any warnings placed in the FitsChan by AST.
*     2008-6-30 (DSB):
*        Free the sc2ast cache.
*     2008-07-10 (TIMJ):
*        Free dark squid information.
*     2008-12-03 (DSB):
*        Free caches used by smf_create_lutwcs and smf_detpos_wcs.
*     2009-06-23 (TIMJ):
*        Free ocsconfig
*     2010-06-18 (TIMJ):
*        Free file quality as special case.
*     2010-08-19 (TIMJ):
*        sc2store no longer uses system malloc in SMURF
*     2010-09-17 (COBA):
*        Free smfFts
*     2010-10-19 (COBA):
*        Reformat smfFts cleanup
*     2014-01-17 (DSB):
*        It is possible for a smfData to have an associated NDF
*        identifier, but for the data pointers to refer to locally
*        allocated memory rather than mapped NDF arrays (e.g.
*        smf_dataOrder can cause this condition). In such cases the data
*        pointers should be freed here. Previously, this was not done
*        because the smfData has an assocuated NDF identifier. Now, an
*        extra flag ("isdyn") is used to over-ride this to force the
*        data pointers to be freed.
*     2016-11-9 (DSB):
*        The qbits field in the smfData structure now indicates the
*        quality bits that are to be exported to the NDF.
*     2017-01-10 (GSB):
*        Pass dtai=VAL__BADD to WCS functions.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2009 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2016-2017 East Asian Observatory.
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


/* General includes */
#include <sys/mman.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

/* SMURF and STARLINK includes */
#include "smf.h"
#include "smf_typ.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/kaplibs.h"
#include "star/thr.h"
#include "ndf.h"
#include "mers.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"

#define ERRFUNC "smf_close_file"

void smf_close_file( ThrWorkForce *wf, smfData ** data, int * status ) {

  void *buf=NULL;         /* Buffer pointer */
  dim_t buflen=0;        /* Size of buffer */
  smfDA   * da;           /* pointer to smfDA in smfData */
  smfFts* fts;            /* pointer to smfFts in smfData */
  dim_t datalen=0;       /* Size of data buffer in bytes */
  smfDream *dream = NULL; /* Pointer to smfDream in smfData */
  smfFile * file;         /* pointer to smfFile in smfData */
  int       freedata = 0; /* should the data arrays be freed? */
  smfHead * hdr;          /* pointer to smfHead in smfData */
  dim_t headlen=0;       /* Size of header (mmap'd files) in bytes */
  dim_t       i;         /* loop counter */
  smfDIMMHead *temphead=NULL; /* Pointer to DIMM header */


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

  /* Display any warnings generated by AST and stored in the FitsChan. */
  file = (*data)->file;
  if( hdr && hdr->fitshdr ) {
    fts1Astwn( hdr->fitshdr,
               file ? file->ndfid : NDF__NOID,
               status );
  }

  /* now file information */
  if (file != NULL) {

    if ( file->isSc2store ) {
      /* Nothing to free here except for data */
      freedata = 1;

    } else if ( file->ndfid != NDF__NOID ) {

      /* Handle quality as a special case */
      if ( (*data)->qual) {
        (*data)->qual = smf_qual_unmap( wf, file->ndfid, (*data)->qfamily,
                                        (*data)->qual, (*data)->qbits,
                                        status );
      }

      /* Annul the NDF (which will unmap things) */
      ndfAnnul( &(file->ndfid), status );

    } else if( file->fd ) {
      /* Array was mmap'd to a file, and must now be sync'd and unmapped,
         and the file descriptor closed */

      /* Get the size of the header and data section */
      smf_calc_mmapsize( sizeof(*temphead), *data, &headlen, &datalen,
                         &buflen, status );

      /* The header is in the same mapped array as the data array,
         but headlen bytes earlier. Point temphead to this location and
         update relevant header values before closing */

      buf = ((char*)((*data)->pntr[0]) - headlen);
      temphead = (smfDIMMHead *) buf;
      temphead->data.isTordered = (*data)->isTordered;
      temphead->data.dtype = (*data)->dtype;
      temphead->data.ndims = (*data)->ndims;
      memcpy( temphead->data.dims, (*data)->dims, sizeof( temphead->data.dims));

      if( msync( buf, buflen, MS_ASYNC ) == -1 ) {
        *status = SAI__ERROR;
        errRep( ERRFUNC, "Unable to synch model container file", status );
      } else if( munmap( buf, buflen ) == -1 ) {
        *status = SAI__ERROR;
        errRep( ERRFUNC, "Unable to unmap model container file", status );
      } else if( close( file->fd ) == -1 ) {
        *status = SAI__ERROR;
        errRep( ERRFUNC, "Unable to close model container file", status );
      } else {
        file->fd = 0;
      }

    } else {
      /* No file so free the data */
      freedata = 1;
    }

    /* Free the data - even if there is an associated NDF - if it was
       allocated by a smurf function. Note, a value of 1 for "isdyn"
       guarantees the memory was allocated within smurf, but a value of
       zero does not guarantee it was *not* allocated within smurf,
       which is why we still do the above checks */
    if( (*data)->isdyn ) freedata = 1;

    (*data)->file = astFree( (*data)->file );
  } else {
    /* no file - data is ours to free */
    freedata = 1;
  }


  /* Tidy up the header */
  if (hdr != NULL) {
    if (hdr->wcs != NULL) hdr->wcs = astAnnul( hdr->wcs );
    if (hdr->tswcs != NULL) hdr->tswcs = astAnnul( hdr->tswcs );
    if (hdr->fitshdr != NULL) hdr->fitshdr = astAnnul( hdr->fitshdr );

    if( hdr->cache1 ) hdr->cache1 = sc2ast_createwcs2( SC2AST__NULLSUB, NULL, 0.0, VAL__BADD, NULL, NULL, NO_FTS, NULL,
                                                       hdr->cache1, status );
    if( hdr->cache2 ) hdr->cache2 = smf_create_lutwcs( 1, NULL, NULL, 0, NULL, 0.0, VAL__BADD, NULL,
                                                       NULL, NULL, hdr->cache2, status );
    if( hdr->cache3 ) hdr->cache3 = smf_detpos_wcs( NULL, -1, 0.0, VAL__BADD, NULL, NULL, hdr->cache3,
                                                    status );

    if (!hdr->isCloned) {
      /* We are responsible for this memory - although what happens
         when we are cloned and the original is freed first? Need
         to think carefully about memory management. */
      if (hdr->allState != NULL) {
        hdr->allState = astFree( hdr->allState );
      }
      if (hdr->fplanex) hdr->fplanex = astFree( hdr->fplanex );
      if (hdr->fplaney) hdr->fplaney = astFree( hdr->fplaney );
      if (hdr->detpos) hdr->detpos = astFree( hdr->detpos );
      if (hdr->tsys) hdr->tsys = astFree( hdr->tsys );
      if (hdr->detname) hdr->detname = astFree( hdr->detname );
      if (hdr->ocsconfig) hdr->ocsconfig = astFree( hdr->ocsconfig );
    }
    hdr = astFree( hdr );
  }

  /* Now the smfDA itself */
  if ( (*data)->da != NULL ) {
    da = (*data)->da;
    da->flatcal = astFree( da->flatcal );
    da->flatpar = astFree( da->flatpar );
    if( da->dksquid) smf_close_file( wf, &da->dksquid, status );
    da->heatval = astFree( da->heatval );
    da = astFree( da );
  }

  /* Free smfFts */
  if((*data)->fts != NULL) {
    fts = (*data)->fts;
    if(fts->zpd) { smf_close_file(wf, &fts->zpd, status); }
    if(fts->fpm) { smf_close_file(wf, &fts->fpm, status); }
    if(fts->sigma) { smf_close_file(wf, &fts->sigma, status); }
    fts = astFree(fts);
  }

  /* Free smfDream */
  if ( (*data)->dream != NULL ) {
    dream = (*data)->dream;
    if ( dream->gridwts != NULL)
      dream->gridwts = astFree( dream->gridwts );
    if ( dream->invmatx != NULL)
      dream->invmatx = astFree( dream->invmatx );
    dream= astFree( dream );
  }

  /* Free up other pointers in the smfData: */
  if ( (*data)->poly ) {
    (*data)->poly = astFree( (*data)->poly );
  }
  if ( (*data)->lut ) {
    (*data)->lut = astFree( (*data)->lut );
  }
  if( (*data)->theta ) {
    (*data)->theta = astFree( (*data)->theta );
  }

  /* Free the data arrays if they are non-null (they should have been
     freed if they were mapped to a file but not if they were stored
     in a separate action as temp storage */
  if (freedata) {
    for (i = 0; i < 2; i++ ) {
      if ( ((*data)->pntr)[i] != NULL )
        ((*data)->pntr)[i] = astFree( ((*data)->pntr)[i] );
    }
    if ( (*data)->qual ) {
      (*data)->qual = astFree( (*data)->qual );
    }
  }

  /* finally free smfData */
  *data = astFree( *data );

}
