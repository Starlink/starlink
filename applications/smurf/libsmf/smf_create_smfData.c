/*
*+
*  Name:
*     smf_create_smfData

*  Purpose:
*     Allocate a smfData structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_create_smfData( int flags, int * status );

*  Arguments:
*     flags = int (Given)
*        Bit flags to control which parts of the struct are initialised.
*        See the Notes section for more information.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_create_smfData = smfData*
*        Pointer to newly created smfData. NULL on error.

*  Description:
*     This function allocates memory for a smfData structure and
*     all the internal structures that form that basis of a smfData
*     object. This can include smfDA, smfFile and smfHead structures.
*     All components are initialised. If status is bad, a NULL
*     pointer is returned. Default behaviour is to allocate all components.
*     This can be disabled using the Flags parameter.

*  Notes:
*     - The following Flags are defined in smf_typ.h:
*        SMF__NOCREATE_DA:   Do not allocate smfDA
*        SMF__NOCREATE_HEAD: Do not allocate smfHead
*        SMF__NOCREATE_FILE: Do not allocate smfFile
*     - Free this memory using smf_close_file
*     - Data arrays are not populated by this routine. The pointers
*       are set to NULL.
*     - The smfDream is initialized to NULL; it must be created and
*       attached separately.

*  Authors:
*     Tim Jenness (TIMJ)
*     Andy Gibb (UBC)
*     Ed Chapin (UBC)
*     Coskun Oba (COBA, UoL)
*     {enter_new_authors_here}

*  History:
*     2006-01-26 (TIMJ):
*        Initial version
*     2006-02-17 (AGG):
*        Add scanfit polynomial initialization
*     2006-07-01 (AGG):
*        Change args to smf_free
*     2007-06-13 (EC):
*        Initialize data->DIMMbuf and data->DIMMlen
*     2007-06-14 (EC):
*        Moved DIMM file parameters to smfFile
*     2007-09-14 (EC):
*        Initialize isTordered
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2009-09-29 (TIMJ):
*        Initialize pixel origin.
*     2010-06-14 (TIMJ):
*        Initialise new QUAL element
*     2010-06-18 (TIMJ):
*        Initialise qfamily element
*     2010-09-17 (COBA):
*        Add smfFts
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009-2010 Science & Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_create_smfData"

smfData *
smf_create_smfData( int flags, int * status ) {

  /* Need to make sure that any memory we malloc will be freed on error
     so make sure we NULL all pointers first. */
  smfData * data = NULL;   /* Main data struct */
  smfHead * hdr = NULL;    /* Data header */
  smfFile * file = NULL;   /* File components */
  smfDA * da = NULL;       /* Data Acquisition information */
  smfFts* fts = NULL;      /* FTS2 information */
  int i;

  if (*status != SAI__OK) return NULL;

  data = astMalloc( 1*sizeof(smfData) );
  if (! (flags & SMF__NOCREATE_FILE) )
    file = smf_create_smfFile( status );
  if (! (flags & SMF__NOCREATE_HEAD) )
    hdr  = smf_create_smfHead( status );
  if (! (flags & SMF__NOCREATE_DA) )
    da   = smf_create_smfDA( status );
  if(!(flags & SMF__NOCREATE_FTS))
    fts = smf_create_smfFts(status);

  if (*status != SAI__OK) {
    /* Add our own message to the stack */
    errRep(FUNC_NAME, "Unable to allocate memory for smfData structure",
           status);
    goto CLEANUP;
  }

  /* Attach components to smfData */
  data->file = file;
  data->hdr  = hdr;
  data->da   = da;
  data->fts  = fts;
  /* Set the DREAM to NULL as we only create it later */
  data->dream = NULL;

  /* Initialise remainder of smfData */
  data->dtype = SMF__NULL;
  data->refcount = 1;
  data->onmap = 1;
  data->virtual = 0;
  data->ndims = 0;
  data->ncoeff = 0;
  data->poly = NULL;
  data->history = NULL;
  data->isFFT = 0;
  data->isTordered = 1;

  for (i = 0; i < 2; i++ ) {
    (data->pntr)[i] = NULL;
  }
  data->qual = NULL;
  data->isdyn = 0;
  data->qbits = SMF__Q_GOOD;   /* Export all bits to the disk NDF */
  data->sidequal = NULL;
  data->qfamily = SMF__QFAM_NULL;

  for (i = 0; i < NDF__MXDIM; i++ ) {
    (data->dims)[i] = 0;
    (data->lbnd)[i] = 1;
  }

  data->lut = NULL;
  data->theta = NULL;

  return data;

 CLEANUP:
  data = astFree( data );
  file = astFree( file );
  hdr = astFree( hdr );
  da = astFree( da );
  fts = astFree( fts );

  return NULL;
}
