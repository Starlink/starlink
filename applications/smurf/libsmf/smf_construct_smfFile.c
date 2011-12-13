/*
*+
*  Name:
*     smf_construct_smfFile

*  Purpose:
*     (Optionally) Create and Populate a smfFile structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     file = smf_construct_smfFile( smfFile * tofill, int ndfid,int isSc2store,
*                 int isTstream, const char * name, HDSLoc* xloc,
*                 int * status );

*  Arguments:
*     tofill = smfFile* (Given)
*        If non-null, the remaining arguments to this function
*        are assigned to this struct and no struct is malloced.
*        This can be used to fill a previously malloced struct.
*     ndfid = int (Given)
*        NDF identifier. Will be stored directly and not cloned.
*     isSc2store = int (Given)
*        True if the file is under "sc2store" control.
*     isTstream = int (Given)
*        True if ths file is time series data.
*     name = const char * (Given)
*        File name. Can be NULL. Contents are copied.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_create_smfFile = smfFile*
*        Pointer to newly created and populated smfFile. NULL on malloc error.
*        If "tofill" was non-NULL on entry, the pointer returned is the
*        pointer to this struct. This pointer will be returned even if
*        status is bad.

*  Description:
*     This function allocates memory for a smfFile structure and
*     all the internal structures. The structure is initialised
*     with the supplied values.

*  Notes:
*     - If the smfFile is supplied by the caller, make sure that
*       there are no active File resources that need freeing first. They
*       will be overwritten.
*     - Free this memory using smf_close_file, via a smfData structure.
*     - Can be freed with a smf_free if file resources are freed first.
*     - See also smf_create_smfFile

*  Authors:
*     Tim Jenness (TIMJ)
*     {enter_new_authors_here}

*  History:
*     2006-01-26 (TIMJ):
*        Initial version.
*     2006-01-27 (TIMJ):
*        No longer have smfFile.xloc
*     2008-07-16 (TIMJ):
*        Use one_strlcpy.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
#include "star/one.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_construct_smfFile"

smfFile *
smf_construct_smfFile(smfFile * tofill, int ndfid, int isSc2store,
                      int isTstream, const char * name,
                      int * status ) {

  smfFile * file;

  file = tofill;
  if (*status != SAI__OK) return file;

  if (tofill == NULL) {
    file = smf_create_smfFile( status );
  }

  if (*status == SAI__OK) {

    file->ndfid = ndfid;
    file->isSc2store = isSc2store;
    file->isTstream = isTstream;
    if (name != NULL) {
      one_strlcpy( file->name, name, sizeof(file->name), status);
    } else {
      (file->name)[0] = '\0';
    }
  }
  return file;
}
