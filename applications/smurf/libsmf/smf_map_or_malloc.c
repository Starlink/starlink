/*
*+
*  Name:
*     smf_map_or_malloc

*  Purpose:
*     Allocate memory either by malloc or ndfMap

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_map_or_malloc( dim_t nelem, smf_dtype type, int zero,
*                                int indf, const char * comp, int * status );

*  Arguments:
*     nelem = dim_t (Given)
*        Number of elements to allocate
*     type = smf_dtype (Given)
*        Data type to map.
*     zero = int (Given)
*        True if the data should be zeroed explicitly. Not needed if you
*        are about to copy data into this buffer.
*     indf = int (Given)
*        NDF identifier. Can be NDF__NOID if malloc is to be used.
*     comp = const char * (Given)
*        If using ndfMap, name of component to map.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_map_or_malloc = void*
*        Pointer. NULL on error.

*  Description:
*     Helper routine that will call ndfMap if supplied with an NDF identifier
*     or else call astCalloc to get the memory. If ndfMap is used status
*     will be set to bad if the number of mapped points are fewer than
*     that requested in the call. The NDF will not be resized, it is assumed
*     that this routine is being used to obtain a variance or quality
*     component either by mapping or by malloc. "WRITE" access will be
*     used.

*  Authors:
*     Tim Jenness
*     {enter_new_authors_here}

*  History:
*     2008-07-16 (TIMJ):
*        Initial version.
*     {enter_further_changes_here}

*  Notes:
*     - Does not support model "mmap"
*     - Used as helper routine for smf_check_smfData

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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

/* System includes */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

void * smf_map_or_malloc (dim_t nelem, smf_dtype type, int zero, int indf,
                          const char * comp, int *status ) {

  void *pntr[3];     /* ndfMap pointers */
  size_t nout = 0;   /* number of elements mapped */

  if (*status != SAI__OK) return NULL;

  /* just malloc if we do not have a file */
  if ( indf == NDF__NOID) {
     if( zero ) {
       return astCalloc( nelem, smf_dtype_sz(type, status) );
     } else {
       return astMalloc( nelem*smf_dtype_sz(type, status) );
     }
  }

  ndfMap( indf, comp, smf_dtype_str(type, status),
          (zero ? "WRITE/ZERO" : "WRITE"), pntr, &nout, status);

  if (nelem != (dim_t)nout && *status == SAI__OK) {
    ndfUnmap( indf, comp, status );
    *status = SAI__ERROR;
    msgSetc( "COMP", comp );
    msgSetk( "ORI", nelem );
    msgSetk( "NOUT", nout );
    errRep(" ", "Mapping ^COMP in NDF but size differs from that listed in smfData attributes (^ORI != ^NOUT)", status);
    pntr[0] = NULL;
  }
  return pntr[0];
}
