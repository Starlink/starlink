/*
*+
*  Name:
*     smf_find_subarray

*  Purpose:
*     Determine the subarray name and number

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_find_subarray( const smfHead * hdr, int * seqcount, int *status );

*  Arguments:
*     hdr = const smfHead * (Given)
*        Header from which to obtain the FITS information
*     seqcount = int* (Returned)
*        Pointer to int to contain the sequence count.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function reads the FITS header to determine the sequence count
*     within the observation. It is a simple wrapper around the fits retrieval.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-11-14 (TIMJ):
*        Initial version.

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

/* Starlink includes */
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_find_seqcount"

void smf_find_seqcount ( const smfHead * hdr,
                         int *seqcount, int *status ) {
  *seqcount = 0;
  if (*status != SAI__OK) return;
  smf_fits_getI( hdr, "SEQCOUNT", seqcount, status );
  return;
}
