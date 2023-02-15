/*
*+
*  Name:
*     smf_calc_mmapsize

*  Purpose:
*     Calculate the size of the header and data chunk for mmapped data file

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calc_mmapsize( dim_t headsize, const smfData * data,
*                        dim_t * headlen, dim_t *datalen,
*                        dim_t * buflen, int * status );

*  Arguments:
*     headsize = dim_t (Given)
*        Minimum size of header chunk. Will be rounded up to the nearest
*        page.
*     data = const smfData * (Given)
*        smfData to analyze to determine size of data chunk.
*     headlen = dim_t * (Returned)
*        Required size of header that matches the pagesize.
*     datalen = dim_t * (Returned)
*        Required size of data segment.
*     buflen = dim_t * (Returned)
*        Size of full buffer (datalen + headlen)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Calculates the header size based on pagesize boundaries and calculates
*     the required data size based on the dimensionality and data type of
*     the supplied smfData.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Attempts to free resources even if status is bad on entry.

*  History:
*     2009-10-13 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
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

#include "sae_par.h"

#include "smf.h"

void smf_calc_mmapsize( dim_t headsize, const smfData * data,
                        dim_t *headlen, dim_t *datalen,
                        dim_t *buflen, int * status ) {
  dim_t pagesize;
  dim_t remainder;
  dim_t ndata;
  dim_t i;

  *datalen = 0;
  *headlen = 0;
  *buflen = 0;

  if (*status != SAI__OK) return;
  if (!smf_validate_smfData(data, 0, 0, status)) return;

  /* Header must fit into integer multiple of pagesize so that the data
     array starts on a page boundary */
  (void)smf_get_freemem( NULL, &pagesize, NULL, status );
  if (*status != SAI__OK) return;

  remainder = headsize % pagesize;
  if( remainder ) *headlen = headsize - remainder + pagesize;

  /* Length of data array buffer */
  ndata = 1;
  for( i=0; i < data->ndims; i++ ) {
    ndata *= (dim_t) (data->dims)[i];
  }
  *datalen = ndata * smf_dtype_sz(data->dtype,status);

  /* Total size */
  *buflen = *datalen + *headlen;

  return;
}
