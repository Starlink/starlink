/*
*+
*  Name:
*     smf_sort_bydouble

*  Purpose:
*     Sort array of smfSortInfo structs by the sortval

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine (QSORT callback)

*  Invocation:
*     qsort( smfSortInfo* tobeSorted, dim_t nel, sizeof(*tobeSorted),
*            smf_sort_bydouble );

*  Description:
*     This routine is a sort comparison function that can be called
*     from qsort() to sort an array of smfSortInfo structures into
*     order based on a double precision value. It should not be called directly.

*  Notes:
*     - The sortkey item (a double) of the structure must be filled in.
*     - Only use with smfSortInfo arrays.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2009-09-25 (TIMJ):
*        Initial version. Moved from smf_find_darks
*     2011-01-25 (TIMJ):
*        Change name of sort item in struct.

*  Copyright:
*     Copyright (C) 2009, 2011 Science and Technology Facilities Council.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include "smf_typ.h"
#include "smf.h"

int smf_sort_bydouble ( const void *in1, const void *in2 ) {
  const smfSortInfo * sort1;
  const smfSortInfo * sort2;
  double sortval1;
  double sortval2;

  sort1 = in1;
  sort2 = in2;

  sortval1 = sort1->sortval;
  sortval2 = sort2->sortval;

  if (sortval1 < sortval2) {
    return -1;
  } else if (sortval1 > sortval2) {
    return 1;
  } else {
    return 0;
  }
}

