/*
*+
*  Name:
*     smf_sort_bytime

*  Purpose:
*     Sort array of smfSortInfo structs by time

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine (QSORT callback)

*  Invocation:
*     qsort( smfSortInfo* tobeSorted, size_t nel, sizeof(*tobeSorted),
*            smf_sort_bytime );

*  Description:
*     This routine is a sort comparison function that can be called
*     from qsort() to sort an array of smfSortInfo structures into
*     time order. It should not be called directly.

*  Notes:
*     - The mjd item of the structure must be filled in.
*     - Only use with smfSortInfo arrays.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2009-09-25 (TIMJ):
*        Initial version. Moved from smf_find_darks

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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include "smf_typ.h"
#include "smf.h"

int smf_sort_bytime ( const void *in1, const void *in2 ) {
  const smfSortInfo * sort1;
  const smfSortInfo * sort2;
  double mjd1;
  double mjd2;

  sort1 = in1;
  sort2 = in2;

  mjd1 = sort1->mjd;
  mjd2 = sort2->mjd;

  if (mjd1 < mjd2) {
    return -1;
  } else if (mjd1 > mjd2) {
    return 1;
  } else {
    /* least likely case last */
    return 0;
  }
}

