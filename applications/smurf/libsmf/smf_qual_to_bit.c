/*
*+
*  Name:
*     smf_qual_to_bit

*  Purpose:
*     Convert a quality value to a 0-indexed bit number

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     int smf_qual_to_bit( smf_qual_t q, int * status );

*  Arguments:
*     q = smf_qual_t (Given)
*        Quality value to test.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     The bit number. -1 if no bits are set. Returns 0 if status
*     is bad on entry (so that it is safe to use the return value
*     as an array index even if status is bad but you know the bit
*     would be defined because a constant is passed in).

*  Description:
*     Converts a quality value (e.g. SMF__Q_BADB) to the corresponding
*     bit number. The first bit is at position zero. If multiple bits
*     are set this function will return the position of the first.

*  Authors:
*     EC: Ed Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     See also smf_qual_str.

*  History:
*     2010-06-16 (TIMJ):
*        Initial version. Refactored from smf_iteratemap
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 University of British Columbia.
*     Copyright (C) 2010 Science and Technology Facilities Council.
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

#include "smf.h"
#include "sae_par.h"

int smf_qual_to_bit(smf_qual_t q, int *status) {
  int num=-1;
  if (*status != SAI__OK) return 0;
  if( q ) {
    while( !((q>>++num)&1) ) ;
  }
  return num;
}
