/*
*  Name:
*     ary_dlt.h

*  Purpose:
*     Include file defining things needed by the delta compression
*     storage form.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     19-OCT-2010 (DSB):
*        Original version.
*     {enter_changes_here}
*/


#include "prm_par.h"


/* These are the flag values that are stored within the compressed DATA
   array to indicate how the corresponding uncompressed values are stored:

   - SINGLE_GOOD: The next uncompressed value is the next value to be read
   from the VALUE array.

   - SINGLE_BAD: The next uncompressed value is VAL__BAD, and the one
   after that is the next value to be read from the VALUE array.

   - REPEAT_GOOD: The next "N" uncompressed values are all equal to the next
   value to be read from the VALUE array, where "N" is the next value to
   be read from the REPEAT array.

   - REPEAT_BAD: The next "N" uncompressed values are all VAL__BAD, but
   the "N+1"'th value is good and is equal to the next value to be read
   from the VALUE array. "N" is the next value to be read from the REPEAT
   array.

   - MULTI_GOOD: The next "N" uncompressed values are all single values
   that could not be compressed and should be read from the next "N"
   elements of the VALUE array. "N" is the next value to be read from the
   REPEAT array.

   Any other value in the compressed DATA array is the difference between
   the next uncompressed value and the previous uncompressed value. So
   NEXT = PREVIOUS + DATA. */

#define SINGLE_GOODI NUM__MAXI
#define REPEAT_GOODI (NUM__MAXI-1)
#define REPEAT_BADI (NUM__MAXI-2)
#define SINGLE_BADI (NUM__MAXI-3)
#define MULTI_GOODI (NUM__MAXI-4)

#define SINGLE_GOODW NUM__MAXW
#define REPEAT_GOODW (NUM__MAXW-1)
#define REPEAT_BADW (NUM__MAXW-2)
#define SINGLE_BADW (NUM__MAXW-3)
#define MULTI_GOODW (NUM__MAXW-4)

#define SINGLE_GOODB NUM__MAXB
#define REPEAT_GOODB (NUM__MAXB-1)
#define REPEAT_BADB (NUM__MAXB-2)
#define SINGLE_BADB (NUM__MAXB-3)
#define MULTI_GOODB (NUM__MAXB-4)

/* These constants give the maximum and minimum delta value that can be
   stored in the DATA array for a given compressed data type. */

#define MIN_DELTAI NUM__MINI
#define MAX_DELTAI (NUM__MAXI-5)

#define MIN_DELTAW NUM__MINW
#define MAX_DELTAW (NUM__MAXW-5)

#define MIN_DELTAB NUM__MINB
#define MAX_DELTAB (NUM__MAXB-5)



