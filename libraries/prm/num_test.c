#include "f77.h"
#include "prm.h"
#include <fenv.h>

F77_LOGICAL_FUNCTION(num_test)( void ) {
/*
*+
*  Name:
*     NUM_TEST

*  Purpose:
*     Test if a floating point error has occurred

*  Language:
*     C99

*  Invocation:
*     RESULT = NUM_TEST()

*  Description:
*     This routine returnes .TRUE. if a floating point error has occurred
*     since it was last called. If so, it clears the floating pointer
*     error status.

*  Copyright:
*     Copyright (C) 2022 East Asian Observatory
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     22-FEB-2022 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/

   F77_LOGICAL_TYPE(RESULT);
   if( fetestexcept( PRM__FPEXCEPTS ) ) {
      RESULT = F77_TRUE;
      feclearexcept( PRM__FPEXCEPTS );
   } else {
      RESULT = F77_FALSE;
   }
   return RESULT;
}

