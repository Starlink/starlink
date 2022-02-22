      LOGICAL FUNCTION NUM_WASOK()
*+
*  Name:
*     NUM_WASOK

*  Purpose:
*     Check current numeric error status

*  Language:
*     Starlink Fortran

*  Invocation:
*     ISOK = NUM_WASOK()

*  Description:
*     Determine whether a numeric operation completed successfully.

*  Returned Value:
*     NUM_WASOK = LOGICAL
*        Returns TRUE if the numeric operation completed successfully.

*  Arguments:
*     STATUS = INTEGER (Given & Returned)
*        Current error status from NUM subsystem. It is not modified
*        if STATUS is not SAI__OK on entry.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     Tim Jenness (JAC, Hawaii)

*  History:
*     1-OCT-2004 (TIMJ):
*        Original version
*     22-FEB-2022 (DSB):
*        Use NUM_TEST function instead of common block

*  Notes:
*     You should clear error status using NUM_CLEARERR before using
*     a NUM routine and using this routine to check status.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Local Variables:
      INTEGER STATUS

*.
      STATUS = SAI__OK
      CALL NUM_GETERR( STATUS )
      NUM_WASOK = ( STATUS .EQ. SAI__OK )

      END
