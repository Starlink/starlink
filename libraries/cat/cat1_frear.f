      SUBROUTINE CAT1_FREAR (PTR, STATUS)
*+
*  Name:
*     CAT1_FREAR
*  Purpose:
*     Wrap-around to PSX routine for releasing a dynamic array.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_FREAR (PTR; STATUS)
*  Description:
*     Wrap-around to PSX routine for releasing a dynamic array.
*  Arguments:
*     PTR  =  INTEGER (Given)
*        Pointer to the array.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Call the appropriate PSX routine.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     14/4/94 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'          ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  PTR
*  Status:
      INTEGER STATUS             ! Global status.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         CALL PSX_FREE (PTR, STATUS)

      END IF

      END
