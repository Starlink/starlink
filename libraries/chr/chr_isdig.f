      LOGICAL FUNCTION CHR_ISDIG( CVALUE )
*+
*  Name:
*     CHR_ISDIG

*  Purpose:
*     Return whether a character is a digit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_ISDIG( CVALUE )

*  Description:
*     Determine whether the given character is a digit, i.e. 0 - 9.

*  Arguments:
*     CVALUE = CHARACTER (Given)
*        The character to be tested.

*  Returned Value:
*     CHR_ISDIG = LOGICAL
*        Returns .TRUE. if the given character is a digit, returns
*        .FALSE. otherwise.

*  Algorithm:
*     Check the ASCII value of a given character against the range of
*     ASCII values for digits using the LLE and LGE intrinsic functions.

*  Copyright:
*     Copyright (C) 1984, 1988, 1990, 1991 Science & Engineering Research Council.
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
*     ASOC5: Dave Baines (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     DLT: D.L. Terrett (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-JUN-1984 (ASOC5):
*        Original version.
*     2-SEP-1988 (AJC):
*        Remove INCLUDE 'SAE_PAR'.
*     26-OCT-1988 (AJC):
*        Improve documentation.
*     25-JAN-1990 (DLT):
*        Use local variable instead of function name
*        in tests because of DECstation compiler bugs.
*     5-FEB-1991 (PCTR):
*        New code to conform more closely to Fortran 77.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implict typing

*  Arguments Given:
      CHARACTER CVALUE

*.

*  Perform tests.
      CHR_ISDIG = ( LGE( CVALUE, '0' ) .AND. LLE( CVALUE, '9' ) )

      END
