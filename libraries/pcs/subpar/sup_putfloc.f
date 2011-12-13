      SUBROUTINE SUBPAR_PUTFLOC ( NAMECODE, LOC, STATUS )
*+
*  Name:
*     SUBPAR_PUTFLOC

*  Purpose:
*     Stores the top-level HDS locator of a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_PUTFLOC ( NAMECODE, LOC, STATUS )

*  Description:
*     The given locator is stored in the internal storage for the
*     top-level locator associated with the indicated parameter.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to the parameter
*     LOC=CHARACTER*(DAT__SZLOC) (given)
*        the value of the locator to be associated with the parameter.
*        This is the 'top-level' locator associated with the HDS
*        container file.
*     STATUS=INTEGER

*  Algorithm:
*     NAMECODE indexes into the array for holding the locator value.

*  Copyright:
*     Copyright (C) 1984, 1993 Science & Engineering Research Council.
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
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1984 (BDK):
*        Original
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'


*  Arguments Given:
      INTEGER NAMECODE          ! pointer to the parameter

      CHARACTER*(DAT__SZLOC) LOC  ! the value of the locator to be
                                  ! associated with the parameter.
                                  ! This is the 'top-level' locator
                                  ! associated with the HDS container file.


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Check that NAMECODE is in range, and insert the value.
*
      IF ( ( NAMECODE .LE. PARPTR ) .AND. ( NAMECODE .GT. 0 ) ) THEN

         PARLOC(1,NAMECODE) = LOC

      ELSE

         STATUS = SUBPAR__NOPAR

      ENDIF

      END
