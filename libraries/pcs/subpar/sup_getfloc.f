      SUBROUTINE SUBPAR_GETFLOC ( NAMECODE, VALID, LOC, STATUS )
*+
*  Name:
*     SUBPAR_GETFLOC

*  Purpose:
*     Gets the top-level HDS locator of a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_GETFLOC ( NAMECODE, VALID, LOC, STATUS )

*  Description:
*     The storage for the locators of the indicated parameter is
*     inspected, and if a valid locator is present it is returned.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to the parameter
*     VALID=LOGICAL (returned)
*        .TRUE. => there is a valid HDS locator associated with the
*        parameter
*     LOC=CHARACTER*(DAT__SZLOC) (returned)
*        the value of the locator associated with HDS container file
*        for the parameter. This is the 'top-level' locator as opposed
*        to the one associated with the parameter value.
*     STATUS=INTEGER

*  Algorithm:
*     NAMECODE indexes into arrays holding a logical flag for the
*     validity of the locator and the locator value. Note the actual
*     locator value is returned rather than a 'clone'.

*  Copyright:
*     Copyright (C) 1984, 1991, 1993 Science & Engineering Research Council.
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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1984 (BDK):
*        Original
*     07-JUN-1991 (AJC):
*        Correct name in banner and invocation
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


*  Arguments Returned:
      LOGICAL VALID             ! .TRUE. => there is a valid HDS
                                ! locator associated with the parameter

      CHARACTER*(DAT__SZLOC) LOC  ! the value of the locator associated
                                  ! with the HDS container file.


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Check that NAMECODE is in range, and return the values.
*
      IF ( ( NAMECODE .LE. PARPTR ) .AND. ( NAMECODE .GT. 0 ) ) THEN

         VALID = PARVALID(NAMECODE)
         LOC = PARLOC(1,NAMECODE)

      ELSE

         STATUS = SUBPAR__NOPAR
         VALID = .FALSE.

      ENDIF

      END
