      SUBROUTINE NDF1_HRST( LOC, STATUS )
*+
*  Name:
*     NDF1_HRST

*  Purpose:
*     Reset an HDS primitive or structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HRST( LOC, STATUS )

*  Description:
*     The routine resets an HDS primitive object or structure. If a
*     structure is supplied, then "resetting" amounts to erasing all its
*     components. If a structure array is supplied, then all components
*     in all of its elements are erased.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to HDS object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If a structure array is supplied, then it should be capable of
*     being vectorised using DAT_VEC.

*  Algorithm:
*     -  Determine if the object is primitive.
*     -  If so, then reset its value(s).
*     -  If not primitive, then vectorise the object (it may be a
*     structure array) and determine how many elements it has.
*     -  Loop to empty each element in the structure (array). Obtain a
*     locator to each cell in turn.
*     -  Determine how many components the structure cell has.
*     -  Loop to erase each component. Obtain a locator to the first
*     component and determine its name.
*     -  Annul the locator, erase the component and return to process
*     the next component.
*     -  When all cell components have been erased, annul the locator
*     to the structure (array) cell and return to process the next cell.
*     -  When all cells have been processed, annul the locator to the
*     vectorised object.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-MAR-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Arguments Given:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCCEL ! Locator to cell
      CHARACTER * ( DAT__SZLOC ) LOCCMP ! Locator to cell component
      CHARACTER * ( DAT__SZLOC ) LOCVEC ! Locator to vectorised object
      CHARACTER * ( DAT__SZNAM ) NAME ! Component name
      INTEGER CELL( 1 )          ! Cell subscript array
      INTEGER EL                 ! Number of array elements
      INTEGER I                  ! Loop counter for cell components
      INTEGER ICELL              ! Loop counter for array cells
      INTEGER NCOMP              ! Number of cell components
      LOGICAL PRIM               ! Is object primitive?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine if the object is primitive.
      CALL DAT_PRIM( LOC, PRIM, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then reset its value(s).
         IF ( PRIM ) THEN
            CALL DAT_RESET( LOC, STATUS )

*  If not primitive, then vectorise the object (it may be a structure
*  array) and determine how many elements it has.
         ELSE
            CALL DAT_VEC( LOC, LOCVEC, STATUS )
            CALL DAT_SIZE( LOCVEC, EL, STATUS )

*  Loop to empty each element in the structure (array). Obtain a locator
*  to each cell in turn.
            IF ( STATUS .EQ. SAI__OK ) THEN
               DO 2 ICELL = 1, EL
                  CELL( 1 ) = ICELL
                  CALL DAT_CELL( LOCVEC, 1, CELL, LOCCEL, STATUS )

*  Determine how many components the structure cell has.
                  CALL DAT_NCOMP( LOCCEL, NCOMP, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop to erase each component. Obtain a locator to the first component
*  and determine its name.
                     DO 1 I = 1, NCOMP
                        CALL DAT_INDEX( LOCCEL, 1, LOCCMP, STATUS )
                        CALL DAT_NAME( LOCCMP, NAME, STATUS )

*  Annul the locator and erase the component.
                        CALL DAT_ANNUL( LOCCMP, STATUS )
                        CALL DAT_ERASE( LOCCEL, NAME, STATUS )
1                    CONTINUE
                  END IF

*  Annul the locator to the structure (array) cell.
                  CALL DAT_ANNUL( LOCCEL, STATUS )
2              CONTINUE
            END IF

*  Annul the locator to the vectorised object.
            CALL DAT_ANNUL( LOCVEC, STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_HRST', STATUS )

      END
