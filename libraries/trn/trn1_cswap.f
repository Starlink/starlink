      SUBROUTINE TRN1_CSWAP( LOC, NAME1, NAME2, STATUS )








*+
*  Name:
*     TRN1_CSWAP

*  Purpose:
*     swap component names in a structure.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_CSWAP( LOC, NAME1, NAME2, STATUS )

*  Description:
*     The routine interchanges the names of two components of a
*     structure (NAME1 and NAME2).  The routine operates even if one
*     component is absent, in which case it simply changes the name of
*     the one which does exist.  If neither component exists, the
*     routine returns without action.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     25-APR-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'DAT_ERR'          ! HDS error codes


*  Arguments Given:
      CHARACTER * ( * ) LOC
                                ! Structure locator
      CHARACTER * ( * ) NAME1
                                ! First component name
      CHARACTER * ( * ) NAME2
                                ! Second component name


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
*     <declarations and descriptions for exported arguments>


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      LOGICAL THERE1            ! Whether the first component exists
      LOGICAL THERE2            ! Whether the second component exists
      INTEGER ITMP              ! Counter for generating temporary
                                ! component names
      INTEGER LTMP              ! Length of temporary component name
      CHARACTER * ( DAT__SZLOC ) LOC1
                                ! Locator to first component
      CHARACTER * ( DAT__SZLOC ) LOC2
                                ! Locator to second component
      CHARACTER * ( DAT__SZNAM ) TMPNAM
                                ! Temporary component name


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   See if the first component exists.
      THERE1 = .FALSE.
      CALL DAT_THERE( LOC, NAME1, THERE1, STATUS )


*   If it exists, get a locator to it.
      IF( THERE1 ) THEN
        CALL DAT_FIND( LOC, NAME1, LOC1, STATUS )


*   If there is no error, mark the error stack and attempt to rename the
*   first component, giving it a temporary name.
        IF( STATUS .EQ. SAI__OK ) THEN
          ITMP = 1
          TMPNAM = 'TRN_TMP_1'
          CALL ERR_MARK
          CALL DAT_RENAM( LOC1, TMPNAM, STATUS )


*   If this failed because there is already a component with the same
*   name, annul the error report and keep trying new names generated
*   from the counter ITMP until successful.
          DO WHILE ( STATUS .EQ. DAT__COMEX )   ! component already
                                                ! exists
            CALL ERR_ANNUL( STATUS )
            ITMP = ITMP + 1
            TMPNAM = 'TRN_TMP_'
            LTMP = 8
            CALL CHR_PUTI( ITMP, TMPNAM, LTMP )
            CALL DAT_RENAM( LOC1, TMPNAM, STATUS )
          ENDDO


*   End of "no error obtaining locator to the first component"
*   condition.  Release the error stack.
          CALL ERR_RLSE
        ENDIF


*   End of "the first component exists" condition.
      ENDIF


*   See if the second component exists.
      THERE2 = .FALSE.
      CALL DAT_THERE( LOC, NAME2, THERE2, STATUS )


*   If it exists, rename it, giving it the name which the first
*   component previously had.
      IF( THERE2 ) THEN
        CALL DAT_FIND( LOC, NAME2, LOC2, STATUS )
        CALL DAT_RENAM( LOC2, NAME1, STATUS )
        CALL DAT_ANNUL( LOC2, STATUS )
      ENDIF


*   Finally, if the first component existed, give it the name which the
*   second component previously had.
      IF( THERE1 ) THEN
        CALL DAT_RENAM( LOC1, NAME2, STATUS )
        CALL DAT_ANNUL( LOC1, STATUS )
      ENDIF


*   Exit routine.
      END
