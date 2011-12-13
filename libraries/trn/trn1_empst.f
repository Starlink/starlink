      SUBROUTINE TRN1_EMPST( STR, STATUS )








*+
*  Name:
*     TRN1_EMPST

*  Purpose:
*     empty a structure.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_EMPST( STR, STATUS )

*  Description:
*     The routine empties an HDS structure by erasing all its
*     components.  The routine attempts to execute even if STATUS is set
*     on entry, although no error report will be made if it fails under
*     these circumstances.  If more than one error is detected by this
*     routine (and STATUS was not set on entry), then a report will
*     only be made for the first error.

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
*     11-FEB-1988:  Original version (DUVAD::RFWS)
*     16-FEB-1988:  Improved error handling (DUVAD::RFWS)
*     12-MAY-1988:  Improved error reporting (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants


*  Arguments Given:
      CHARACTER * ( * ) STR     ! Locator to structure to be emptied


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
      INTEGER NCOMP             ! Number of structure components
      INTEGER ICOMP             ! Structure component counter
      INTEGER LSTAT             ! Local status variable
      INTEGER CSTAT             ! Error status when erasing a structure
                                ! component
      CHARACTER * ( DAT__SZLOC ) LOCC
                                ! Locator to structure component
      CHARACTER * ( DAT__SZNAM ) NAME
                                ! Structure component name


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Initialise the local status variable and mark the error stack.
      LSTAT = SAI__OK
      CALL ERR_MARK


*   Find the number of structure components.
      NCOMP = 0
      CALL DAT_NCOMP( STR, NCOMP, LSTAT )


*   Loop to delete all the components in reverse order.
      DO ICOMP = NCOMP, 1, -1


*   Initialise the component status variable and mark the error stack
*   again.
        CSTAT = SAI__OK
        CALL ERR_MARK


*   Get the name of the component to be erased.
        CALL DAT_INDEX( STR, ICOMP, LOCC, CSTAT )
        CALL DAT_NAME( LOCC, NAME, CSTAT)
        CALL DAT_ANNUL( LOCC, CSTAT )


*   Erase the component.
        CALL DAT_ERASE( STR, NAME, CSTAT )


*   If an error was detected but LSTAT has already been set, then annul
*   the error report.
        IF( CSTAT .NE. SAI__OK ) THEN
          IF( LSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( CSTAT )


*   Otherwise, record the error status in LSTAT and allow the error
*   report to stand.
          ELSE
            LSTAT = CSTAT
          ENDIF
        ENDIF


*   Release the error stack.
        CALL ERR_RLSE


*   End of "loop to delete all components" loop.
      ENDDO


*   If an error was detected erasing any of the components, then LSTAT
*   will be set.  However, if STATUS was set on entry, then ignore this
*   and annul the associated error report.
      IF( LSTAT .NE. SAI__OK ) THEN
        IF( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( LSTAT )


*   Otherwise, return the local status value and allow the error
*   report to stand.
        ELSE
          STATUS = LSTAT
        ENDIF
      ENDIF


*   Release the error stack.
      CALL ERR_RLSE


*   Exit routine.
      END
