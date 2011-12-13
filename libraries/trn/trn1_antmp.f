      SUBROUTINE TRN1_ANTMP( LOC, STATUS )








*+
*  Name:
*     TRN1_ANTMP

*  Purpose:
*     annul locator to temporary object.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_ANTMP( LOC, STATUS )

*  Description:
*     The routine annuls an active locator associated with a temporary
*     HDS object, erasing the object in the process.  This overcomes a
*     deficiency in HDS, which leaves temporary objects in existence
*     when their locators have been annulled.  Note that this routine
*     makes no check that the object is acually temporary - it simply
*     erases it anyway.  The routine attempts to execute even if STATUS
*     is set on entry, although no error report will be made if it fails
*     under these circumstances.

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
*     27-APR-1988:  Original version (DUVAD::RFWS)
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
*     <declarations and descriptions for imported arguments>


*  Arguments Given and Returned:
      CHARACTER * ( * ) LOC     ! Locator to be annulled


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
      INTEGER LSTAT             ! Local status variable
      CHARACTER * ( DAT__SZNAM ) NAME
                                ! Object name
      CHARACTER * ( DAT__SZLOC ) LOCP
                                ! Locator to parent object


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Set the local status value and mark the error stack.
      LSTAT = SAI__OK
      CALL ERR_MARK


*   Find the object name from the locator supplied.
      NAME = ' '
      CALL DAT_NAME( LOC, NAME, LSTAT )


*   Find the parent object from the locator supplied.
      CALL DAT_PAREN( LOC, LOCP, LSTAT )


*   Annul the original locator (this will be attempted even if the above
*   steps failed).
      CALL DAT_ANNUL( LOC, LSTAT )


*   Erase the associated object.
      CALL DAT_ERASE( LOCP, NAME, LSTAT )


*   Annul the locator to the parent object.
      CALL DAT_ANNUL( LOCP, LSTAT )


*   If an error was detected by this routine, but STATUS was set on
*   entry, then annul the error report made by this routine.
      IF( LSTAT .NE. SAI__OK ) THEN
        IF( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( LSTAT )


*   If STATUS was not set, allow the error report to stand and return
*   the local status value.
        ELSE
          STATUS = LSTAT
        ENDIF


*   End of "an error was detected by this routine" condition.
      ENDIF


*   Release the error stack.
      CALL ERR_RLSE


*   Exit routine.
      END
