      SUBROUTINE TRN1_WRLOG( LOC, NAME, DFLT, VAL, STATUS )








*+
*  Name:
*     TRN1_WRLOG

*  Purpose:
*     write a logical value to a structure component.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_WRLOG( LOC, NAME, DFLT, VAL, STATUS )

*  Description:
*     The routine creates (if necessary) and sets a logical value for a
*     scalar component of a structure.  Any existing component with
*     the same name is over-written or erased.  A default value is
*     specified, so that if the required value equals this default,
*     then no object will actually be created.  In this case, the
*     absence of the object implies the required value.

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
*     29-MAR-1988:  Original version (DUVAD::RFWS)
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
      CHARACTER * ( * ) LOC     ! Locator to enclosing structure
      CHARACTER * ( * ) NAME    ! Component name
      LOGICAL DFLT              ! The default value
      LOGICAL VAL               ! Logical value to set


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
      LOGICAL THERE             ! Whether the component exists


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   If the required value is the default, then see if the named
*   component already exists.
      IF( VAL .EQV. DFLT ) THEN
        THERE = .FALSE.
        CALL DAT_THERE( LOC, NAME, THERE, STATUS )


*   If it exists, then erase it.
        IF( THERE ) CALL DAT_ERASE( LOC, NAME, STATUS )


*   If the value is not the default, then ensure a _LOGICAL component
*   with the required name is present.
      ELSE
        CALL CMP_MOD( LOC, NAME, '_LOGICAL', 0, 0, STATUS )


*   Set the logical value.
        CALL CMP_PUT0L( LOC, NAME, VAL, STATUS )
      ENDIF


*   Exit routine.
      END
