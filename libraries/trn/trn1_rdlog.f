      SUBROUTINE TRN1_RDLOG( LOC, NAME, DFLT, VAL, STATUS )








*+
*  Name:
*     TRN1_RDLOG

*  Purpose:
*     read a logical value from a structure component.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_RDLOG( LOC, NAME, DFLT, VAL, STATUS )

*  Description:
*     The routine reads a logical value from a scalar component of a
*     structure (type conversion will be performed if appropriate).
*     A default value is specified which applies if the component is
*     absent.

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
*     5-DEC-1988:  Re-written to improve efficiency (DUVAD::RFWS)
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
      CHARACTER * ( * ) LOC     ! Locator to enclosing structure
      CHARACTER * ( * ) NAME    ! Component name
      LOGICAL DFLT              ! The default value


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      LOGICAL VAL               ! Logical value


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      LOGICAL THERE             ! Whether the component is present


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   See if the required component is present.
      THERE = .FALSE.
      CALL DAT_THERE( LOC, NAME, THERE, STATUS )


*   If it is there, read the logical value.
      IF( THERE ) THEN
        CALL CMP_GET0L( LOC, NAME, VAL, STATUS )


*   Otherwise, use the default value.
      ELSE
        VAL = DFLT
      ENDIF


*   Exit routine.
      END
