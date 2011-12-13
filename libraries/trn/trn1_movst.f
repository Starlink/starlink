      SUBROUTINE TRN1_MOVST( STR1, STR2, STATUS )








*+
*  Name:
*     TRN1_MOVST

*  Purpose:
*     move the contents of a structure.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_MOVST( STR1, STR2, STATUS )

*  Description:
*     The routine moves all the components of one structure into a
*     second structure.  Any existing components in the second
*     structure whose names clash with components being moved are first
*     erased.

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
*     25-APR--1988:  Original version (DUVAD::RFWS)
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
      CHARACTER * ( * ) STR1    ! Locator to first structure
      CHARACTER * ( * ) STR2    ! Locator to second structure


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
      LOGICAL THERE             ! Whether a component is present
      INTEGER NCOMP1            ! Number of components in structure 1
      INTEGER NCOMP2            ! Number of components in structure 2
      INTEGER ICOMP             ! Loop counter for components
      CHARACTER * ( DAT__SZLOC ) LOCC
                                ! Locator to structure component
      CHARACTER * ( DAT__SZNAM ) NAME
                                ! Structure component name


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Find the number of components in each structure.
      NCOMP1 = 0
      CALL DAT_NCOMP( STR1, NCOMP1, STATUS )
      NCOMP2 = 0
      CALL DAT_NCOMP( STR2, NCOMP2, STATUS )


*   Repeatedly locate the first component in the first structure
*   and obtain its name.  Do this until all the components have been
*   moved, or an error is detected.
      ICOMP = 0
      DO WHILE ( ( ICOMP .LT. NCOMP1 ) .AND. ( STATUS .EQ. SAI__OK ) )
        ICOMP = ICOMP + 1
        CALL DAT_INDEX( STR1, 1, LOCC, STATUS )
        CALL DAT_NAME( LOCC, NAME, STATUS )


*   See if there is a component with the same name in the second
*   structure.  If so, erase it.
        IF( NCOMP2 .GT. 0 ) THEN
          THERE = .FALSE.
          CALL DAT_THERE( STR2, NAME, THERE, STATUS )
          IF( THERE ) CALL DAT_ERASE( STR2, NAME, STATUS )
        ENDIF


*   Move the component across from the first structure to the second.
         CALL DAT_MOVE( LOCC, STR2, NAME, STATUS )


*   End of "repeatedly locate the first component" loop.
      ENDDO


*   Exit routine.
      END
