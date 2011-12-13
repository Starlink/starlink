      SUBROUTINE TRN_JOIN( LOCTR1, LOCTR2, ELOC, NAME, LOCTR, STATUS )







*+
*  Name:
*     TRN_JOIN

*  Purpose:
*     join transformations.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_JOIN( LOCTR1, LOCTR2, ELOC, NAME, LOCTR, STATUS )

*  Description:
*     The routine joins two transformation structures passed by HDS
*     locator, concatenating them to produce a combined transformation
*     which is stored as a new component of an existing HDS structure.
*     If the enclosing structure locator (ELOC) is supplied as a blank
*     character string, then a temporary object will be created to hold
*     the new transformation instead.  In this latter case, the NAME
*     argument is not used and may also be blank.

*  Arguments:
*     LOCTR1 = CHARACTER * ( * ) (given)
*        HDS locator to first transformation structure.
*     LOCTR2 = CHARACTER * ( * ) (given)
*        HDS locator to second transformation structure.
*     ELOC = CHARACTER * ( * ) (given)
*        HDS locator to an enclosing structure to contain the new
*        object (or a blank string if a temporary object is
*        required).
*     NAME = CHARACTER * ( * ) (given)
*        HDS name of the structure component to contain the new
*        (concatenated) transformation structure.
*     LOCTR = CHARACTER * ( * ) (returned)
*        HDS locator to the new transformation structure.
*     STATUS = INTEGER (given & returned)
*        Inherited error status.

*  Algorithm:
*     - Validate the two transformation structures supplied.
*     - Copy the first transformation structure into the output
*       structure, or (if required) create a temporary structure and
*       copy it into that.
*     - Call TRN_APND to append the second transformation structure.
*     - If an error occurred, attempt to clean up by erasing any new
*       structure which was created.

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
*     18-AUG-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants


*  Arguments Given:
      CHARACTER * ( * ) LOCTR1  ! Locator to first transformation

      CHARACTER * ( * ) LOCTR2  ! Locator to second transformation

      CHARACTER * ( * ) ELOC    ! Locator to enclosing structure to
                                ! contain the new object

      CHARACTER * ( * ) NAME    ! Name of new component to contain the
                                ! joined transformations


*  Arguments Returned:
      CHARACTER * ( * ) LOCTR   ! Locator to new joined transformation


*  Status:
      INTEGER STATUS            ! Error status


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Validate the two transformations.
      CALL TRN1_VTR( LOCTR1, STATUS )
      CALL TRN1_VTR( LOCTR2, STATUS )


*   If ELOC is not a blank character string, copy the first
*   transformation directly into the new component and obtain a locator
*   to it.
      IF( ELOC .NE. ' ' ) THEN
        CALL DAT_COPY( LOCTR1, ELOC, NAME, STATUS )
        CALL DAT_FIND( ELOC, NAME, LOCTR, STATUS )


*   Otherwise, create a temporary structure to contain the joined
*   transformations and copy the contents of the first transformation
*   structure into it.
      ELSE
        CALL TRN1_TEMP( 'TRN_TRANSFORM', 0, 0, LOCTR, STATUS )
        CALL TRN1_CPYST( LOCTR1, LOCTR, STATUS )
      ENDIF


*   Append the second transformation.
      CALL TRN_APND( LOCTR, LOCTR2, STATUS )


*   If an error was detected, attempt to erase the structure which was
*   created.
      IF( STATUS .NE. SAI__OK ) CALL TRN1_ANTMP( LOCTR, STATUS )


*   Exit routine.
      END
