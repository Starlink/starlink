      SUBROUTINE TRN1_UPVSN( LOCTR, STATUS )








*+
*  Name:
*     TRN1_UPVSN

*  Purpose:
*     update the software version number for a transformation.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_UPVSN( LOCTR, STATUS )

*  Description:
*     The routine updates the software version number in a
*     transformation structure which has been altered.  If the
*     TRN_VERSION object has a value less than the version number of the
*     current software, then it is updated to the current value.  The
*     transformation structure is passed by HDS locator, but this is not
*     validated before use.

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
*     20-MAY-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_PAR'          ! TRN_ public constants


*  Arguments Given:
      CHARACTER * ( * ) LOCTR   ! Locator to transformation structure


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
      REAL VERSN                ! Software version number of
                                ! transformation


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Obtain the software version number from the transformation.
      CALL CMP_GET0R( LOCTR, 'TRN_VERSION', VERSN, STATUS )


*   If there is no error and the version number is out of date, then
*   ensure the TRN_VERSION component is of the correct type and shape
*   and enter a new value.
      IF( STATUS .EQ. SAI__OK ) THEN
        IF( VERSN .LT. TRN__VERSN ) THEN
          CALL CMP_MOD( LOCTR, 'TRN_VERSION', '_REAL', 0, 0, STATUS )
          CALL CMP_PUT0R( LOCTR, 'TRN_VERSION', TRN__VERSN, STATUS )
        ENDIF
      ENDIF


*   Exit routine.
      END
