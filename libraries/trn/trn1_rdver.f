      SUBROUTINE TRN1_RDVER( LOCTR, VERSN, STATUS )








*+
*  Name:
*     TRN1_RDVER

*  Purpose:
*     read software version number from a transformation.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_RDVER( LOCTR, VERSN, STATUS )

*  Description:
*     The routine reads the software version number from the TRN_VERSION
*     object in a transformation structure passed by HDS locator.  If
*     the version number is absent, or the transformation has a version
*     above that of the current software, a STATUS value is set and an
*     error is reported.  The transformation structure itself is not
*     validated.

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
*     16-FEB-1988:  Original version (DUVAD::RFWS)
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
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      CHARACTER * ( * ) LOCTR   ! Locator to transformation


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      REAL VERSN                ! Software version number


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
*     <declarations for local variables>


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Read the TRN_VERSION object to obtain the version number of the
*   software which created the transformation.
      CALL CMP_GET0R( LOCTR, 'TRN_VERSION', VERSN, STATUS )


*   If there is no error, check that the version number does not exceed
*   that of the current software.  Report an error if it does.
      IF( STATUS .EQ. SAI__OK ) THEN
        IF( VERSN .GT. TRN__VERSN ) THEN
          STATUS = TRN__VERMM   ! software version mis-match
          CALL TRN1_ERRC( 'TRN1_RDVER', LOCTR, 'TRN_VERSION', STATUS )
        ENDIF


*   End of "no error reading software version number" condition.
      ENDIF


*   Exit routine.
      END
