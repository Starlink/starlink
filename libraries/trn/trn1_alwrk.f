      SUBROUTINE TRN1_ALWRK( NWRK, PWRKI, PWRKR, PWRKD, LWRK, STATUS )








*+
*  Name:
*     TRN1_ALWRK

*  Purpose:
*     allocate workspace.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_ALWRK( NWRK, PWRKI, PWRKR, PWRKD, LWRK, STATUS )

*  Description:
*     The routine allocates workspace by creating a temporary structure
*     containing primitive arrays of each data type, which it then maps.
*     Pointers to the mapped arrays are returned.  The associated
*     locators are stored in the LOCATORS object, a (1-dimensional)
*     character array in the same structure - this may be read to
*     obtain the locators required to un-map the arrays when
*     de-allocating the workspace (TRN1_RELTS performs this task).

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
*     9-MAY-1988:  Original version (DUVAD::RFWS)
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
      INTEGER NWRK              ! Number of workspace elements required
                                ! for each data type


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      INTEGER PWRKI             ! Pointer to mapped _INTEGER array
      INTEGER PWRKR             ! Pointer to mapped _REAL array
      INTEGER PWRKD             ! Pointer to mapped _DOUBLE array
      CHARACTER * ( * ) LWRK    ! Locator to temporary structure
                                ! containing the workspace arrays


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
      INTEGER L_NITEM           ! Number of elements in active locator
      PARAMETER ( L_NITEM = 3 ) ! list
      INTEGER L_I               ! Location of locator to _INTEGER object
      PARAMETER ( L_I = 1 )
      INTEGER L_R               ! Location of locator to _REAL object
      PARAMETER ( L_R = 2 )
      INTEGER L_D               ! Location of locator to _DOUBLE object
      PARAMETER ( L_D = 3 )


*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCLST( L_NITEM )
                                ! List of active locators


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Create a temporary structure to contain the workspace.
      CALL TRN1_TEMP( 'TRN_WORKSPACE', 0, 0, LWRK, STATUS )


*   Create primitive arrays of each data type inside it.
      CALL DAT_NEW( LWRK, 'INTEGER_WRK', '_INTEGER', 1, NWRK,
     :                 STATUS )
      CALL DAT_NEW( LWRK, 'REAL_WRK', '_REAL', 1, NWRK, STATUS )
      CALL DAT_NEW( LWRK, 'DOUBLE_WRK', '_DOUBLE', 1, NWRK, STATUS )


*   Create a character array component called LOCATORS in the same
*   structure to contain the active locators associated with the objects
*   above.
      CALL DAT_NEWC( LWRK, 'LOCATORS', DAT__SZLOC, 1, L_NITEM,
     :                  STATUS )


*   Get locators to the workspace arrays, storing them in the
*   appropriate elements of the LOCLST array.
      CALL DAT_FIND( LWRK, 'INTEGER_WRK', LOCLST( L_I ), STATUS )
      CALL DAT_FIND( LWRK, 'REAL_WRK', LOCLST( L_R ), STATUS )
      CALL DAT_FIND( LWRK, 'DOUBLE_WRK', LOCLST( L_D ), STATUS )


*   Map the workspace arrays, returning the pointers via the routine
*   arguments.
      CALL DAT_MAP( LOCLST( L_I ), '_INTEGER', 'WRITE', 1, NWRK,
     :                 PWRKI, STATUS )
      CALL DAT_MAP( LOCLST( L_R ), '_REAL', 'WRITE', 1, NWRK,
     :                 PWRKR, STATUS )
      CALL DAT_MAP( LOCLST( L_D ), '_DOUBLE', 'WRITE', 1, NWRK,
     :                 PWRKD, STATUS )


*   Store the list of active locators in the LOCATORS component.
      CALL CMP_PUT1C( LWRK, 'LOCATORS', L_NITEM, LOCLST, STATUS )


*   Exit routine.
      END
