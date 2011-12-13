      SUBROUTINE TRN1_GTNV( LOCTR, NVIN, NVOUT, STATUS )








*+
*  Name:
*     TRN1_GTNV

*  Purpose:
*     get numbers of variables from a transformation structure.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_GTNV( LOCTR, NVIN, NVOUT, STATUS )

*  Description:
*     The routine obtains the numbers of input and output variables for
*     a transformation structure passed by HDS locator.  This is not a
*     user-level routine and does not validate the transformation
*     structure, although it does validate the type of the MODULE_ARRAY
*     component and the "numbers of variables" information returned.
*     The routine TRN_GTNV provides a user interface.

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
*     5-FEB-1988:  Original version (DUVAD::RFWS)
*     28-APR-1988:  Modified for use by TRN_GTNV (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      CHARACTER * ( * ) LOCTR   ! Locator to transformation structure


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      INTEGER NVIN              ! Number of input variables
      INTEGER NVOUT             ! Number of output variables


*  Status:
      INTEGER STATUS


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      INTEGER NMOD              ! Number of modules in the
                                ! transformation
      CHARACTER * ( DAT__SZLOC ) LOCMA
                                ! Locator to the MODULE_ARRAY component
      CHARACTER * ( DAT__SZTYP ) TYPE
                                ! HDS type of the MODULE_ARRAY component
      CHARACTER * ( DAT__SZLOC ) LOCC
                                ! Locator to a cell of the MODULE_ARRAY
                                ! component


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Locate the MODULE_ARRAY component and obtain its size and type.
      CALL DAT_FIND( LOCTR, 'MODULE_ARRAY', LOCMA, STATUS )
      CALL DAT_SIZE( LOCMA, NMOD, STATUS )
      CALL DAT_TYPE( LOCMA, TYPE, STATUS )


*   If there is no error, check that the type is valid.  Report an
*   error if it is not.
      IF( STATUS .EQ. SAI__OK ) THEN
        IF( TYPE .NE. 'TRN_MODULE' ) THEN
          STATUS = TRN__TYPIN   ! type invalid
          CALL TRN1_ERRL( 'TRN1_GTNV', LOCMA, STATUS )


*   If the type is valid, get a locator to the first cell of the array.
        ELSE
          CALL DAT_CELL( LOCMA, 1, 1, LOCC, STATUS )


*   Read the number of input variables from the NVAR_IN object.
          CALL CMP_GET0I( LOCC, 'NVAR_IN', NVIN, STATUS )


*   If there is no error, check that the number of variables is valid.
*   Report an error if it is not.
          IF( STATUS .EQ. SAI__OK ) THEN
            IF( NVIN .LT. 1 ) THEN
              STATUS = TRN__NVRIN       ! number of variables invalid
              CALL TRN1_ERRC( 'TRN1_GTNV', LOCTR, 'NVAR_IN', STATUS )


*   If the value was valid, move the cell locator to the last cell of
*   the array.
            ELSE
              IF( NMOD .GT. 1 ) THEN
                CALL DAT_ANNUL( LOCC, STATUS )
                CALL DAT_CELL( LOCMA, 1, NMOD, LOCC, STATUS )
              ENDIF


*   Read the number of output variables from the NVAR_OUT object.
              CALL CMP_GET0I( LOCC, 'NVAR_OUT', NVOUT, STATUS )


*   If there is no error, check that the number of variables is valid.
*   Report an error if it is not.
              IF( STATUS .EQ. SAI__OK ) THEN
                IF( NVOUT .LT. 1 ) THEN
                  STATUS = TRN__NVRIN   ! number of variables invalid
                  CALL TRN1_ERRC( 'TRN1_GTNV', LOCTR, 'NVAR_OUT',
     :                            STATUS )


*   End of "number of output variables is valid" condition.
                ENDIF


*   End of "no error reading number of output variables" condition.
              ENDIF


*   End of "number of input variables is valid" condition.
            ENDIF


*   End of "no error reading number of input variables" condition.
          ENDIF


*   Annul the local locators.
          CALL DAT_ANNUL( LOCC, STATUS )
          CALL DAT_ANNUL( LOCMA, STATUS )


*   End of "the type of the MODULE_ARRAY component is valid" condition.
        ENDIF


*   End of "no error finding the MODULE_ARRAY component" condition.
      ENDIF


*   Exit routine.
      END
