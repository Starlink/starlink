      SUBROUTINE TRN1_RDCLS( LOCTR, CLASS, STATUS )








*+
*  Name:
*     TRN1_RDCLS

*  Purpose:
*     read classification information from a transformation.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_RDCLS( LOCTR, CLASS, STATUS )

*  Description:
*     The routine reads classification information from a transformation
*     structure passed by HDS locator, returning it in a logical array.
*     The information obtained is not validated.

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
      INCLUDE 'TRN_CONST'        ! TRN_ private constants
      INCLUDE 'TRN_PAR'          ! TRN_ public constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      CHARACTER * ( * ) LOCTR   ! Locator to transformation structure


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      LOGICAL CLASS( TRN__MXCLS )
                                ! Classification array to contain
                                ! results


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      LOGICAL THERE             ! Whether a CLASSIFICATION component
                                ! exists within the transformation
      INTEGER I                 ! Loop counter for indexing
                                ! classification array elements
      CHARACTER * ( DAT__SZLOC ) LOCCL
                                ! Locator to the CLASSIFICATION
                                ! structure
      CHARACTER * ( DAT__SZTYP ) CLTYPE
                                ! HDS type of the CLASSIFICATION
                                ! structure
      CHARACTER * ( DAT__SZNAM) CLNAME( TRN__MXCLS )
                                ! Names of objects within the
                                ! CLASSIFICATION structure


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*    (names of HDS objects used to hold the classification properties)
      DATA CLNAME( TRN__LIN   ) / TRN_CN_LIN   /
      DATA CLNAME( TRN__INDEP ) / TRN_CN_INDEP /
      DATA CLNAME( TRN__DIAG  ) / TRN_CN_DIAG  /
      DATA CLNAME( TRN__ISOT  ) / TRN_CN_ISOT  /
      DATA CLNAME( TRN__POSDT ) / TRN_CN_POSDT /
      DATA CLNAME( TRN__NEGDT ) / TRN_CN_NEGDT /
      DATA CLNAME( TRN__CONDT ) / TRN_CN_CONDT /
      DATA CLNAME( TRN__UNIDT ) / TRN_CN_UNIDT /


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   See if a CLASSIFICATION component is present within the
*   transformation structure.  If it is not, then the classification
*   array is set to .FALSE. throughout.
      THERE = .FALSE.
      CALL DAT_THERE( LOCTR, 'CLASSIFICATION', THERE, STATUS )
      IF( .NOT. THERE ) THEN
        DO I = 1, TRN__MXCLS
          CLASS( I ) = .FALSE.
        ENDDO


*   If the component exists, obtain a locator to it and determine its
*   HDS type.
      ELSE
        CALL DAT_FIND( LOCTR, 'CLASSIFICATION', LOCCL, STATUS )
        CALL DAT_TYPE( LOCCL, CLTYPE, STATUS )


*   If there is no error, check the type.  If it is not correct, set
*   STATUS and report an error.
        IF( STATUS .EQ. SAI__OK ) THEN
          IF( CLTYPE .NE. 'TRN_CLASS' ) THEN
            STATUS = TRN__TYPIN ! type invalid
            CALL TRN1_ERRL( 'TRN1_RDCLS', LOCCL, STATUS )


*   If the type is OK, read a logical value for each classification
*   property from the contents of the CLASSIFICATION structure.
          ELSE
            DO I = 1, TRN__MXCLS
              CALL TRN1_RDLOG( LOCCL, CLNAME( I ), .FALSE., CLASS( I ),
     :                         STATUS )
            ENDDO
          ENDIF


*   End of "no error finding the component type" condition.
        ENDIF


*   Annul the locator to the CLASSIFICATION structure.
        CALL DAT_ANNUL( LOCCL, STATUS )


*   End of "the CLASSIFICATION component is present" condition.
      ENDIF


*   Exit routine.
      END
