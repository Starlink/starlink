      SUBROUTINE TRN1_WRCLS( LOCTR, CLASS, STATUS )








*+
*  Name:
*     TRN1_WRCLS

*  Purpose:
*     write classification information to a transformation.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_WRCLS( LOCTR, CLASS, STATUS )

*  Description:
*     The routine enters classification information into a
*     transformation structure passed by HDS locator.  The information
*     is supplied as a logical array.  If necessary, a CLASSIFICATION
*     component is created in the transformation structure and standard
*     _LOGICAL objects are created within it to contain the
*     transformation properties.  The names of these objects are defined
*     in the include file TRN_CONST.  If the transformation already
*     contains classification information it is over-written by this
*     routine.  The classification array supplied is not validated
*     before use.

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


*  Arguments Given:
      CHARACTER * ( * ) LOCTR   ! Locator to transformation structure
      LOGICAL CLASS( TRN__MXCLS )
                                ! Classification array


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
*     <local constants defined by parameter>


*  Local Variables:
      LOGICAL CLTRUE            ! Whether the classification array
                                ! contains any .TRUE. values
      LOGICAL THERE             ! Whether a CLASSIFICATION component
                                ! exists in the transformation
      INTEGER I                 ! Loop counter for indexing elements
                                ! of the classification array
      CHARACTER * ( DAT__SZNAM) CLNAME( TRN__MXCLS )
                                ! Names of objects within the
                                ! CLASSIFICATION structure
      CHARACTER * ( DAT__SZLOC ) LOCCL
                                ! Locator to the CLASSIFICATION
                                ! structure


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


*   Determine if the classification array has any  .TRUE. values in it.
      CLTRUE = .FALSE.
      DO I = 1, TRN__MXCLS
        CLTRUE = ( CLTRUE .OR. CLASS( I ) )
      ENDDO


*   If it does not, see if a CLASSIFICATION component is present within
*   the transformation structure.  If it is, then erase it.
      IF( .NOT. CLTRUE ) THEN
        THERE = .FALSE.
        CALL DAT_THERE( LOCTR, 'CLASSIFICATION', THERE, STATUS )
        IF( THERE ) CALL DAT_ERASE( LOCTR, 'CLASSIFICATION', STATUS )


*   If there are some .TRUE. values, ensure a CLASSIFICATION component
*   with the required type is present and obtain a locator to it.
      ELSE
        CALL CMP_MOD( LOCTR, 'CLASSIFICATION', 'TRN_CLASS', 0, 0,
     :                STATUS )
        CALL DAT_FIND( LOCTR, 'CLASSIFICATION', LOCCL, STATUS )


*   Write the logical value of each element of the classification array
*   to the appropriate component in the CLASSIFICATION structure.
        DO I = 1, TRN__MXCLS
          CALL TRN1_WRLOG( LOCCL, CLNAME( I ), .FALSE., CLASS( I ),
     :                     STATUS )
        ENDDO


*   Annul the locator to the CLASSIFICATION structure.
        CALL DAT_ANNUL( LOCCL, STATUS )


*   End of "the classification array contains some .TRUE. values"
*   condition.
      ENDIF


*   Exit routine.
      END
