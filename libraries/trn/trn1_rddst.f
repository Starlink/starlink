      SUBROUTINE TRN1_RDDST( LOCTR, DFOR, DINV, STATUS )








*+
*  Name:
*     TRN1_RDDST

*  Purpose:
*     read definition status information.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_RDDST( LOCTR, DFOR, DINV, STATUS )

*  Description:
*     The routine reads the values of the FORWARD and INVERSE objects
*     in an HDS transformation structure and returns the definition
*     status for each direction.  Symbolic constants are used to
*     specify the values returned - these are defined in the include
*     file TRN_CONST.  The HDS objects from which the values are
*     obtained are fully validated.

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
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      CHARACTER * ( * ) LOCTR   ! Locator to transformation structure


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      INTEGER DFOR              ! Forward definition status
      INTEGER DINV              ! Inverse definition status


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
      LOGICAL CHR_SIMLR         ! String comparison (case insensitive)


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      CHARACTER * 9 VAL         ! Character representation of
                                ! definition status


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Read the value of the FORWARD component.
      CALL CMP_GET0C( LOCTR, 'FORWARD', VAL, STATUS )


*   If there is no error, check the result against the permitted values
*   and set the forward definition status DFOR appropriately.
      IF( STATUS .EQ. SAI__OK ) THEN
        IF( CHR_SIMLR( VAL, 'DEFINED' ) ) THEN
          DFOR = TRN_DS_DEF
        ELSE IF( CHR_SIMLR( VAL, 'UNDEFINED' ) ) THEN
          DFOR = TRN_DS_UDEF


*   If the value is invalid, set STATUS and report an error.
        ELSE
          STATUS = TRN__DSTIN   ! definition status invalid
          CALL TRN1_ERRC( 'TRN1_RDDST', LOCTR, 'FORWARD', STATUS )
        ENDIF
      ENDIF


*   Read the value of the INVERSE component.
      CALL CMP_GET0C( LOCTR, 'INVERSE', VAL, STATUS )


*   If there is no error, check the result against the permitted values
*   and set the inverse definition status DINV appropriately.
      IF( STATUS .EQ. SAI__OK ) THEN
        IF( CHR_SIMLR( VAL, 'DEFINED' ) ) THEN
          DINV = TRN_DS_DEF
        ELSE IF( CHR_SIMLR( VAL, 'UNDEFINED' ) ) THEN
          DINV = TRN_DS_UDEF


*   If the value is invalid, set STATUS and report an error.
        ELSE
          STATUS = TRN__DSTIN   ! definition status invalid
          CALL TRN1_ERRC( 'TRN1_RDDST', LOCTR, 'INVERSE', STATUS )
        ENDIF
      ENDIF


*   Exit routine.
      END
