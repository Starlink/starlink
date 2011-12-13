      SUBROUTINE TRN1_CBCLI( NVIN1, NVOUT1, DFOR1, DINV1, CLASS1,
     :                       NVIN2, NVOUT2, DFOR2, DINV2, CLASS2, NVIN,
     :                       NVOUT, DFOR, DINV, CLASS, STATUS )








*+
*  Name:
*     TRN1_CBCLI

*  Purpose:
*     combine classification information.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_CBCLI( NVIN1, NVOUT1, DFOR1, DINV1, CLASS1,
*                      NVIN2, NVOUT2, DFOR2, DINV2, CLASS2, NVIN,
*                      NVOUT, DFOR, DINV, CLASS, STATUS )

*  Description:
*     The routine combines classification information associated with
*     two (possibly bi-directional) transformations to reflect the
*     effects of concatenating them. The classification information
*     supplied is not validated before use, but the information
*     associated with the two transformations is checked for
*     compatibility.

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
*     30-MAR-1988:  Original version (DUVAD::RFWS)
*     5-MAY-1988:  Removed locators from argument list (DUVAD::RFWS)
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
      INCLUDE 'TRN_CONST'        ! TRN_ private constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      INTEGER NVIN1             ! Number of input variables for
                                ! transformation 1
      INTEGER NVOUT1            ! Number of output variables for
                                ! transformation 1
      INTEGER DFOR1             ! Forward definition status for
                                ! transformation 1
      INTEGER DINV1             ! Inverse definition status for
                                ! transformation 1
      LOGICAL CLASS1( TRN__MXCLS )
                                ! Classification array for
                                ! transformation 1
      INTEGER NVIN2             ! Number of input variables for
                                ! transformation 2
      INTEGER NVOUT2            ! Number of output variables for
                                ! transformation 2
      INTEGER DFOR2             ! Forward definition status for
                                ! transformation 2
      INTEGER DINV2             ! Inverse definition status for
                                ! transformation 2
      LOGICAL CLASS2( TRN__MXCLS )
                                ! Classification array for
                                ! transformation 2

*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      INTEGER NVIN              ! Number of input variables for the
                                ! combined transformation
      INTEGER NVOUT             ! Number of output variables for the
                                ! combined transformation
      INTEGER DFOR              ! Forward definition status for the
                                ! combined transformation
      INTEGER DINV              ! Inverse definition status for the
                                ! combined transformation
      LOGICAL CLASS( TRN__MXCLS )
                                ! Classification array for the
                                ! combined transformation


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      LOGICAL CLFOR1( TRN__MXCLS )
                                ! Forward classification array for
                                ! transformation 1
      LOGICAL CLINV1( TRN__MXCLS )
                                ! Inverse classification array for
                                ! transformation 1
      LOGICAL CLFOR2( TRN__MXCLS )
                                ! Forward classification array for
                                ! transformation 2
      LOGICAL CLINV2( TRN__MXCLS )
                                ! Inverse classification array for
                                ! transformation 2
      LOGICAL CLFOR( TRN__MXCLS )
                                ! Forward classification array for
                                ! the combined transformation
      LOGICAL CLINV( TRN__MXCLS )
                                ! Inverse classification array for
                                ! the combined transformation
      INTEGER I                 ! Loop counter for indexing
                                ! classification arrays


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Check for incompatibilities between the numbers of variables
*   for each transformation.  Report an error if necessary.
      IF( NVOUT1 .NE. NVIN2 ) THEN
        STATUS = TRN__NTVMM     ! number of transformation variables
                                ! mis-matched
        CALL TRN1_ERROR( 'TRN1_CBCLI', ' ', STATUS )


*   If there is no error, combine the numbers of variables information.
      ELSE
        NVIN = NVIN1
        NVOUT = NVOUT2


*   Combine the definition status information and check that the
*   combined transformation is defined in at least one direction.
*   Report an error if it is not.
        DFOR = MIN( DFOR1, DFOR2 )
        DINV = MIN( DINV1, DINV2 )
        IF( ( DFOR .EQ. TRN_DS_UDEF ) .AND.
     :      ( DINV .EQ. TRN_DS_UDEF ) ) THEN
          STATUS = TRN__ICDIR   ! incompatible transformation directions
          CALL TRN1_ERROR( 'TRN1_CBCLI', ' ', STATUS )


*   If there is no error, combine the classification array for the
*   first transformation with its definition status and number of
*   variables information to derive classification arrays appropriate
*   to each direction (forward/inverse).
        ELSE

*   ...forward:
          IF( DFOR1 .NE. TRN_DS_UDEF ) THEN
            DO I = 1, TRN__MXCLS
              CLFOR1( I ) = CLASS1( I )
            ENDDO
            CALL TRN1_NVCLS( NVIN1, NVOUT1, CLFOR1, STATUS )
          ELSE
            DO I = 1, TRN__MXCLS
              CLFOR1( I ) = .FALSE.
            ENDDO
          ENDIF

*   ...inverse:
          IF( DINV1 .NE. TRN_DS_UDEF ) THEN
            DO I = 1, TRN__MXCLS
              CLINV1( I ) = CLASS1( I )
            ENDDO
            CALL TRN1_NVCLS( NVOUT1, NVIN1, CLINV1, STATUS )
          ELSE
            DO I = 1, TRN__MXCLS
              CLINV1( I ) = .FALSE.
            ENDDO
          ENDIF


*   Repeat this process for the second transformation.

*   ...forward:
          IF( DFOR2 .NE. TRN_DS_UDEF ) THEN
            DO I = 1, TRN__MXCLS
              CLFOR2( I ) = CLASS2( I )
            ENDDO
            CALL TRN1_NVCLS( NVIN2, NVOUT2, CLFOR2, STATUS )
          ELSE
            DO I = 1, TRN__MXCLS
              CLFOR2( I ) = .FALSE.
            ENDDO
          ENDIF

*   ...inverse:
          IF( DINV2 .NE. TRN_DS_UDEF ) THEN
            DO I = 1, TRN__MXCLS
              CLINV2( I ) = CLASS2( I )
            ENDDO
            CALL TRN1_NVCLS( NVOUT2, NVIN2, CLINV2, STATUS )
          ELSE
            DO I = 1, TRN__MXCLS
              CLINV2( I ) = .FALSE.
            ENDDO
          ENDIF


*   Combine the arrays to reflect concatenation, deriving an array for
*   the combined transformation in each direction (forward/inverse).
          CALL TRN1_CBCLS( CLFOR1, CLFOR2, CLFOR, STATUS )
          CALL TRN1_CBCLS( CLINV2, CLINV1, CLINV, STATUS )


*   Form an overall classification array for the combined
*   transformation using a logical OR operation between the forward and
*   inverse arrays.  Finally, validate the result, which fills in any
*   entries which may still be missing.
          DO I = 1, TRN__MXCLS
            CLASS( I ) = ( CLFOR( I ) .OR. CLINV( I ) )
          ENDDO
          CALL TRN1_VALCL( NVIN, NVOUT, DFOR, DINV, CLASS, STATUS )


*   End of "the combined transformation is defined in at least one
*   direction" condition.
        ENDIF


*   End of "the numbers of variables are compatible" condition.
      ENDIF


*   Exit routine.
      END
