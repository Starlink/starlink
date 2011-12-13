      SUBROUTINE TRN1_VALCL( NVIN, NVOUT, DFOR, DINV, CLASS, STATUS )








*+
*  Name:
*     TRN1_VALCL

*  Purpose:
*     validate a classification array.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_VALCL( NVIN, NVOUT, DFOR, DINV, CLASS, STATUS )

*  Description:
*     The routine checks that the logical classification array entries
*     associated with a possibly bi-directional transformation are
*     consistent and complete.  If inconsistent entries are present,
*     STATUS is set and an error is reported.  The routine also adds
*     missing entries to the array if these can be deduced from those
*     supplied.  Note that the validity of some entries depends on the
*     numbers of input and output variables and on the definition status
*     of the transformation in each direction.

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
*     28-NOV-1988:  Added a further deduction rule (DUVAD::RFWS)
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
      INTEGER NVIN              ! Number of input variables
      INTEGER NVOUT             ! Number of output variables
      INTEGER DFOR              ! Forward definition status for the
                                ! transformation
      INTEGER DINV              ! Inverse definition status for the
                                ! transformation


*  Arguments Given and Returned:
      LOGICAL CLASS( TRN__MXCLS )
                                ! Classification array to be validated


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
      LOGICAL REDNV             ! Transformation restricted to reducing
                                ! the number of variables?


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN



*   Check for inconsistent entries.
*   ------------------------------


*   Determine if the transformation definition is restricted to one
*   direction in such a way that the only possible action involves
*   reducing the number of variables.
      REDNV =
     :  ( ( DFOR .EQ. TRN_DS_UDEF ) .AND. ( NVOUT .LT. NVIN ) ) .OR.
     :  ( ( DINV .EQ. TRN_DS_UDEF ) .AND. ( NVIN .LT. NVOUT ) )


*   If the transformation is restricted in this way, it cannot be
*   isotropic, nor can the sign or value of the determinant of its
*   Jacobian matrix be specified.  Flag an error if any of these
*   inconsistent conditions exist.
      IF( REDNV .AND. ( CLASS( TRN__ISOT ) .OR.
     :                  CLASS( TRN__POSDT ) .OR.
     :                  CLASS( TRN__NEGDT ) .OR.
     :                  CLASS( TRN__CONDT ) .OR.
     :                  CLASS( TRN__UNIDT ) ) ) THEN
        STATUS = TRN__CLSIN     ! classification information invalid


*   The sign of the determinant of the Jacobian matrix cannot be
*   specified unless the transformation preserves the number of
*   coordinate variables.  Flag an error if the sign is specified under
*   the wrong conditions.
      ELSE IF( ( NVIN .NE. NVOUT ) .AND. ( CLASS( TRN__POSDT ) .OR.
     :                                     CLASS( TRN__NEGDT ) ) ) THEN
        STATUS = TRN__CLSIN     ! classificaton information invalid


*   The determinant of the Jacobian matrix cannot be both positive and
*   negative.  Flag an error if these conflicting properties are
*   specified.
      ELSE IF( CLASS( TRN__POSDT ) .AND. CLASS( TRN__NEGDT ) ) THEN
        STATUS = TRN__CLSIN     ! classification information invalid
      ENDIF


*   Report an error if the classification array entries are
*   inconsistent.
      IF( STATUS .NE. SAI__OK ) THEN
        CALL TRN1_ERROR( 'TRN1_VALCL', ' ', STATUS )


*   If there is no error...
      ELSE



*   Deduce any further entries which follow from those supplied.
*   -----------------------------------------------------------


*   The Jacobian matrix must be diagonal if there is only one input and
*   one output variable.
        CLASS( TRN__DIAG ) = CLASS( TRN__DIAG ) .OR.
     :                       ( ( NVIN .EQ. 1 ) .AND. ( NVOUT .EQ. 1 ) )


*   The transformation must be isotropic if there is only one input and
*   one output variable.
        CLASS( TRN__ISOT ) = CLASS( TRN__ISOT ) .OR.
     :                       ( ( NVIN .EQ. 1 ) .AND. ( NVOUT .EQ. 1 ) )


*   Coordinate variables are transformed independently if the Jacobian
*   matrix is diagonal.
        CLASS( TRN__INDEP ) = CLASS( TRN__INDEP ) .OR.
     :                        CLASS( TRN__DIAG )


*   A unit determinant for the Jabobian matrix also implies a constant
*   determinant.
        CLASS( TRN__CONDT ) = CLASS( TRN__CONDT ) .OR.
     :                        CLASS( TRN__ UNIDT )


*   If the number of input and output variables are both 1, then a
*   constant determinant implies linearity.
        IF( ( NVIN .EQ. 1 ) .AND. ( NVOUT .EQ. 1 ) )
     :    CLASS( TRN__LIN ) = CLASS( TRN__LIN ) .OR.
     :                        CLASS( TRN__CONDT )


*   Linearity implies a constant determinant for the Jacobian matrix,
*   so long as the transformation is not restricted to reducing the
*   number of coordinate variables.
        CLASS( TRN__CONDT ) = CLASS( TRN__CONDT ) .OR.
     :                      ( CLASS( TRN__LIN ) .AND. ( .NOT. REDNV ) )


*   End of "there is no error" condition.
      ENDIF


*   Exit routine.
      END
