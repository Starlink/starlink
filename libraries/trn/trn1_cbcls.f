      SUBROUTINE TRN1_CBCLS( CLASS1, CLASS2, RESULT, STATUS )








*+
*  Name:
*     TRN1_CBCLS

*  Purpose:
*     combine two logical classification arrays.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_CBCLS( CLASS1, CLASS2, RESULT, STATUS )

*  Description:
*     The routine combines the logical classification arrays associated
*     with two uni-directional transformations to reflect the effect of
*     concatenating them.

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
      LOGICAL CLASS1( TRN__MXCLS )
                                ! Classification array for the first
                                ! transformation
      LOGICAL CLASS2( TRN__MXCLS )
                                ! Classification array for the second
                                ! transformation


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      LOGICAL RESULT( TRN__MXCLS )
                                ! Classification array for the combined
                                ! transformation


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      INTEGER I                 ! Loop counter for indexing
                                ! classification array elements


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Perform a logical AND operation between corresponding elements of
*   the two input classification arrays.  This is effective in
*   combining most of the classification properties.  Those requiring
*   special treatment are handled separately below.
      DO I = 1, TRN__MXCLS
        RESULT( I ) = ( CLASS1( I ) .AND. CLASS2 ( I ) )
      ENDDO


*   The determinant of the Jacobian matrix for the combined
*   transformation is positive if: (a) it is positive for both the
*   transformations individually, or (b) it is negative for both
*   individually.  Set the appropriate RESULT array element accordingly.
      RESULT( TRN__POSDT ) =
     : ( ( CLASS1( TRN__POSDT ) .AND. CLASS2( TRN__POSDT ) ) .OR.
     :   ( CLASS1( TRN__NEGDT ) .AND. CLASS2( TRN__NEGDT ) ) )


*   The determinant of the Jacobian matrix for the combined
*   transformation is negative if: (a) it is positive for one input
*   transformation and (b) it is negative for the other.  Set the
*   appropriate RESULT array element accordingly.
      RESULT( TRN__NEGDT ) =
     : ( ( CLASS1( TRN__POSDT ) .AND. CLASS2( TRN__NEGDT ) ) .OR.
     :   ( CLASS1( TRN__NEGDT ) .AND. CLASS2( TRN__POSDT ) ) )


*   Exit routine.
      END
