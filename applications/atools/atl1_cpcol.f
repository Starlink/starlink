      SUBROUTINE ATL1_CPCOL( LBND1, UBND1, LBND2, UBND2, NCOL, COLS,
     :                       FORWRD, A, B, STATUS )
*+
*  Name:
*     ATL1_CPCOL

*  Purpose:
*     Copies selected columns of a 2D array to or from another
*     array suitable for use as an AST position list.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL1_CPCOL( LBND1, UBND1, LBND2, UBND2, NCOL, COLS,
*                      FORWRD, A, B, STATUS )

*  Description:
*     Transfers position data between two 2D arrays, one of which (A) is
*     arranged so that selected columns corresponds to the coordinate axes,
*     and each row corresponds to a position. The other array (B) is arranged
*     so that each row corresponds to a coordinate axis and each column to
*     a position. The A array may have extra unused columns, but the B
*     array has no unused rows.

*  Arguments:
*     LBND1 = INTEGER (Given)
*        The lower bound on axis 1 of array A.
*     UBND1 = INTEGER (Given)
*        The upper bound on axis 1 of array A.
*     LBND2 = INTEGER (Given)
*        The lower bound on axis 2 of array A and axis 1 of array B.
*     UBND2 = INTEGER (Given)
*        The upper bound on axis 2 of array A and axis 1 of array B.
*     NCOL = INTEGER (Given)
*        The number of columns of data to be transferred.
*     COLS( NCOL ) = INTEGER (Given)
*        The column indices within array A to use. These must be in the
*        range LBND1 to UBND1.
*     FORWRD = LOGICAL (Given)
*        If .TRUE., then data is copied from array A into array B. If
*        .FALSE., data is copied from array B into array A.
*     A( LBND1:UBND1, LBND2:UBND2 ) = DOUBLE PRECISION (Given and Returned)
*        The A array.
*     B( LBND2:UBND2, NCOL ) = DOUBLE PRECISION (Given and Returned)
*        The B array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-JUL-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER LBND1
      INTEGER UBND1
      INTEGER LBND2
      INTEGER UBND2
      INTEGER NCOL
      INTEGER COLS( NCOL )
      LOGICAL FORWRD

*  Arguments Given and Returned:
      DOUBLE PRECISION A( LBND1:UBND1, LBND2:UBND2 )
      DOUBLE PRECISION B( LBND2:UBND2, NCOL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Index of the column within COLS
      INTEGER J                  ! Index of the position
      INTEGER K                  ! Index of the column within A
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the column indices are OK.
      DO I = 1, NCOL
         IF( COLS( I ) .LT. LBND1 .OR. COLS( I ) .GT. UBND1 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'I', I )
            CALL MSG_SETI( 'C', COLS( I ) )
            CALL MSG_SETI( 'L', LBND1 )
            CALL MSG_SETI( 'U', UBND1 )
            CALL ERR_REP( 'ATL1_CPCOL_ERR1', 'ATL1_CPCOL: Element ^I '//
     :                    'of the COLS array has value ^C which is '//
     :                    'outside the allowed range of ^L to ^U '//
     :                    '(internal programming error).', STATUS )
            GO TO 999
         END IF
      END DO

*  Loop round each column to be copied.
      DO I = 1, NCOL

*  Get the index within A of the column to be copied.
         K = COLS( I )

*  Copy the data from A to B.
         IF( FORWRD ) THEN
            DO J = LBND2, UBND2
               B( J, I ) = A( K, J )
            END DO

*  or copy it from B to A.
         ELSE
            DO J = LBND2, UBND2
               A( K, J ) = B( J, I )
            END DO
         END IF

      END DO

 999  CONTINUE

      END
