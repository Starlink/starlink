      SUBROUTINE KPS1_LFTPS( X, Y, V, ORDER, BONLY, A, B )
*+
*  Name:
*     KPG1_LFTPS

*  Purpose:
*     Increment least squares polynomial sums.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LFTPS( X, Y, V, ORDER, BONLY, A, B )

*  Description
*     This routine adds a point into the cumulative sums for KPS1_LFTx
*     and KPS1_LFTQx.

*  Arguments:
*     X = DOUBLE PRECISION (Given)
*        The position.
*     Y = DOUBLE PRECISION (Given)
*        The value.
*     V = DOUBLE PRECISION (Given)
*        The variance of value.
*     ORDER = INTEGER (Given)
*        The order of polynomial which will be fit using these sums.
*     BONLY + LOGICAL (Given)
*        If true only do sums for B. Can be used when symmetry means
*        A is same for all lines.
*     A( ORDER + 1, ORDER + 1 ) = DOUBLE PRECISION (Given and Returned)
*        Matrix holder power X sums (A in Ax=B).
*     B( ORDER + 1 ) = DOUBLE PRECISION (Given and Returned)
*        Y times power X sums (B in Ax=B).

*  Authors:
*     PWD: Peter W. Draper (JAC, Durham University)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-SEP-2005 (PWD):
*        Original version.
*     2006 April 12 (MJC):
*        Remove unused variable and correct invocation.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:

*  Arguments Given:
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION V
      INTEGER ORDER
      LOGICAL BONLY

*  Arguments Given and Returned:
      DOUBLE PRECISION A( ORDER + 1, ORDER + 1 )
      DOUBLE PRECISION B( ORDER + 1 )

*  Local Variables:
      DOUBLE PRECISION W         ! Weight, inverse variance.
      INTEGER J                  ! Loop index
      INTEGER K                  ! Loop index
*.

      W = 1.0D0 / V

*  B sums.
      DO K = 1, ORDER + 1
         IF ( ( K - 1 ) .NE. 0 ) THEN
            B( K ) = B( K ) + ( Y * ( X ** ( K - 1 ) ) ) * W
         ELSE
            B( K ) = B( K ) + Y * W
         END IF
      END DO

*  A sums.
      IF ( .NOT. BONLY ) THEN
         DO K = 1, ORDER + 1
            DO J = K, ORDER + 1
               IF ( ( K + J - 2 ) .NE. 0 ) THEN
                  A( K, J ) = A( K, J ) + ( X ** ( K + J - 2 ) ) * W
               ELSE
                  A( K, J ) = A( K, J ) + W
               END IF
               A( J, K ) = A( K, J )
            END DO
         END DO
      END IF
      END
