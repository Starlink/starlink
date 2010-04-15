      SUBROUTINE PDA_SAARI( A, NDEC, N, M, IP, LINK, IFAIL )
*+
*  Name:
*     PDA_SAARI

*  Purpose:
*     Sorts the rows of a two dimensional array into ascending order.

*  Language:
*     Fortran-77

*  Invocation:
*     CALL PDA_SAARI( A, NDEC, N, M, IP, LINK  )

*  Description:
*     This routine returns a list row sorted indices to an array (rows
*     and columns span the first and second dimensions,
*     respectively). This means that the data in the first row is
*     sorted, any tied positions are then sorted by the corresponding
*     values of the data in the second row, any tied values here are
*     then sorted using the values in the third row and so on until the
*     array is completely value ordered, or all rows have been used.
*
*     The sort is stable so any completely tied columns preserve their
*     original order.

*  Arguments:
*     A( NDEC, M ) = INTEGER (Given)
*        The matrix to be ranked row by row.
*     NDEC = INTEGER (Given)
*        The declared size of the first dimension of A. This should be
*        two elements larger than the size of A to be sorted
*        (i.e. N). The dimensions IP and LINK should also be declared
*        as this size (i.e A should be at least A(N+2,M)).
*     N = INTEGER (Given)
*        The number of rows of A to be used (this should not be
*        bigger than NDEC-2).
*     M = INTEGER (Given)
*        The number of columns of A to be used.
*     IP( NDEC ) = INTEGER (Returned)
*        The indices of A when ranked into ascending order.
*     LINK( NDEC ) = INTEGER (Given and Returned)
*        Workspace.
*     IFAIL = INTEGER (Returned)
*        Non zero if a bounds error has been detected.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     12-SEP-1996 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Arguments Given:
      INTEGER NDEC
      INTEGER N
      INTEGER M
      INTEGER A( NDEC, M )

*  Arguments Given and Returned:
      INTEGER LINK( NDEC )

*  Arguments Returned:
      INTEGER IP( NDEC )
      INTEGER IFAIL

*  Local Variables:
      INTEGER I
      INTEGER J
      INTEGER K

*.

*  Check that NDEC is at least 2 bigger than N.
      IFAIL = 0
      IF ( NDEC .LT. N + 2 ) THEN
         IFAIL = 1
      ELSE IF ( N .EQ. 1 ) THEN

*  Already sorted do nothing.
      ELSE

*  Algorithm is very simple. Just sort the rows in reverse order.
*  This relies on a stable sort so that the order of columns is
*  preserved, so we use a list merge sort.
         DO 1 I = 1, N
            IP( I ) = I
 1       CONTINUE
         DO 2 J = M, 1, -1
            CALL PDA_LMSAI( N, A( 1, J ), IP, LINK )
 2       CONTINUE
      END IF
      END
