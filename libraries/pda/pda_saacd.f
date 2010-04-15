      SUBROUTINE PDA_SAACD( A, NDEC, N, M, IP, LINK, IFAIL )
*+
*  Name:
*     PDA_SAACD

*  Purpose:
*     Sorts the columns of a two dimensional array into ascending order.

*  Language:
*     Fortran-77

*  Invocation:
*     CALL PDA_SAACD( A, NDEC, N, M, IP, LINK  )

*  Description:
*     This routine returns a list column sorted indices to an array
*     (rows and columns span the first and second dimensions,
*     respectively). This means that the data in the first column is
*     sorted, any tied positions are then sorted by the corresponding
*     values of the data in the second column, any tied values here are
*     then sorted using the values in the third column and so on until
*     the array is completely value ordered, or all columns have been
*     used.
*
*     The sort is stable so any completely tied columns preserve their
*     original order.

*  Arguments:
*     A( NDEC, M ) = DOUBLE PRECISION (Given)
*        The matrix to be ranked column by column.
*     NDEC = INTEGER (Given)
*        The declared size of the first dimension of A.
*     N = INTEGER (Given)
*        The number of rows of A to be used.
*     M = INTEGER (Given)
*        The number of columns of A to be used. The declared size of
*        this array should be at least two larger than this value
*        (i.e. A should be at least A(NDEC,M+2)).
*     IP( M + 2 ) = INTEGER (Returned)
*        The indices of A when ranked into ascending order.
*     LINK( M + 2 ) = INTEGER (Given and Returned)
*        Workspace.
*     IFAIL = INTEGER (Returned)
*        Non zero if a bounds error has been detected.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     16-SEP-1996 (PDRAPER):
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
      DOUBLE PRECISION A( NDEC, * )

*  Arguments Given and Returned:
      INTEGER LINK( * )

*  Arguments Returned:
      INTEGER IP( * )
      INTEGER IFAIL

*  Local Variables:
      INTEGER I
      INTEGER J
      INTEGER K

*.

*  Check any bounds that we can.
      IFAIL = 0
      IF ( NDEC .LT. N ) THEN
         IFAIL = 1
      ELSE IF ( M .EQ. 1 ) THEN

*  Already sorted do nothing.
      ELSE

*  Algorithm is very simple. Just sort the columns in reverse order.
*  This relies on a stable sort so that the order of rows is
*  preserved, so we use a list merge sort.
         DO 1 I = 1, M
            IP( I ) = I
 1       CONTINUE
         DO 2 J = N, 1, -1
            CALL PDA_LMSCD( A, NDEC, M, J, IP, LINK )
 2       CONTINUE
      END IF
      END
