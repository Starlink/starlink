      SUBROUTINE PDA_IPERM( N, X )
*+
*  Name:
*     PDA_IPERM

*  Purpose:
*     Forms the inverse of a permutation.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL PDA_IPERM( N, X )

*  Description:
*     This routine inverts a permutation in place. It can be used to
*     transform an index vector (from a sort) into a rank vector and
*     vice versa.

*  Arguments:
*     N = INTEGER (Read)
*        Number of elements.
*     X( N ) = _INTEGER (Read and Write)
*        The permutation. On exit this contains the inverse.

*  References:
*     "The Art of Computer Programming, Fundermental Algorithms Vol 1",
*     by Donald E. Knuth (Addison-Wesley).

*  Notes:
*     The permutation must consist of positive integers.
*
*     The permutation inverse:
*
*        Y(X(I))=I for I=1,N
*
*     can be formed trivially with 2*N arrays.

*  Timing:
*      Proportional to N.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     8-NOV-1995 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Arguments Given:
      INTEGER N

*  Arguments Given and Returned:
      INTEGER X( N )

*  Local Variables:
      INTEGER M                 ! Number value processed.
      INTEGER I, J, K           ! Storage variables.

*.

*  Initialise number of processed elements.
      M = N

*  Start main loop.
 2    CONTINUE
      I = X( M )
      IF ( I .LT. 0 ) THEN

*  This one has already been done, skip change back to original
*  sign and skip to next.
         X( M ) = -X( M )
         GO TO 6
      ELSE IF ( M .EQ. I ) THEN

*  Permutation fixed.
         GO TO 6
      END IF

*  Start a permutation cycle.
      K = M
 4    CONTINUE
      J = X( I )
      X( I ) = -K
      IF ( J .EQ. M ) THEN

*  End of this cycle.
         X( M ) = I
      ELSE
         K = I
         I = J
         GO TO 4
      END IF

*  Decrement number of processes elements.
 6    CONTINUE
      M = M - 1

*  Return to process more if not all done.
      IF ( M .GT. 0 ) GO TO 2
      END
* @(#)pda_iperm.f   1.3   95/11/10 10:05:54   95/11/10 10:06:00
