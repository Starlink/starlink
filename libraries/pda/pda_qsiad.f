      SUBROUTINE PDA_QSIAD( EL, X, IP )
*+
*  Name:
*     PDA_QSIAD

*  Purpose:
*     Sort an array of pointers to access a DOUBLE PRECISION  array
*     in ascending order.

*  Language:
*     Fortran 77

*  Invocation:
*     CALL PDA_QSIAD( EL, X, IP )

*  Description:
*     The routine uses the QUICKSORT algorithm to permute an array of
*     pointers so that they access an associated array of values in
*     ascending order. The "median of three" modification is included
*     to reduce the likelihood of encountering the worst-case behaviour
*     of QUICKSORT.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements of X to sort.
*     X( EL ) = DOUBLE PRECISION (Given and Returned)
*        The array to be sorted.
*     IP( EL ) = INTEGER (Returned)
*        The indices of the elements of X in sorted order (i.e. IP( 1 )
*        gives the index into X of the lowest value).

*  References:
*     -  Sedgwick, R., 1988, "Algorithms" (Addison-Wesley).

*  Timing:
*     If N elements are to be sorted, the average time goes as N.ln(N).
*     The worst-case time goes as N**2.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     PDRAPER: P.W. Draper (STARLINK, Durham University)
*     {enter_new_authors_here}

*  History:
*     11-Jan-1991 (MJC):
*        Original version.
*     29-MAY-1992 (RFWS):
*        Modified to permute a separate pointer array. Also added the
*        "median of three" enhancement and the use of a sentinel value
*        to avoid having to check array bounds within the innermost
*        loop.
*     1-JUN-1992 (RFWS):
*        Added exchange sort to find the median of three partition
*        element.
*     6-AUG-1992 (RFWS):
*        Rationalised the inner loop to improve performance.
*     8-NOV-1995 (PDRAPER):
*        Renamed to PDA_QSIA (Quick Sort Indexed Ascending) for
*        the PDA library. Now returns the index of the first 1..N
*        elements of X.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER EL
      DOUBLE PRECISION X( EL )

*  Arguments Returned:
      INTEGER IP( EL )

*  Local Constants:
      INTEGER MXSTK             ! Size of the recursion stack (log
                                ! base 2 of the maximum number of
                                ! elements to be sorted)
      PARAMETER ( MXSTK = 64 )

*  Local Variables:
      DOUBLE PRECISION XPART    ! Partition value
      INTEGER I                 ! Index to pointer array
      INTEGER I1                ! Pointer index of partition value?
      INTEGER I2                ! Pointer index of partition value?
      INTEGER I3                ! Pointer index of partition value?
      INTEGER ITMP              ! Temporary store to swap pointers
      INTEGER J                 ! Index to pointer array
      INTEGER L                 ! Left hand pointer element in sub-file
      INTEGER LL( MXSTK )       ! Stack for left sub-file limits
      INTEGER R                 ! Right pointer element in sub-file
      INTEGER RR( MXSTK )       ! Stack for right sub-file limits
      INTEGER STK               ! Recursion stack pointer

*.

*  Initialise.
      STK = 1
      LL( 1 ) = 1
      RR( 1 ) = EL
      DO 5 I = 1, EL
         IP( I ) = I
 5    CONTINUE

*  Loop until the stack is empty.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( STK .GT. 0 ) THEN

*  If the current sub-file is sorted, then pop the recursion stack.
         IF ( LL( STK ) .GE. RR( STK ) ) THEN
            STK = STK - 1

*  Otherwise, partition the current sub-file.
         ELSE

*  Set the sub-file limits.
            L = LL( STK )
            R = RR( STK )

*  Find the index of a pointer to a suitable partition value (the
*  median of three possible elements) by performing an elementary
*  exchange sort.
            I1 = L
            I2 = ( L + R ) / 2
            I3 = R
            IF ( X( IP( I1 ) ) .GT. X( IP( I2 ) ) ) THEN
               ITMP = I1
               I1 = I2
               I2 = ITMP
            END IF
            IF ( X( IP( I1 ) ) .GT. X( IP( I3 ) ) ) THEN
               ITMP = I1
               I1 = I3
               I3 = ITMP
            END IF
            IF ( X( IP( I2 ) ) .GT. X( IP( I3 ) ) ) THEN
               ITMP = I2
               I2 = I3
               I3 = ITMP
            END IF

*  Store the partition value.
            XPART = X( IP( I2 ) )

*  Initialise for partitioning.
            I = L
            J = R

*  Loop to partition the subfile, incrementing I and decrementing J
*  until an exchange of values is indicated. Note we need not check the
*  array bounds as XPART is known to be present and acts as a sentinel.
 2          CONTINUE             ! Start of 'DO WHILE' loop

 3          CONTINUE             ! Start of 'DO WHILE' loop
            IF ( X( IP( I ) ) .LT. XPART ) THEN
               I = I + 1
               GO TO 3
            END IF

 4          CONTINUE             ! Start of 'DO WHILE' loop
            IF ( X( IP( J ) ) .GT. XPART ) THEN
               J = J - 1
               GO TO 4
            END IF

*  Exchange pairs of values when necessary by interchanging their
*  pointers.
            IF ( I .LT. J ) THEN
               ITMP = IP( I )
               IP( I ) = IP( J )
               IP( J ) = ITMP

*  Return to locate another pair of values to exchange,
               I = I + 1
               J = J - 1
               GO TO 2
            END IF

*  Push sub-file limits on to the stacks to further subdivide this
*  region, retaining the smaller part (which will be partitioned next).
            IF ( ( J - L ) .LT. ( R - I ) ) THEN
               LL( STK + 1 ) = LL( STK )
               RR( STK + 1 ) = J
               LL( STK ) = J + 1
            ELSE
               LL( STK + 1 ) = I
               RR( STK + 1 ) = RR( STK )
               RR( STK ) = I - 1
            END IF

*  Increment the stack counter.
            STK = STK + 1
         END IF

*  Iterate until the stack is empty.
         GO TO 1
      END IF

      END
* @(#)pda_qsiad.f   1.2   95/11/10 10:03:23   95/11/10 10:03:52
