      SUBROUTINE sla_PERMUT ( N, ISTATE, IORDER, J )
*+
*     - - - - - - -
*      P E R M U T
*     - - - - - - -
*
*  Generate the next permutation of a specified number of items.
*
*  Given:
*     N         i      number of items:  there will be N! permutations
*     ISTATE    i(N)   state, ISTATE(1)=-1 to initialize
*
*  Returned:
*     ISTATE    i(N)   state, updated ready for next time
*     IORDER    i(N)   next permutation of numbers 1,2,...,N
*     J         i      status: -1 = illegal N (zero or less is illegal)
*                               0 = OK
*                              +1 = no more permutations available
*
*  Notes:
*
*  1) This routine returns, in the IORDER array, the integers 1 to N
*     inclusive, in an order that depends on the current contents of
*     the ISTATE array.  Before calling the routine for the first
*     time, the caller must set the first element of the ISTATE array
*     to -1 (any negative number will do) to cause the ISTATE array
*     to be fully initialized.
*
*  2) The first permutation to be generated is:
*
*          IORDER(1)=N, IORDER(2)=N-1, ..., IORDER(N)=1
*
*     This is also the permutation returned for the "finished"
*     (J=1) case.
*
*     The final permutation to be generated is:
*
*          IORDER(1)=1, IORDER(2)=2, ..., IORDER(N)=N
*
*  3) If the "finished" (J=1) status is ignored, the routine continues
*     to deliver permutations, the pattern repeating every N! calls.
*
*  P.T.Wallace   Starlink   25 August 1999
*
*  Copyright (C) 1999 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      INTEGER N,IORDER(N),ISTATE(N),J

      INTEGER I,IP1,ISLOT,ISKIP


*  -------------
*  Preliminaries
*  -------------

*  Validate, and set status.
      IF (N.LT.1) THEN
         J = -1
         GO TO 9999
      ELSE
         J = 0
      END IF

*  If just starting, initialize state array
      IF (ISTATE(1).LT.0) THEN
         ISTATE(1) = -1
         DO I=2,N
            ISTATE(I) = 0
         END DO
      END IF

*  --------------------------
*  Increment the state number
*  --------------------------

*  The state number, maintained in the ISTATE array, is a mixed-radix
*  number with N! states.  The least significant digit, with a radix of
*  1, is in ISTATE(1).  The next digit, in ISTATE(2), has a radix of 2,
*  and so on.

*  Increment the least-significant digit of the state number.
      ISTATE(1) = ISTATE(1)+1

*  Digit by digit starting with the least significant.
      DO I=1,N

*     Carry?
         IF (ISTATE(I).GE.I) THEN

*        Yes:  reset the current digit.
            ISTATE(I) = 0

*        Overflow?
            IF (I.GE.N) THEN

*           Yes:  there are no more permutations.
               J = 1
            ELSE

*           No:  carry.
               IP1 = I+1
               ISTATE(IP1) = ISTATE(IP1)+1
            END IF
         END IF
      END DO

*  -------------------------------------------------------------------
*  Translate the state number into the corresponding permutation order
*  -------------------------------------------------------------------

*  Initialize the order array.  All but one element will be overwritten.
      DO I=1,N
         IORDER(I) = 1
      END DO

*  Look at each state number digit, starting with the most significant.
      DO I=N,2,-1

*     Initialize the position where the new number will go.
         ISLOT = 0

*     The state number digit says which unfilled slot is to be used.
         DO ISKIP=0,ISTATE(I)

*        Increment the slot number until an unused slot is found.
            ISLOT = ISLOT+1
            DO WHILE (IORDER(ISLOT).GT.1)
               ISLOT = ISLOT+1
            END DO
         END DO

*     Store the number in the permutation order array.
         IORDER(ISLOT) = I
      END DO

 9999 CONTINUE

      END
