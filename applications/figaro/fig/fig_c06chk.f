C+
      SUBROUTINE FIG_C06CHK (N,NEXT,PREV)
C
C     F I G _ C 0 6 C H K
C
C     Given a number N, checks it against the factorisation criteria
C     used by the NAG C06xxx routines, viz that it must have no more
C     than 20 prime factors, none of which may be greater than 19.  If N
C     fails the test, return the closest larger and smaller numbers that
C     pass it.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) N     (Integer) The number to be tested.
C     (<) NEXT  (Integer) The smallest number >= N acceptable to C06.
C     (<) PREV  (Integer) The largest number <= N acceptable to C06.
C
C     Common variables used - None
C
C     Subroutines / functions used -
C
C     FIG_C06TST   (FIG_ routine) Applies the C06 criteria to one value
C
C                                        4th Sept 1986  KS / AAO.
C
C     This routine is a modification of one originally written by
C     JOS / AAO.  It now returns PREV as well as NEXT and includes
C     the factors < 20 criterion.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N, NEXT, PREV
C
C     Functions
C
      LOGICAL FIG_C06TST
C
C     See if N itself is OK.
C
      IF (N.LE.0) THEN
         NEXT = 1
         PREV = 1
      ELSE IF (FIG_C06TST(N)) THEN
         NEXT = N
         PREV = N
      ELSE
C
C        If not, work up to find NEXT and then down to find PREV.
C
         NEXT = N + 1
         DO WHILE (.NOT.FIG_C06TST(NEXT))
            NEXT = NEXT + 1
         END DO
         PREV = N - 1
         DO WHILE (.NOT.FIG_C06TST(PREV))
            PREV = PREV - 1
         END DO
      END IF
C
      END
