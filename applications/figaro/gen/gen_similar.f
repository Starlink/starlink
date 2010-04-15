C+
C                        G E N _ S I M I L A R
C
C  Routine name:
C     GEN_SIMILAR
C
C  Function:
C     Tests to see if two floating point values are nearly the same
C
C  Description:
C     GEN_SIMILAR compares two floating point values, not for equality,
C     but for near equality - being the same, give or take a bit of
C     rounding error.
C
C  Language:
C     FORTRAN
C
C  Call:
C     STATUS = GEN_SIMILAR (VALUE1,VALUE2)
C
C  Parameters -   (">" input, "<" output)
C
C     (>) VALUE1  (Real, ref) The first of the two numbers
C     (>) VALUE2  (Real, ref) The second of the two numbers
C
C  Returns -
C     (<) STATUS  (Logical, function value) True if the values are
C                 similar, false otherwise.
C
C  External subroutines / functions used:   None
C
C  External variables used:  None
C
C  Author: Keith Shortridge, AAO
C
C  Date: 1st March 1989
C-
C  History:
C     16th July  1986   Original macro version (very unsatisfactory)  KS/AAO
C     1st  March 1989   Fortran version based on algorithm provided by
C                       JOS/AAO.
C+
      LOGICAL FUNCTION GEN_SIMILAR (VALUE1,VALUE2)
C
      IMPLICIT NONE
C
C     Parameters
C
      REAL VALUE1, VALUE2
C
C     Local variables
C
      REAL  DIFF          ! Difference between ratio of values and 1.0
C
C     DELTA is the largest value such that 1 and 1+DELTA are not
C     significantly different for the purposes of this routine.
C     Obviously, changing DELTA changes the tolerance of the routine.
C
      REAL DELTA
      PARAMETER (DELTA=1.0E-6)
C
C     There is a pathological case where the two values are both
C     zero.  That's the one that matters, and this test gets rid of
C     that and other cases too.
C
      IF (VALUE1.EQ.VALUE2) THEN
         GEN_SIMILAR=.TRUE.
      ELSE
C
C        Compare the ratio of the two numbers with 1.0 and see if they
C        differ by more than DELTA.
C
         IF (ABS(VALUE2).GT.ABS(VALUE1)) THEN
            DIFF = 1.0 - VALUE1/VALUE2
         ELSE
            DIFF = 1.0 - VALUE2/VALUE1
         END IF
         GEN_SIMILAR = DIFF.LE.DELTA
      END IF
C
      END
