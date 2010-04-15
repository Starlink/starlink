C+
      LOGICAL FUNCTION GEN_CHKINT (ARRAY,NELM)
C
C     G E N _ C H K I N T
C
C     Checks a real array for the condition that each element
C     contains an integer.  That is, GEN_CHKINT is true if
C     ARRAY(I)=INT(ARRAY(I)) for all values of I.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) ARRAY    (Real array ARRAY(NELM)) The data to be tested.
C     (>) NELM     (Integer) The number of elements of ARRAY.
C
C     Returns -
C
C     GEN_CHKINT   (Logical) Result of the test.
C
C     Subroutines / functions used -  None
C
C                                          KS / AAO 23rd Nov 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL ARRAY(NELM)
C
C     Local variables
C
      INTEGER I
C
C     Test
C
      GEN_CHKINT=.TRUE.
      DO I=1,NELM
         IF (ARRAY(I).NE.INT(ARRAY(I))) THEN
            GEN_CHKINT=.FALSE.
            GO TO 600
         END IF
      END DO
  600 CONTINUE
C
      END
