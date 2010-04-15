C+
      LOGICAL FUNCTION GEN_CHKNSF (ARRAY,NELM)
C
C     G E N _ C H K N S F
C
C     Checks a real array for the (admittedly somewhat odd)
C     condition that every element of the array contains its own
C     element number.  That is, GEN_CHKNSF is true if
C     ARRAY(I)=FLOAT(I) for all values of I.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) ARRAY    (Real array ARRAY(NELM)) The data to be tested.
C     (>) NELM     (Integer) The number of elements of ARRAY.
C
C     Returns -
C
C     GEN_CHKNSF   (Logical) Result of the test.
C
C     Subroutines / functions used -  None
C
C                                          KS / CIT  4th March 1983
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
      GEN_CHKNSF=.TRUE.
      DO I=1,NELM
         IF (ARRAY(I).NE.FLOAT(I)) THEN
            GEN_CHKNSF=.FALSE.
            GO TO 600
         END IF
      END DO
  600 CONTINUE
C
      END
