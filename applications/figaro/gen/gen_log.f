C+
      SUBROUTINE GEN_LOG (VEXIST,IN,INVAR,NELM,OUT,OUTVAR)
C
C     G E N _ L O G
C
C     Takes the base 10 log of each element in an array to
C     generate an output array.  Output elements corresponding to
C     negative input elements are set to zero.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) VEXIST  (Logical) True if Variance array exists
C     (>) IN      (Real array IN(NELM)) The input array
C     (>) INVAR   (Real array INVAR(NELM)) The input variance array
C     (>) NELM    (Integer) The number of elements in IN.
C     (<) OUT     (Real array OUT(NELM)) The resulting output array
C                 Note that IN and OUT may be the same array in the
C                 calling routine.
C     (>) OUTVAR  (Real array OUTVAR(NELM)) The output variance array
C                 Note that INVAR and OUTVAR may be the same array in the
C                 calling routine.
C
C     Common variables used -  None
C
C     Subroutines / functions used -  None
C
C     KS  / CIT 22nd April 1984
C     JJL / Starlink, Soton, 23rd May 1997. Error propagation added
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL IN(NELM), INVAR(NELM), OUT(NELM), OUTVAR(NELM)
      LOGICAL VEXIST
C
C     Local variables
C
      INTEGER I
      REAL CONST
C
C     Set CONST as natural log of 10
C
      CONST = LOG(10.0)
C
      DO I=1,NELM
         IF (IN(I).LE.0.) THEN
            OUT(I)=0.
            IF (VEXIST) OUTVAR(I) = 0
         ELSE
            OUT(I) = LOG10(IN(I))
            IF (VEXIST) OUTVAR(I) = INVAR(I)/((IN(I)*CONST)**2.)
         END IF
      END DO
C
      END
