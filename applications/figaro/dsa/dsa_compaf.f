C+
      SUBROUTINE DSA_COMPAF (ARRAY1,ARRAY2,NELM,NERR,ERROR1)
C
C                           D S A _ C O M P A F
C
C  Routine name:
C     DSA_COMPAF
C
C  Function:
C     Compares the contents of two floating point arrays.
C
C  Description:
C     This routine compares two floating point arrays, reporting the
C     number of elements that differ and the number of the first
C     discrepant element.  Some allowance is made for rounding error.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_COMPAF (ARRAY1,ARRAY2,NELM,NERR,ERROR1)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) ARRAY1        (Real array,ref) First of the two arrays.
C     (>) ARRAY2        (Real array,ref) Second of the two arrays.
C     (>) NELM          (Integer,ref) Number of array elements.
C     (<) NERR          (Integer,ref) Number of discrepant elements.
C     (<) ERROR1        (Integer,ref) Number of first discrepant element.
C
C  External subroutines / functions used:  None.
C
C  Prior requirements: None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  History:
C     16th July 1987   Original version.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM, NERR, ERROR1
      REAL ARRAY1(NELM), ARRAY2(NELM)
C
C     Local variables
C
      REAL    DIFF                                 ! Absolute discrepancy
      REAL    DIFLIM                               ! Allowed discrepancy
      INTEGER I                                    ! Loop index
C
      NERR=0
      ERROR1=0
      DO I=1,NELM
         IF (ARRAY1(I).NE.ARRAY2(I)) THEN
            DIFF=ABS(ARRAY1(I)-ARRAY2(I))
            DIFLIM=ABS(ARRAY1(I)+ARRAY2(I))*0.00005
            IF (DIFF.GT.DIFLIM) THEN
               NERR=NERR+1
               IF (NERR.EQ.1) ERROR1=I
            END IF
         END IF
      END DO
C
      END
