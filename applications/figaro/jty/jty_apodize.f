      SUBROUTINE JTY_APODIZE(N,N1,N2,DATA,PERCENT)
C Program to apodize the ends of the spectrum with a cosine bell
C The cosine bell starts rising from zero at N1 and falls to zero at N2
C
C History : Original version J. Tonry.
C Modified: HME 01-SEP-1992.  Added brackets to parameter statement
C         : TDCA 18-FEB-1999. Minor style changes.
C         : ACD 21-JUL-2000.  Added IMPLICIT NONE and made more robust
C                             for small numbers of points.

      IMPLICIT NONE
      INTEGER N, N1, N2, I, II, NSQUASH
      REAL DATA(1), PERCENT, ARG, FACTOR, PI
      PARAMETER ( PI = 3.14159265 )

      NSQUASH = N * .01 * PERCENT
      NSQUASH = MIN(NSQUASH,(N2-N1)/2)
      IF (NSQUASH .GT. 1) THEN
         DO II = 1,NSQUASH
             I = II - 1
             ARG = PI * FLOAT(I)/(NSQUASH-1)
             FACTOR = .5 * (1 - COS(ARG))
             DATA(N1+I) = FACTOR * DATA(N1+I)
             DATA(N2-I) = FACTOR * DATA(N2-I)
         END DO
      END IF
      DO I = 1,N1-1
          DATA(I) = 0
      END DO
      DO I = N2+1,N
          DATA(I) = 0
      END DO
      RETURN
      END
