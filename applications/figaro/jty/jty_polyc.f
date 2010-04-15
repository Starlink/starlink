      SUBROUTINE JTY_POLYC(NC,SCALE,COEFF,PCOEFF)
C Routine to convert coefficients of a 1-d Legendre polynomial to
C coefficients of a plain polynomial.
C The NC coefficients are stored with the low order coefficient first.
C
C History:  Original version J. Tonry.
C Modified: TDCA 18-FEB-1999. Size of COEFF and PCOEFF arrays changed
C           from 1 to 3. Minor style changes.
C Modified: TDCA 19-MAY-1999. Intermediate steps added to some
C           calculations to avoid rounding errors.

      IMPLICIT NONE

      DOUBLE PRECISION COEFF(3), PCOEFF(3), SC(2), TEMP
      DOUBLE PRECISION PRODUCT, TEMP2, TEMP3
      REAL SCALE(2)
      INTEGER I, K, NC

C     Functions used
      INTEGER JTY_IBC

      INCLUDE 'JTY_LEGENDRE'

      SC(1) = DBLE(SCALE(1))
      SC(2) = DBLE(SCALE(2))

      DO K = 0,NC-1
          TEMP = 0.0
          DO I = K,NC-1
              TEMP = TEMP + COEFF(I+1) * LCOEFF(K+1,I+1)
          ENDDO
          PCOEFF(K+1) = TEMP
      ENDDO

      DO K = 0,NC-1
          TEMP = 0D0
          TEMP2 = 0.0
          DO I = K,NC-1
              PRODUCT = PCOEFF(I+1) * SC(1)**I *
     :        (-SC(2))**(I-K) * DFLOAT(JTY_IBC(K,I))
              TEMP3 = PCOEFF(I+1) * SC(1)**I * (-SC(2))**(I-K)
              TEMP3 = TEMP3 * DFLOAT(JTY_IBC(K,I))
              TEMP = TEMP + TEMP3
          ENDDO
          PCOEFF(K+1) = TEMP
      ENDDO

      RETURN
      END








