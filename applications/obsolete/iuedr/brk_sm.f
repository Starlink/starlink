       SUBROUTINE BRK_SM( KS, NOKS, M1, M2, TKS, TNOKS, NSM )

*+
*
*   Name:
*      SUBROUTINE BRK_SM
*
*   Description:
*      Smooths data over +/- NSM points in array, KS,
*      with associated data quality flags NOKS (F, T = good, bad);
*      leaves smoothed values in TKS, TNOKS arrays.
*
*   History:
*      Ian Howarth        ??-AUG-84     IUEDR Vn. 1.3
*      Paul Rees          30-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     08-OCT-94     IUEDR Vn. 3.1-6
*
*   Method:
*      See Barker (1984).
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER M1
      INTEGER M2

      REAL*8 KS(M1:M2)

      LOGICAL NOKS(M1:M2)

      INTEGER NSM

*   Export:
      REAL*8 TKS(150)

      LOGICAL TNOKS(150)

*   Local variables:
      INTEGER I
      INTEGER I1
      INTEGER I2
      INTEGER J
      INTEGER SUM
      INTEGER WEIGHT

*    Loop over all points
       DO I = M1, M2

*       At each point, loop over +/- NSM points and calculate weighted mean
          I1 = MAX(M1, I - NSM)
          I2 = MIN(M2, I + NSM)
          SUM = 0
          TKS(I) = 0.0
          TNOKS(I) = .TRUE.

          DO J = I1 + 1, I2 - 1
             IF ( .NOT. NOKS(J) ) THEN
                WEIGHT = (NSM - ABS(I-J))
                TKS(I) = TKS(I) + KS(J) * WEIGHT
                SUM = SUM + WEIGHT
             END IF
          ENDDO

*       Normalise
          IF ( SUM .GT. 0 ) THEN
             TKS(I) = TKS(I) / SUM
             TNOKS(I) = .FALSE.
          END IF
       ENDDO

       END
