*CDF_LOC
*
* CDF_LOC -- locates a particular level in the histogram of a 2D array.
*            This is useful for autoscaling.
*
* CDF_LOC returns the LEVEL for which FRAC pixels in the selected region of
* a frame are below it. To avoid sorting the array completely it only does
* this approximately though for speed, so do not use the value in critical
* applications.
*
* The method is to first compute the min and max values in the first pass
* and then to narrow down LEVEL by binning into a histogram and identifying
* values above and below it.  On each cycle the range straddling level
* is cut down by a factor of NHIST. You can select the number of cycles
* to be carried out.
*
*      SUBROUTINE CDF_LOC(DATA, NX, NY, HIST, NHIST, FRAC, CYCLES,
*      LOW, HIGH, LEVEL)
*
*  Arguments:
*
*    R*4   DATA(NX,NY)        -- Data array.
*    I*4   NX, NY             -- Size of data array
*    R*4   HIST(NHIST)        -- Histogram work buffer
*    I*4   NHIST              -- Size of histogram work buffer
*    R*4   FRAC               -- Level equivalent to lower value returned
*                                Must range from 0 to 1. e.g. 0.1
*    I*4   CYCLES             -- Number of refinement cycles. This should be
*                                large enough for different values of FRAC to
*                                give different values of LEVEL
*
*    R*4   LOW, HIGH          -- Range covering the value of LEVEL. Normally
*                                they should just be the min and max. They are
*                                supplied rather than calculated to save time.
*
*    R*4   LEVEL
*
*CDF_LOC
      SUBROUTINE CDF_LOC(DATA, NX, NY, HIST, NHIST, FRAC,
     &     CYCLES, LOW, HIGH, LEVEL)
C
      IMPLICIT NONE
      INCLUDE 'PRM_PAR'
      INTEGER NX, NY, NHIST, CYCLES
      REAL DATA(NX,NY), HIST(NHIST), FRAC, LOW, HIGH, LEVEL
      INTEGER I, J, L, NCYC, NOK, NLO, NSUM
      REAL D1, D2, DMUL, NEWD1, NEWD2
C
      IF(FRAC.LE.0. .OR. FRAC.GE.1.) THEN
         WRITE(*,*) 'FRAC out of range in CDF_LOC'
         IF(FRAC.LT.0.5) THEN
            LEVEL = LOW
         ELSE
            LEVEL = HIGH
         END IF
         RETURN
      END IF
      D1 = LOW
      D2 = HIGH
      DO NCYC = 1, CYCLES
         DMUL = REAL(NHIST)/(D2-D1)
         NLO  = 0
         NOK  = 0
         DO L = 1, NHIST
            HIST(L) = 0.
         END DO
         DO J = 1, NY
            DO I = 1, NX
               IF(DATA(I,J).NE.VAL__BADR) THEN
                  NOK = NOK + 1
                  L = NINT(DMUL*(DATA(I,J)-D1)+0.5)
                  IF(L.LT.1) THEN
                     NLO = NLO + 1
                  ELSE IF(L.LE.NHIST) THEN
                     HIST(L) = HIST(L) + 1.
                  END IF
               END IF
            END DO
         END DO
C
C     Make into CDF
C
         NSUM = NLO
         DO I = 1, NHIST
            NSUM = NSUM + NINT(HIST(I))
            HIST(I) = REAL(NSUM)/REAL(NOK)
         END DO
C
C     Identify where the search level is
C
         L = 1
         DO WHILE(FRAC.GT.HIST(L))
            L = L + 1
         END DO
         NEWD1 = D1 + (D2-D1)*REAL(L-1)/REAL(NHIST)
         NEWD2 = D1 + (D2-D1)*REAL(L  )/REAL(NHIST)
         D1    = NEWD1
         D2    = NEWD2
      END DO
      LEVEL = (D1+D2)/2.
      RETURN
      END


