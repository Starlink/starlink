      SUBROUTINE RTD1_STINT( IORD )
C----------------------------------------------------------------------
C   RTD1_STINT computes certain quantities which are needed by RTD1_EVERT 
C      when it computes the actual weights for an interpolation. The
C      result produced by RTD1_STINT is in the BCOEF array in the COMMON
C      block, and is based on binomial coefficients computed by RTD1_BINOM.
C      From the Everett interpolation package originally coded by
C      Larry Goad at KPNO.  This version has been lifted almost unchanged
C      from the AIPS package - the only change has been that the include
C      files are now explicitly included in the program body.
C  INPUT:
C      IORD    I*2  The order of the interpolation, 0 = linear,
C                   1 = cubic, 2 = quintic, 3 = septic.
C
C  Modified by P.W. Draper, 25 November 1997. Was the FIGARO routine
C     INITEI. Renamed and calls & common blocks modified to RTD1_
C     namespace. 
C
C----------------------------------------------------------------------
      INTEGER*2 IORD
      INTEGER*2 MORD, MORD1, MORD12, MORD21
      INTEGER*2 IN, NT, M, N
      REAL*4    XT
C                             Everett Interpolation internal variables:
      REAL*4    BCOEF(49), SV(14)
      INTEGER*2 MORD2, IS0, NVALS
      COMMON /RTD1_CEVI/ BCOEF, SV, MORD2, IS0, NVALS
      COMMON /RTD1_STCOM/ MORD, MORD1, MORD12, MORD21
      SAVE /RTD1_CEVI/
      SAVE /RTD1_STCOM/
C-----------------------------------------------------------------------
      MORD = IORD
      MORD1 = MORD + 1
      MORD21 = MORD + MORD1
      MORD12 = MORD1 + MORD1
      MORD2 = MIN(IORD, 6)
      IN = 1
      NT = 0
      BCOEF(1) = 1.
C                                   Compute the BCOEF array:
      DO 50 M = 1,MORD2
         IN = IN + NT + 1
         NT = M + M
         XT = NT
         CALL RTD1_BINOM (XT, NT, BCOEF(IN))
C
         DO 40 N = 1,NT,2
            BCOEF(IN+N) = -BCOEF(IN+N)
 40         CONTINUE
 50      CONTINUE
C                                    Set up pointer constants:
      IS0   = MORD2 + 1
      NVALS = IS0  + IS0
C
 999  RETURN
      END
