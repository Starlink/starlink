      SUBROUTINE FIG_ETERP (DX, DY, NX, NY, DATA, OUTVAL)
C-----------------------------------------------------------------------
C  Subroutine to do Everett interpolation.  INITEI should be called once
C  with the order of the interpolation desired to set up variables in
C  common before any subsequent calls to this routine.
C  INPUTS:
C    DX      R*4      X position.
C    DY      R*4      Y position.
C    NX      I*4      Number of X pts in DATA.
C    NY      I*4      Number of Y pts in DATA.
C    DATA    R*4      Image data array.
C  OUTPUTS:
C    OUTVAL  R*4      Interpolated value.
C  Note: This routine is based closely on the routine ETERP from the AIPS
C  program SLICE, but has been simplified for Figaro since it can now
C  assume that all the data array is available.
C                                                KS / CIT 22nd March 1984
C
C     26th Jul 1994.   Make common blocks SAVE. HME / UoE, Starlink.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER*4 NX, NY
      REAL*4 DATA(NX,NY), DX, DY, SVX(14), SVY(14), OUTVAL, XSUM, ZIKP
      INTEGER*2 IXI, IYI, K, MORD, MORD1, MORD12, MORD21
      INTEGER*2 J, JP, KP
      COMMON /STCOM/ MORD, MORD1, MORD12, MORD21
      SAVE /STCOM/
C-----------------------------------------------------------------------
      OUTVAL = 0.0
      CALL IEVERT (DX, IXI, SVX)
      CALL IEVERT (DY, IYI, SVY)
C                                       Loop on y-interpolation:
      DO 130 J = 1 , MORD12
         JP = J + IYI
         IF ((JP.LT.1).OR.(JP.GT.NY)) GO TO 130
         XSUM = 0.0
C                                       Do an x-interpolation:
         DO 120 K = 1, MORD12
            KP = K + IXI
            IF ((KP.LT.1).OR.(KP.GT.NX)) GO TO 120
            ZIKP = DATA(KP,JP)
            XSUM = XSUM + (ZIKP * SVX(K))
 120        CONTINUE
         OUTVAL = OUTVAL + (XSUM * SVY(J))
 130     CONTINUE
      RETURN
      END
      SUBROUTINE INITEI (IORD)
C----------------------------------------------------------------------
C   INITEI computes certain quantities which are needed by IEVERT
C      when it computes the actual weights for an interpolation. The
C      result produced by INITEI is in the BCOEF array in the COMMON
C      block, and is based on binomial coefficients computed by BINOM.
C      From the Everett interpolation package originally coded by
C      Larry Goad at KPNO.  This version has been lifted almost unchanged
C      from the AIPS package - the only change has been that the include
C      files are now explicitly included in the program body.
C  INPUT:
C      IORD    I*2  The order of the interpolation, 0 = linear,
C                   1 = cubic, 2 = quintic, 3 = septic.
C----------------------------------------------------------------------
      INTEGER*2 IORD
      INTEGER*2 MORD, MORD1, MORD12, MORD21
      INTEGER*2 IN, NT, M, N
      REAL*4    XT
C                             Everett Interpolation internal variables:
      REAL*4    BCOEF(49), SV(14)
      INTEGER*2 MORD2, IS0, NVALS
      COMMON /CEVI/ BCOEF, SV, MORD2, IS0, NVALS
      COMMON /STCOM/ MORD, MORD1, MORD12, MORD21
      SAVE /CEVI/
      SAVE /STCOM/
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
         CALL BINOM (XT, NT, BCOEF(IN))
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
      SUBROUTINE IEVERT (DX, IX, SVECT)
C----------------------------------------------------------------------
C   Compute subscript offset and weights for interpolating at a
C   specified position in a vector.  Interpolation is done using a
C   group of pixels centered on the specified position. The order of
C   interpolation is specified by integer MORD in subroutine SETCOF.
C   We do linear interpolation for MORD=0, cubic for 1, and quintic
C   for 2. If MORD=1 (i.e., cubic interpolation) we will be using
C   four pixels in the interpolation.  From the Everett interpolation
C   package originally coded by Larry Goad at KPNO. This version has been
C   lifted almost unchanged from the AIPS package - the only change has
C   been that the include files are now explicitly included in the
C   program body.                              KS / CIT  22nd MArch 1984
C
C   Inputs:  DX     R*4     Position in vector
C   Outputs: IX     I*2     Offset to start
C            SVECT  R*4(*)  Weights
C----------------------------------------------------------------------
      INTEGER*2 IX
      REAL*4    SVECT(1), DX
      INTEGER*2 NR, IR0, IV0, MC, IR
      REAL*4    U, W, CW, CU, W2, U2, R0
C                             Everett Interpolation internal variables:
      REAL*4    BCOEF(49), SV(14)
      INTEGER*2 MORD2, IS0, NVALS
      COMMON /CEVI/ BCOEF, SV, MORD2, IS0, NVALS
      SAVE /CEVI/
      DATA R0 /0.0/
C-----------------------------------------------------------------------
      IX = DX
      U  = DX - IX
      IX = IX - IS0
      DO 5 IR = 1,NVALS
         SVECT(IR) = R0
    5    CONTINUE
C                                       distance of point from cells
      W  = 1.0 - U
      CW = W
      CU = U
      W2 = W * W
      U2 = U * U
      SVECT(IS0)   = W
      SVECT(IS0+1) = U
C                                       leave if on cell or linear
      IF ((U.EQ.0) .OR. (MORD2.EQ.0)) GO TO 999
      NR  = 1
      IR0 = 0
      IV0 = IS0 - 1
      DO 20 MC = 1,MORD2
C                                       (IR0=MC*MC):
         IR0 = IR0 + NR
C                                       (NR=2*MC+1):
         NR  = NR  + 2
C                                       (IV0=IS0-MC-1):
         IV0 = IV0 - 1
         CU  = CU * (U2 - IR0) / ((IR0 + IR0 + MC) + (IR0 + IR0 + MC))
         CW  = CW * (W2 - IR0) / ((IR0 + IR0 + MC) + (IR0 + IR0 + MC))
         DO 10 IR = 1,NR
            SVECT(IV0+IR)   = SVECT(IV0+IR)   + CW * BCOEF(IR0+IR)
            SVECT(IV0+IR+1) = SVECT(IV0+IR+1) + CU * BCOEF(IR0+IR)
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE BINOM (X, M, VAL)
C----------------------------------------------------------------------
C   BINOM generates binomial coefficients for use in the Everett
C   interpolation routines. It is called only by SETCOF.  From the
C   Everett interpolation package originally coded by Larry Goad, KPNO.
C----------------------------------------------------------------------
      INTEGER*2  M
      REAL*4     X, VAL(1)
      INTEGER*2  I
      REAL*4     R, XL
C-----------------------------------------------------------------------
      VAL(1) = 1.
      R = 0.
      XL = X + 1.
C
      DO 20 I = 1,M
         XL = XL - 1.
         R = R + 1.
         VAL(I+1) = VAL(I) * XL / R
 20      CONTINUE
C
      RETURN
      END
