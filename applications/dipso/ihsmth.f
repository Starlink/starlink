*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE IHSMTH
*
*   APPLIES GAUSSIAN SMOOTHING
*   (DOES NOT ASSUME REGULAR 'Y' INTERVALS)
*
*   IMPORTS:
*     WAVE     (REAL)  ARRAY OF 'Y' VALUES
*     WORK     (REAL)  ARRAY OF 'X' VALUES
*     NPNTS    (INTEGER)  SIZE OF (WAVE, WORK, FLUX) ARRAYS
*     SIGMA    (REAL)  SIGMA FOR SMOOTHING FUNCTION
*
*   EXPORTS:
*     FLUX     (REAL)  ARRAY OF SMOOTHED 'X' VALUES
*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE IHSMTH(WAVE,WORK,FLUX,NPNTS,SIGMA)
*
*
*   DECLARATIONS
*
*
       IMPLICIT NONE
*
*
       INTEGER I, J
       INTEGER JSTART, J1
       INTEGER NPNTS
*
*
       REAL WAVE(NPNTS), WORK(NPNTS), FLUX(NPNTS)
       REAL SIGMA, SIG3
       REAL FLUXI, WTI, WV, W1, W2
       REAL GAUSWT, WTGAUS
       REAL WAV1, WAVN
       REAL W0, SIG, WX
*
*
       LOGICAL JTST
*
*
*   STATEMENT FUNCTION
*
*
       WTGAUS(W0,SIG,WX) = EXP(-0.5*((WX-W0)/SIG)**2)
*
*
*   INITIALISATIONS
*
*
       IF (SIGMA.LE.0.0) THEN
          DO 50 I = 1, NPNTS
             FLUX(I) = WORK(I)
   50     CONTINUE
          WRITE (*,'('' Sigma less than or equal 0 - no smooth'')')
          GOTO 300
       ENDIF
       SIG3 = 3.0*SIGMA
       JSTART = 1
       WAV1 = WAVE(1)
       WAVN = WAVE(NPNTS)
*
*
*   SMOOTH DATA OVER +/- 3 SIGMA
*
*
       DO 200 I = 1, NPNTS
          FLUXI = 0.0
          WTI = 0.0
          WV = WAVE(I)
          W1 = WV - SIG3
          W2 = WV + SIG3
          IF (W1.LT.WAV1) W1 = WAV1
          IF (W2.GT.WAVN) W2 = WAVN
          J1 = JSTART
          JTST = .TRUE.
          DO 100 J = J1, NPNTS
             IF (WAVE(J).GE.W1) THEN
                IF (JTST) THEN
                   JTST = .FALSE.
                   JSTART = J
                ENDIF
                IF (WAVE(J).GT.W2) GOTO 150
                GAUSWT = WTGAUS(WV,SIGMA,WAVE(J))
                FLUXI = FLUXI + WORK(J)*GAUSWT
                WTI = WTI + GAUSWT
             ENDIF
  100     CONTINUE
  150     CONTINUE
          IF (WTI.GT.0.0) THEN
             FLUX(I) = FLUXI/WTI
          ELSE
             FLUX(I) = 0.0
          ENDIF
  200  CONTINUE

  300  CONTINUE

       END
