*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE DERED
*
*   DEREDDENS FLUX CALIBRATED DATA
*   USING SEATON (UV) AND HOWARTH (OPTICAL/IR) FITS
*
*   IMPORTS:
*     EBV      (REAL) E(B-V)
*     R        (REAL) RATIO OF Av/E(B-V)
*     GALLMC   (REAL) SWITCH - 0 = GALACTIC LAW, ELSE LMC
*     WAVE     (REAL) ARRAY OF WAVELENGTHS (Angstroms)
**    FLUX     (REAL) ARRAY OF FLUXES
*     NPNTS    (INTEGER)  SIZE OF WAVE, FLUX ARRAYS
*
*   EXPORTS:
*     (UPDATES IMPORTS MARKED '*')
*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE DERED(EBV,R,GALLMC,WAVE,FLUX,NPNTS)
*
*
*   DECLARATIONS
*
*
       IMPLICIT NONE
*
*
       INTEGER NPNTS
       INTEGER I
       INTEGER XRANGE
*
*
       REAL WAVE(NPNTS), FLUX(NPNTS)
       REAL EBV, R, GALLMC
       REAL INVW, BIGX
*
*
       DOUBLE PRECISION TEMP
*
*
*   CALCULATE X(x) [=BIGX] AT EACH POINT, & DEREDDEN
*
*
       XRANGE = 0
       IF (GALLMC.NE.0.0) WRITE (*,'('' Using LMC reddening'')')
       DO 100 I = 1, NPNTS
          INVW = 1.0/(WAVE(I)*1.E-4)
*   OUT OF RANGE?
          IF (INVW.GT.10.0 .OR. INVW.LT.0.0) THEN
             XRANGE = XRANGE + 1
          ENDIF
*   FIND RANGE & CALCULATE VALUES
          IF (INVW.GE.1.83) THEN
             IF (GALLMC.NE.0.0) THEN
*   LMC LAW
                IF (INVW.GE.2.75) THEN
                   BIGX = (INVW-4.557)**2 + 0.293
                   BIGX = (0.105*INVW+0.462)*INVW + 0.454/BIGX
                   BIGX = R - 0.236 + BIGX
                ELSE
                   BIGX = INVW - 1.83
                   BIGX = R + (0.094*BIGX+2.04)*BIGX
                ENDIF
*   GALACTIC LAW
             ELSEIF (INVW.GT.7.14) THEN
                BIGX = ((0.2975*INVW-3.2)*INVW) + 16.17
                BIGX = BIGX + R - 3.2
             ELSEIF (INVW.GT.3.65) THEN
                BIGX = (INVW-4.60)**2 + 0.28
                BIGX = 2.29 + 0.848*INVW + 1.01/BIGX
                BIGX = BIGX + R - 3.2
             ELSEIF (INVW.GT.2.75) THEN
                BIGX = (INVW-4.60)**2 + 0.28
                BIGX = 1.56 + 1.048*INVW + 1.01/BIGX
                BIGX = BIGX + R - 3.2
             ELSE
                BIGX = INVW - 1.83
                BIGX = R + (2.56-0.993*BIGX)*BIGX
             ENDIF
*   (COMMON) IR LAW
          ELSEIF (INVW.GE.0.0) THEN
             BIGX = ((1.86-0.48*INVW)*INVW-0.1)*INVW
             BIGX = BIGX*R/3.1
          ENDIF
*   GET DEREDDENED FLUX
          TEMP = ABS(FLUX(I))
          TEMP = LOG10(TEMP)
          TEMP = TEMP + 0.4*BIGX*EBV
          TEMP = 10.0**TEMP
          FLUX(I) = SIGN(REAL(TEMP),FLUX(I))
  100  CONTINUE
       IF (XRANGE.GT.0) THEN
          WRITE (*,'('' '',I5,'' points out of range of fits'')') XRANGE
       ENDIF
*
*
       END
