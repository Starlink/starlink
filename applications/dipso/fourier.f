      SUBROUTINE FOURIER (X,Y,NP,PROP,INTFLG,FREQ,RE,IM,PS,XI,YI,HOLD
     +                     ,SHIFT,ENDM, OK)
      IMPLICIT NONE
      INTEGER I, J, NEG, NMAX, NP, NPT, HOLD
      INTEGER ICYCLE, NBIGAP
      DOUBLE PRECISION X(*), Y(*), XI(*), YI(*)
      DOUBLE PRECISION ENDM(*)
      DOUBLE PRECISION PROP
      DOUBLE PRECISION DX, XMAX, XMIN
      DOUBLE PRECISION PERIOD, CYCLES, SHIFT
      DOUBLE PRECISION FREQ(0:*), RE(0:*), IM(0:*), PS(0:*)
      LOGICAL INTFLG, OK

      OK = .TRUE.
C
C  Find appropriate minimum and maximum X-values for interpolation arrays.
C A couple of data points will probably be lost at either end.
C  X-spacing of interpolated array (DX) will equal the mean X-spacing of the
C original data.
C
      DX = (X(NP)-X(1))/DBLE(NP-1)
      XMIN = X(1) + DX
      XMAX = X(NP) - DX
C
C  Perform 4-point Laguerre interpolation on the data to equalise the X-
C spacings if INTFLG = .TRUE.
C  Set working data arrays XI and YI equal to input data arrays X and Y
C if INTFLG = .FALSE.
C
      IF (INTFLG) THEN
          WRITE (*,
     :    '(''   FTTRANS:  using 4-point Laguerre interpolation'')')
        CALL LAGINT4 (X,Y,NP,XMIN,XMAX,DX,XI,YI,NPT,4,OK)
        IF (.NOT.OK) RETURN
      ELSE
        NPT = NP
        DO 5, I=1,NPT
          XI(I) = X(I)
          YI(I) = Y(I)
    5   CONTINUE
      ENDIF
C
C  Shift interpolated data by an integer number of periods so that it straddles
C X = zero.
C
      PERIOD = XI(NPT) - XI(1)
      CYCLES = XI(1)/PERIOD
      IF (CYCLES.LE.0.0D+00) THEN
        CYCLES = CYCLES - 1.0D+00
      ENDIF
      ICYCLE = -(INT(CYCLES) + 1)
      SHIFT = DBLE(ICYCLE)*PERIOD
*     WRITE(*,*) '   FOURIER: Shifting x-axis by ',SHIFT
      NEG = 0
      HOLD = NPT
      DO 10,J=1,NPT
        XI(J) = XI(J) + SHIFT
        IF (XI(J).LT.0.0D+00) THEN
          NEG = NEG+1
        ENDIF
   10 CONTINUE
      NMAX = NPT-NEG-1
C
C  Endmask the interpolated data if the proportion to be endmasked at
C either end (PROP) is significantly greater than zero.
C
      IF (PROP.GT.1.0D-06) THEN
        CALL ENDMSK (XI,YI,NPT,PROP,ENDM)
      ENDIF
C
C  Fourier transform the interpolated, endmasked data.
C
      CALL FOURI (XI,YI,NPT,NEG,NMAX,DX,
     +              FREQ,RE,IM,PS)
      NP = NPT
      RETURN
      END
