      SUBROUTINE XCORRE (PMASK, LAGLO, LAGHI, WAVE1, F1, N1,
     +                  WAVE2, F2, N2, LAG, XCOR, NXC, OK)
C
C  Program evaluates cross-correlation of two functions F1 and F2.
C i.e routine evaluates XCOR(u) where
C       XCOR(u) = 1/(RmsF1*RmsF2) . integral[F1(x).F2(x+u)]dx
C
C  NB: WAVE1 and WAVE2 are assumed to be in similar units.
C      LAG, u,  is returned in the same units as WAVE1 and WAVE2.
C
      IMPLICIT NONE
C  Imports:-
      DOUBLE PRECISION PMASK               ! Fraction of data (either end) to be
C                                                    endmasked either end
      DOUBLE PRECISION LAGLO                            ! Lower end of LAG range
      DOUBLE PRECISION LAGHI                            ! Upper end of LAG range
C  Import/exports:-
      INTEGER N1, N2                    ! Number of datapairs in imported arrays
      DOUBLE PRECISION WAVE1(*), WAVE2(*)  ! "Wavelength" arrays => interpolated
      DOUBLE PRECISION F1(*), F2(*)              ! "Flux" arrays => interpolated
C  Exports:-
      INTEGER NXC                        ! Number of calculated (LAG,XCOR) pairs
      REAL LAG(*)                                          ! Array of lag values
      REAL XCOR(*)                           ! Array of cross-correlation values
      LOGICAL OK                                                  ! Success flag
C  Local:-
      INTEGER MAX                                 ! Largest value of data arrays
      PARAMETER ( MAX = 20000 )
      DOUBLE PRECISION SUM                        ! Mean spacing in WAVE1 values
      DOUBLE PRECISION IWAVE1(MAX)                   ! Interpolated WAVE1 values
      DOUBLE PRECISION IF1(MAX)                         ! Interpolated F1 values
      INTEGER NI1                                 ! Number of (IWAVE1,IF1) pairs
      DOUBLE PRECISION MS1                                          ! Rms of IF1
      DOUBLE PRECISION IWAVE2(MAX)                   ! Interpolated WAVE2 values
      DOUBLE PRECISION IF2(MAX)                         ! Interpolated F2 values
      INTEGER NI2                                 ! Number of (IWAVE2,IF2) pairs
      DOUBLE PRECISION MS2                                          ! Rms of IF2
      DOUBLE PRECISION START                ! Where to start interpolation of F2
      DOUBLE PRECISION A, A1, A2                         ! Normalising variables
      INTEGER XCLO                             ! Lower bound of LAG, XCOR arrays
      INTEGER XCHI                             ! Upper bound of LAG, XCOR arrays
      INTEGER Z                                              ! Alignment integer
      INTEGER I, I2, K                             ! Do-loop increment variables
C
      OK = .TRUE.
      IF ( (N1.LE.5) .OR. (N2.LE.5) ) THEN
        WRITE(*,*) '    XCORR:  insufficient data '
        OK = .FALSE.
        RETURN
      ENDIF
      IF ( LAGLO .GE. LAGHI ) THEN
        WRITE(*,*) '    XCORR:  zero or negative lag range '
        OK = .FALSE.
        RETURN
      ENDIF
C
C  Calculate mean Spacing in WAVE1 values, SUM.
C
      SUM = (WAVE1(N1)-WAVE1(1))/DBLE(N1-1)
      IF ( SUM .LE. 1.0D-10 ) THEN
        WRITE(*,*) '    XCORR:  mean data spacing is too small '
        OK = .FALSE.
        RETURN
      ENDIF
C
C  Interpolate datapairs (WAVE1,F1) onto grid with evenly spaced abscissae:
C (IWAVE1,IF1).
C  Spacing between WAVE values will be SUM; there will be NI1 interpolated
C datapairs.
C  NI1 will be less than N1 since some points will be lost during the
C interpolation.
C
        WRITE (*,
     :  '(''   XCORR:  interpolating first function...'')')
        CALL LAGINT (WAVE1, F1, N1, WAVE1(2), SUM,
     +                     IWAVE1, IF1, NI1, 4, OK)
        IF ( .NOT.OK ) RETURN
C
C  Interpolate datapairs (WAVE2,F2) onto same grid (IWAVE1,IF1).
C  Interpolation begins at START, which is set to be some multiple of SUM
C away from the beginning of the WAVE1 array to ensure the two functions
C are mapped onto the same abscissal axis.
C  Beginning interpolation at START ensures that as few as possible of the
C F2 datapairs are lost during the interpolation.
C  Obtain NI2 interpolated datapairs from function 2.
C
C  Find starting point (START) for function two interpolation:
C          START = IWAVE1(1) + n*SUM       ( where n is an integer ),
C            WAVE2(1) <= START <= WAVE2(2)
C
      START = IWAVE1(1) - DINT( (IWAVE1(1) - WAVE2(2)) / SUM ) * SUM
      WRITE (*,
     : '(''        ...and second function...'')')
      CALL LAGINT (WAVE2, F2, N2, START, SUM,
     +               IWAVE2, IF2, NI2, 4, OK)
      IF ( .NOT.OK ) RETURN
C
C  Endmask interpolated datasets.
C
      IF (PMASK.GE.1.0D-04) THEN
        CALL YENDMSK (IWAVE1, IF1, NI1, PMASK)
        CALL YENDMSK (IWAVE2, IF2, NI2, PMASK)
      ENDIF
C
C  Align the two interpolated abscissal arrays (F1 and F2 do not necessarily
C start at the same "wavelength" value!) Alignment integer is Z.
C
      Z = DNINT((IWAVE1(1)-IWAVE2(1))/SUM)
C
C  Obtain XCOR for each lag value.
C
      XCLO = INT( LAGLO/SUM ) - 1
      XCHI = INT( LAGHI/SUM ) + 1
      NXC = XCHI - XCLO + 1
      IF ( NXC .GE. MAX ) THEN
        WRITE(*,*) '    XCORR:  lag range is too wide '
        OK = .FALSE.
        RETURN
      ENDIF
C
      DO 200, I2 = 1, NXC
        I = I2 - 1 + XCLO
        LAG(I2) = REAL( SUM * DBLE(I) )
        XCOR(I2) = 0.0
        A1 = 0.0D+00
        A2 = 0.0D+00
        DO 100, K=1,NI1
          IF ( ((K+I+Z).GE.1) .AND. ((K+I+Z).LE.NI2) ) THEN
            A1 = A1 + IF1(K)**2
            A2 = A2 + IF2(K+I+Z)**2
            XCOR(I2) = REAL( DBLE(XCOR(I2)) + (IF1(K) * IF2(K+I+Z)) )
          ENDIF
  100   CONTINUE
        A = DSQRT( A1 * A2 )
        IF ( A .LE. 1.0D-20 ) THEN
          WRITE (*,
     :    '(''   XCORR:  zero normalisation at data end'')')
          XCOR(I2) = 0.0
        ELSE
          XCOR(I2) = REAL( DBLE(XCOR(I2)) / A )
        ENDIF
  200 CONTINUE
C
C  Write interpolated data into original data arrays for return to main program.
C
      N1 = NI1
      DO 300, I=1,N1
        WAVE1(I) = IWAVE1(I)
        F1(I) = IF1(I)
  300 CONTINUE
      N2 = NI2
      DO 400, I=1,N2
        WAVE2(I) = IWAVE2(I)
        F2(I) = IF2(I)
  400 CONTINUE
      RETURN
      END
