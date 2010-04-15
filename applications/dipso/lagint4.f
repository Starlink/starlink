      SUBROUTINE LAGINT4 (X,Y,NPTS,XMIN,XMAX,DX,XIN,YIN,NIN,PINT,OK)
C
C  Subroutine applies LAGUERRE Interpolation to imported arrays of datapairs.
C  X-values do not need to be equally spaced for valid interpolation by this
C method.
C  Data is interpolated into fresh arrays; the new abscissae are equally spaced.
C
C  Imports:
C     X    = Array of abscissa values.
C     Y    = Array of ordinate values.
C     NPTS = Length of X and Y arrays = Number of imported datapoints.
C     XMIN = Lower range of interpolated abscissae required.
C     XMAX = Upper range of interpolated abscissae required.
C     DX   = Spacing of interpolated abscissae required.
C     PINT = Number of points to be used in interpolation (must be even).
C   All unchanged on exit.
C
C  Exports:
C     XIN  = Array of interpolated abscissa values.
C     YIN  = Array of interpolated ordinate values.
C     NIN  = Length of XIN and YIN arrays = Number of interpolated datapoints.
C
      IMPLICIT NONE
      INTEGER BS, I, I1, J, NIN, NPTS, ORDER, PINT, Z
      DOUBLE PRECISION X(*), Y(*), XIN(*), YIN(*)
      DOUBLE PRECISION DEM, DX, MNXSP, NUM, XMAX, XMIN
      LOGICAL OK
      INTEGER NBIGAP
C
      MNXSP = (X(NPTS)-X(1))/DBLE(NPTS-1)
      NIN = INT((XMAX-XMIN)/DX) + 1
      XIN(1) = XMIN
      BS=0
      Z=1
C
C  Interpolate for values XIN(1) to XIN(NIN).
C
      DO 1000, I=1,NIN
        YIN(I) = 0.0D+00
        ORDER = PINT
        IF (I.GT.1) THEN
          XIN(I) = XIN(I-1) + DX
        ENDIF
        IF ((XIN(I).LT.X(1)).OR.(XIN(I).GT.X(NPTS))) THEN
          WRITE (*,
     :    '(''   Error in subroutine LAGINT;''/
     :      ''   attempt to extrapolate past end of data''/
     :      ''   Please report to ZUVAD::IDH'',A)') 7
*         WRITE(*,'('' Attempt to extrapolate past end of data. '')')
          OK = .FALSE.
          RETURN
        ENDIF
    1   IF((XIN(I).LT.X(ORDER/2)).OR.(XIN(I).GT.X(NPTS+1-ORDER/2)))THEN
          BS = BS+1
          ORDER = ORDER - 2
          GOTO 1
        ENDIF
   10   IF (XIN(I).GT.X(Z)) THEN
          Z=Z+1
          GOTO 10
        ENDIF
        NBIGAP = 0
        IF (((X(Z)-X(Z-1)).GE.(3.0D+00*MNXSP)).AND.(ORDER.GE.4)) THEN
          ORDER = ORDER - 2
*         WRITE(*,*) 'Gap in data abscissa spacing at least three times
*    + mean spacing detected.'
          NBIGAP = NBIGAP + 1
*         WRITE(*,*) '; switching to ',ORDER,' - point interpolation '
*         WRITE(*,*) 'to evaluate interpolated point ',I
        ENDIF
        IF (NBIGAP.GT.0) THEN
           WRITE (*,
     :     '(''   FTTRANS:  interpolation order degraded'',I3,
     :     '' time(s) to account for gappy data'')')
        ENDIF
        DO 100, I1 = (Z-ORDER/2), (Z-1+ORDER/2)
          NUM = 1.0D+00
          DEM = 1.0D+00
          DO 200, J = (Z-ORDER/2), (Z-1+ORDER/2)
            IF (J.NE.I1) THEN
              NUM = NUM*(XIN(I)-X(J))
              DEM = DEM*(X(I1)-X(J))
            ENDIF
  200     CONTINUE
          IF (DABS(DEM).LT.1.0D-35) THEN
*           WRITE(*,*) 'Error arose when trying to carry out Laguerre
*    + interpolation: '
*           WRITE(*,*) 'Zero denominator obtained for point ',I
            WRITE (*,
     :      '(''   FTTRANS: division by zero during interpolation'')')
            OK = .FALSE.
            RETURN
          ENDIF
          YIN(I) = YIN(I) + NUM/DEM * Y(I1)
  100   CONTINUE
 1000 CONTINUE
      IF (BS.NE.0) THEN
*       WRITE (*,'(/'' CAUTION:- '')')
*       WRITE(*,*)'Lower order interpolation used to obtain end points.'
*       WRITE(*,*)'There were ',BS,' endpoints.'
        WRITE (*,
     :  '(''   FTTRANS: low order interpolation used to'',
     :   '' obtain'',I3,'' endpoints'')')BS
      ENDIF
      RETURN
      END
