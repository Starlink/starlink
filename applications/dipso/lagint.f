      SUBROUTINE LAGINT (X, Y, NPTS, XMIN, DX, XIN, YIN, NIN, PINT, OK)
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
C     DX   = Spacing of interpolated abscissae required.
C     PINT = Number of points to be used in interpolation (must be even).
C   All unchanged on exit.
C
C  Exports:
C     XIN  = Array of interpolated abscissa values.
C     YIN  = Array of interpolated ordinate values.
C     NIN  = Length of XIN and YIN arrays = Number of interpolated datapoints.
C     OK   = Success flag.
C
      IMPLICIT NONE
      LOGICAL OK
      INTEGER BS, I, I1, J, NIN, NPTS, ORDER, PINT, Z, NBIGAP
      DOUBLE PRECISION X(*), Y(*), XIN(*), YIN(*)
      DOUBLE PRECISION DEM, DX, MNXSP, NUM, XMIN
      DOUBLE PRECISION XT
C
      OK = .TRUE.
      MNXSP = (X(NPTS)-X(1))/DBLE(NPTS-1)
      I = 0
      BS = 0
      Z = 1
C
C  Interpolate for values XIN(1) to XIN(NIN).
C
      DO 1000, NIN = 1, 20000
        XT = XMIN + DX * DBLE( NIN - 1 )
        IF ( XT .LT. X(2) ) GOTO 1000
        IF ( XT .GT. X(NPTS-1) ) GOTO 1100
        I = I + 1
        XIN(I) = XT
        YIN(I) = 0.0D+00
        ORDER = PINT
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
        DO 100, I1 = (Z-ORDER/2), (Z-1+ORDER/2)
          NUM = 1.0D+00
          DEM = 1.0D+00
          DO 200, J = (Z-ORDER/2), (Z-1+ORDER/2)
            IF (J.NE.I1) THEN
              NUM = NUM*(XIN(I)-X(J))
              DEM = DEM*(X(I1)-X(J))
            ENDIF
  200     CONTINUE
          IF ( DABS(DEM) .GE. 1.0D-33 ) YIN(I) = YIN(I) + NUM/DEM*Y(I1)
  100   CONTINUE
 1000 CONTINUE
 1100 NIN = I
      IF (BS.NE.0) THEN
*       WRITE (*,'(/'' CAUTION:- '')')
*       WRITE(*,*)'Lower order interpolation used to obtain end points.'
*       WRITE(*,*)'There were ',BS,' endpoints.'
      ENDIF
      RETURN
      END
