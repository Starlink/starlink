      SUBROUTINE LAGUERRE (X, Y, NPTS, XMIN, XMAX, DX, PINT, OK)
C
C  Subroutine applies EVEN ORDER LAGUERRE Interpolation to imported arrays of
C datapairs.
C  X-values do not need to be equally spaced for valid interpolation by this
C method.
C
      IMPLICIT NONE
C Imports:
      DOUBLE PRECISION XMIN, XMAX             ! X-range of interpolated data
      DOUBLE PRECISION DX                     ! X-spacing of interpolated data
      INTEGER PINT                            ! Order of interpolation
      INTEGER NBIGAP
C Import/exports:
      INTEGER NPTS        ! No. imported datpairs -> no. interpolated datapairs
      DOUBLE PRECISION X(*), Y(*)          ! Imported data -> interpolated data
C Exports:
      LOGICAL OK                              ! Success flag
C Local:
      DOUBLE PRECISION XIN(20000), YIN(20000) ! Working data arrays
      DOUBLE PRECISION MNXSP                  ! Mean X-spacing of imported data
      DOUBLE PRECISION DEM, NUM               ! Denominator, numerator accums.
      INTEGER NIN                           ! No. of interpolated points holder
      INTEGER Z                               ! Data position marker
      INTEGER ORDER                           ! Temporary changed order holder
      INTEGER BS                              ! No. lower order points accum.
      INTEGER I, I1, J                        ! Do-loop increment variables
      LOGICAL EVEN                            ! Odd-or-even value testing func.
C
      OK = EVEN(PINT)
      IF (.NOT.OK) THEN
        WRITE(*,*) '    INTERP ERROR: Order was odd '
        RETURN
      ENDIF
      MNXSP = (X(NPTS)-X(1))/DBLE(NPTS-1)
*     WRITE(*,'(A43,E10.4)') ' LAGINT : Mean X-spacing of RAW
*    +data was ', MNXSP
      NIN = INT((XMAX-XMIN)/DX) + 1
      IF ( NIN.GT.20000 ) THEN
        WRITE(*,*)'    INTERP:  error - too many interpolated points '
        OK = .FALSE.
        RETURN
      ENDIF
      XIN(1) = XMIN
      BS=0
      Z=1
C
C  Interpolate for values XIN(1) to XIN(NIN).
C
      NBIGAP = 0
      DO 100, I=1,NIN
        YIN(I) = 0.0D+00
        ORDER = PINT
        IF (I.GT.1) THEN
          XIN(I) = XIN(I-1) + DX
        ENDIF
        IF ((XIN(I).LT.X(1)).OR.(XIN(I).GT.X(NPTS))) THEN
          WRITE(*,*) '    INTERP:  error - interpolation beyond data '
          OK = .FALSE.
          RETURN
        ENDIF
    1   IF((XIN(I).LT.X(ORDER/2)).OR.(XIN(I).GT.X(NPTS+1-ORDER/2)))THEN
          BS = BS+1
          ORDER = ORDER - 2
          GOTO 1
        ENDIF
    2   IF (XIN(I).GT.X(Z)) THEN
          Z=Z+1
          GOTO 2
        ENDIF
        IF (((X(Z)-X(Z-1)).GE.(3.0D+00*MNXSP)).AND.(ORDER.GE.4)) THEN
          ORDER = ORDER - 2
*         WRITE(*,*) 'LAGINT: Gap in data abscissa spacing at least three
*    + times mean spacing '
*         WRITE(*,*) 'detected; switching to ',ORDER,' - point interpolat
*    +ion to evaluate '
*         WRITE(*,*) 'interpolated point ',I
          NBIGAP = NBIGAP + 1
        ENDIF
        DO 20, I1 = (Z-ORDER/2), (Z-1+ORDER/2)
          NUM = 1.0D+00
          DEM = 1.0D+00
          DO 10, J = (Z-ORDER/2), (Z-1+ORDER/2)
            IF (J.NE.I1) THEN
              NUM = NUM*(XIN(I)-X(J))
              DEM = DEM*(X(I1)-X(J))
            ENDIF
   10     CONTINUE
          IF (DABS(DEM).LT.1.0D-33) THEN
*           WRITE(*,*) '    LAGINT ERROR: '
*           WRITE(*,*) 'Zero denominator obtained for point ',I
            WRITE (*,
     :      '(''   INTERP:  division by zero during interpolation'')')
            OK = .FALSE.
            RETURN
          ENDIF
          YIN(I) = YIN(I) + NUM/DEM * Y(I1)
   20   CONTINUE
  100 CONTINUE
        IF (NBIGAP.GT.0) THEN
         WRITE (*,
     :   '(''   INTERP:  low-order interpolation used to span'',
     :   I3,'' large gaps'')') NBIGAP
        ENDIF
      NPTS = NIN
      DO 200, I=1,NPTS
        X(I) = XIN(I)
        Y(I) = YIN(I)
  200 CONTINUE
      IF (BS.NE.0) THEN
*       WRITE (*,'('' LAGINT CAUTION:- '')')
*       WRITE(*,*)'Lower order interpolation used to obtain end points.'
*       WRITE(*,*)'There were ',BS,' endpoints.'
        WRITE (*,
     :  '(''   INTERP: low order interpolation used at'',I3,
     :  '' endpoint(s)'')')BS
      ENDIF
      RETURN
      END
