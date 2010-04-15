       SUBROUTINE ZANSTRA(OK,ARRAY)
*
*
*   calculates black-body Zanstra temperatures
*
*   inputs:
*     line     (real):  identifier for emission line
*     fobs     (real):  observed flux
*                       (units = continuum units*Angstroms)
*     EB_V     (real):  E(B-V)
*                       (R=3.1, galactic law assumed)
*     TNEB     (real):  Electron temperature of plasma
*                       (in K or 10**4K)
*
*
*   X, Y values obtained from cursor hit (may be
*   in log or lin space).   These values refer to
*   a DEREDDENED (stellar) continuum level
*
*

       IMPLICIT NONE

       INTEGER I, J, K
       INTEGER ARG1
       INTEGER INDIX, INDEX, NUMIT
       INTEGER STRL

       REAL ARRAY(10)
       REAL LINE, FOBS, EB_V, TNEB
       REAL HKT, CC
       REAL XVAL, YVAL, YFLX
       REAL TEMP, U1, EPU1, RATBB, GTS, GTSCAL, RTEST
       REAL DELTAT, FLUX, RATCOM, BBINT, X
       REAL WEDGE(5), IDEN(5)

       CHARACTER*10 ID(5)
       CHARACTER*80 DUMMY

       LOGICAL OK
       LOGICAL FIRSTCURSOR

       PARAMETER (HKT=1.43883E+08,CC=2.997925E+18)

*  Local Data:
       DATA WEDGE/228., 228., 504., 504., 912./
       DATA IDEN/1640., 4686., 4471., 5876., 4861./
       DATA ID/'He IIX', 'He IIX', 'He IX', 'He IX', 'H IX'/
*
*   assign variables names
*

       LINE = ARRAY(1)
       FOBS = ARRAY(2)
       EB_V = ARRAY(3)
       TNEB = ARRAY(4)

*
*   identify ion
*

       IF (NINT(LINE).EQ.1640) THEN
          INDIX = 1
       ELSEIF (NINT(LINE).EQ.4686) THEN
          INDIX = 2
       ELSEIF (NINT(LINE).EQ.4471) THEN
          INDIX = 3
       ELSEIF (NINT(LINE).EQ.5876) THEN
          INDIX = 4
       ELSEIF (NINT(LINE).EQ.4861) THEN
          INDIX = 5
       ELSE
          OK = .FALSE.
          WRITE (*,'(''   ZANSTRA:  line not identified'')')
          GOTO 200
       ENDIF

*
*   determine 'continuum' point for normalisation

       CALL SGS_REQCU(XVAL,YVAL,I)
*
       FIRSTCURSOR = .TRUE.
       CALL SGSCURSE(I,XVAL,YVAL,FIRSTCURSOR)
       FIRSTCURSOR = .FALSE.

*
*   convert to 'real' units as necessary
*

       IF (TNEB.LT.100.0) TNEB = TNEB*10000.0
       IF (YVAL.LT.0.0) YVAL = 10.0**YVAL
       IF (XVAL.LT.10.0) XVAL = 10.0**XVAL

*
*   deredden fluxes, calculate ratio of recombination rates
*

       IF (INDIX.EQ.1) THEN
          FLUX = LOG10(FOBS) + 7.761*EB_V/2.5
          RATCOM = 2.0*(TNEB/10000.0)**0.1
       ELSEIF (INDIX.EQ.2) THEN
          FLUX = LOG10(FOBS) + 3.787*EB_V/2.5
          RATCOM = 4.37*(TNEB/10000.0)**0.29
       ELSEIF (INDIX.EQ.3) THEN
          FLUX = LOG10(FOBS) + 3.977*EB_V/2.5
          RATCOM = 19.61*(TNEB/10000.0)**0.27
       ELSEIF (INDIX.EQ.4) THEN
          FLUX = LOG10(FOBS) + 2.851*EB_V/2.5
          RATCOM = 5.39*(TNEB/10000.0)**0.39
       ELSE
          FLUX = LOG10(FOBS) + 3.630*EB_V/2.5
          RATCOM = 8.49*(TNEB/10000.0)**0.06
       ENDIF
       FLUX = 10.0**FLUX

*
*   observed value of G(Ts)
*

       GTS = (FLUX*IDEN(INDIX)*RATCOM)/(YVAL*XVAL**2)

*
*   iterate to obtain black-body temperature which
*   reproduces the observed G(Ts)
*

       TEMP = 50000.0
       DELTAT = 10000.0
       NUMIT = 0

  100  CONTINUE
       IF (NUMIT.GT.500) THEN
          WRITE (*,'(''   ZANSTRA:  failed to converge'')')
          OK = .FALSE.
          GOTO 200
       ENDIF
       NUMIT = NUMIT + 1
       TEMP = TEMP + DELTAT
       U1 = HKT/(XVAL*TEMP)
       IF (U1.LT.88.0 .AND. U1.GT.1.0E-07) THEN
          EPU1 = EXP(U1) - 1
          RATBB = EPU1/U1**3
       ELSE
          OK = .FALSE.
          WRITE (*,
     :    '(''   ZANSTRA:   numerical error due to extreme '',
     :    ''temperature of '',1PE12.5)') TEMP
          GOTO 200
       ENDIF
       GTSCAL = RATBB*BBINT(WEDGE(INDIX),TEMP,OK)
       IF (.NOT.OK) GOTO 200
       IF (NUMIT.LE.1) GOTO 100
       RTEST = GTSCAL/GTS
       IF (ABS(RTEST-1.0).GT.0.001) THEN
          IF (DELTAT.GT.0.0) THEN
             IF (RTEST.GT.1.0) DELTAT = -DELTAT/3.0
          ELSE
             IF (RTEST.LT.1.0) DELTAT = -DELTAT/3.0
          ENDIF
          GOTO 100
       ENDIF

*
*   calculate normalisation constant at cursor point
*

       ARG1 = 1
       CALL BBODY(XVAL,YFLX,ARG1,ARG1,DUMMY,TEMP,OK)
       IF (OK) THEN
          YVAL = LOG10(YVAL) - LOG10(YFLX)
          IF (ABS(YVAL).LT.35.0) THEN
             YVAL = 10.0**YVAL
          ELSE
             OK = .FALSE.
          ENDIF
       ENDIF
       IF (.NOT.OK) THEN
          WRITE(*,'(''   ZANSTRA:   T(BB) ='',1PE12.5)') TEMP
          WRITE(*,'(''              Error calculating normalisation'')')
          GOTO 200
       ENDIF

*
*
*

       WRITE (*,'(''   ZANSTRA:  '',A,1X,F5.0)') ID(INDIX)
     :        (1:(INDEX(ID(INDIX),'X')-1)), IDEN(INDIX)
       WRITE (*,'(''             T(BB) ='', 1PE12.5)') TEMP
       WRITE (*,'(''          C(norm.) ='', 1PE12.5)') YVAL

  200  CONTINUE

       END
