       SUBROUTINE FCALLER
     : (CMD, PARAMS, MAXPT, MAXBRK, X, Y, NPT,
     :  BRKS, NBRK, TITLE, OK)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      CHARACTER*(*) CMD                ! Command name
      CHARACTER*(*) PARAMS             ! Parameter value string
      INTEGER MAXPT                    ! Maximum number of spectrum points
      INTEGER MAXBRK                   ! Maximum number of break points
*    Import-Export :
      REAL X(*)                        ! X-values
      REAL Y(*)                        ! Y-values
      INTEGER NPT                      ! Number of spectrum points
      INTEGER BRKS(*)                  ! Break points
      INTEGER NBRK                     ! Number of break points
      INTEGER SJBRKS(200)              ! Stack data break points
      INTEGER SNBRKS                   ! Number of stack break points
      CHARACTER*(*) TITLE              ! Associated character string
      CHARACTER*79 STITLE, TITLEB      ! Temporary character string
*    Status :
      LOGICAL OK                       ! Whether DECODE completed OK
*    Local constants :
      INTEGER MAXVAL                   ! Maximum number of parameter values
      PARAMETER(MAXVAL=10)
*    Local variables :
      REAL VALUES(MAXVAL)              ! Array for parameter values
      REAL WORV                        ! Wavelength OR Velocity
      REAL TSTVAL, RLIDH
      REAL SWORV,SWORV2                ! shift applied to data by FTRANS
      DOUBLE PRECISION DWORV           ! double precision SWORV
      INTEGER ARMAX                    ! maximum size of various arrays
      PARAMETER ( ARMAX = 20000 )
      REAL SJX(ARMAX), SJY(ARMAX)      ! working arrays for (x,y) datapairs
      REAL SHIX(ARMAX)                 ! array of re-shifted abscissae
      REAL ENDM(ARMAX)                 ! array of endmasking weights
      DOUBLE PRECISION DENDM(ARMAX)    ! doub. prec. version of ENDM
      DOUBLE PRECISION DX(ARMAX), DY(ARMAX)   ! double precision working arrays
      DOUBLE PRECISION EIX(ARMAX), EIY(ARMAX) ! interpolated and endmasked data
      DOUBLE PRECISION FREQ(0:(ARMAX-1))      ! array of frequeny values
      DOUBLE PRECISION RE(0:(ARMAX-1))        ! array of real part of transform
      DOUBLE PRECISION IM(0:(ARMAX-1))        ! array of imaginary part of transform
      DOUBLE PRECISION PS(0:(ARMAX-1))        ! array of power spectrum
      REAL REIX(ARMAX), REIY(ARMAX)           ! real versions of EIX, EIY
      REAL RFREQ(ARMAX)                ! real version of FREQ
      REAL RRE(ARMAX)                  ! real version of RE
      REAL RIM(ARMAX)                  ! real version of IM
      REAL RPS(ARMAX)                  ! real version of PS
      DOUBLE PRECISION PMASK           ! Proportion of data either end to be
     +                                 !    masked with a cosine bell
      REAL XDIFF, DIFMAX, DIFMIN       ! used to test for constant abscissal spacing
      INTEGER SAVM                     ! save mode marker (which Fourier data to save)
      LOGICAL INTFLG                   ! Whether or not to interpolate data
      REAL FC, FAC                     ! Temporary FTFILTER variables
      INTEGER I! Loop index
      INTEGER INDEX, INDEXB            ! Stack indices for FTRANS and FTFILTER
      INTEGER NPTS, NBRKS              ! Temporary FTRANS variables
      INTEGER HOLD                     ! Number of points in interpolate, endmasked data
      REAL SD(ARMAX)                   ! POLFIT error array
      DOUBLE PRECISION DSD(ARMAX)      ! Doub. prec. version of SD
      DOUBLE PRECISION CHISQ, SCAT, COEFS(20)  ! Used by POLFIT
      INTEGER ORDER, MODE              ! POLFIT variables
      REAL VEL(ARMAX), XCOR(ARMAX)     ! XCOR arrays
      INTEGER ASIZE, BSIZE
*-
*    Assume OK until error is detected
      OK = .TRUE.
*    Assume command found until it is not
*
*  Compare command string to the ones handled here; act if found.
*
C  Command FTRANS
C
      IF (CMD.EQ.'FTRANS') THEN
         VALUES (2) = 0.05
         CALL DECODE (CMD, PARAMS, 1, 2, VALUES,
     :   'Entry ',OK)
         IF (OK) THEN
           INDEX = NINT(VALUES(1))
           PMASK = DBLE(VALUES(2))
           IF ((PMASK.LT.0.0D+00).OR.(PMASK.GT.0.5D+00)) THEN
             WRITE(*,*) '   FTRANS: Invalid Pmask value '
             OK = .FALSE.
             GOTO 10
           ENDIF
           IF (INDEX.NE.0) THEN
             NPTS = ARMAX
             SNBRKS = 200
             CALL GETSTK (INDEX, NPTS, SJX, SJY,
     +          SNBRKS, SJBRKS, STITLE, SWORV, OK)
             IF (.NOT.OK) GOTO 10
           ELSE
             NPTS = NPT
             DO 4, I = 1,NPTS
               SJX(I) = X(I)
               SJY(I) = Y(I)
    4        CONTINUE
           ENDIF
           IF (NPTS .GT. ARMAX) THEN
             WRITE(*,*) '   FTRANS: too many datum pairs '
             WRITE(*,*) '               - maximum is ',ARMAX
             OK = .FALSE.
             GOTO 10
           ENDIF
           STITLE = ' Shifted endmasked data '
           TITLEB = ' Endmasked data: shift removed '
C
C  Should data be interpolated ?
C  Find smallest and largest interval in the abscissal array (DIFMIN
C and DIFMAX respectively).
C  IF (DIFMAX-DIFMIN)/DIFMAX is not negligible, the data is not evenly
C spaced and must be interpolated.
C
           DIFMAX = 1.0E-35
           DIFMIN = 1.0E+35
           DO 3, I = 2, NPTS
             XDIFF = SJX(I) - SJX(I-1)
             IF (ABS(SJX(I)).GE.1.0E-30) THEN
              IF ((XDIFF/ABS(SJX(I))).LT.-1.0E-06) THEN
               WRITE(*,*)'   FTRANS: abscissae not in increasing order'
               OK = .FALSE.
               GOTO 10
              ENDIF
             ENDIF
             IF (XDIFF.GT.DIFMAX) THEN
               DIFMAX = XDIFF
             ENDIF
             IF (XDIFF.LT.DIFMIN) THEN
               DIFMIN = XDIFF
             ENDIF
    3      CONTINUE
           IF ( ABS((DIFMAX-DIFMIN)/DIFMAX).GT.1.0E-02 ) THEN
             WRITE(*,*) '   FTRANS: data are unevenly spaced '
             INTFLG = .TRUE.
             STITLE = ' Interpolated, shifted, endmasked data '
             TITLEB = ' Interpolated, endmasked data: shift removed '
           ELSE
             WRITE(*,*) '   FTRANS: data are evenly spaced: will not
     + interpolate'
             INTFLG = .FALSE.
           ENDIF
C
C  Write data into double precision arrays.
C
           DO 5, I = 1, NPTS
             DX(I) = DBLE(SJX(I))
             DY(I) = DBLE(SJY(I))
    5      CONTINUE
           CALL FOURIER (DX,DY,NPTS,PMASK,INTFLG,FREQ,RE,IM,PS,EIX,EIY
     +                       ,HOLD,DWORV,DENDM,OK)
C
C  Write interpolated, endmasked data into single precision arrays.
C  Reverse abscissal shift (held in SWORV) to facility comparison of
C original and interpolated data.
C
           SWORV = SNGL(DWORV)
           DO 55, I = 1,HOLD
             REIX(I) = SNGL(EIX(I))
             REIY(I) = SNGL(EIY(I))
             SHIX(I)  =SNGL(EIX(I)-DWORV)
             ENDM(I) = SNGL(DENDM(I))
   55      CONTINUE
C
C  Write transforms into single precision arrays for pushing onto stack.
C
           DO 6, I = 1, NPTS
             RFREQ(I) = SNGL(FREQ(I-1))
             RRE(I) = SNGL(RE(I-1))
             RIM(I) = SNGL(IM(I-1))
             RPS(I) = SNGL(PS(I-1))
    6      CONTINUE
           ASIZE = ARMAX
           BSIZE = 1
           SNBRKS = 1
           SJBRKS(1) = NPTS
           SWORV = SHIX(1)
           STITLE = ' Real part of Fourier Transform (RE) '
           CALL UPUSH (ASIZE, RFREQ, RRE, NPTS, BSIZE, SJBRKS, SNBRKS,
     +                 STITLE, SWORV, OK)
           STITLE = ' Imaginary part of Fourier Transform (IM) '
           CALL UPUSH (ASIZE, RFREQ, RIM, NPTS, BSIZE, SJBRKS, SNBRKS,
     +                 STITLE, SWORV, OK)
           STITLE = ' Power Spectrum = SQRT( RE**2 + IM**2 ) '
           CALL UPUSH (ASIZE, RFREQ, RPS, NPTS, BSIZE, SJBRKS, SNBRKS,
     +                 STITLE, SWORV, OK)
         ELSE
   10      CONTINUE
         ENDIF
C
C  Command FTFILTER
C
      ELSEIF (CMD.EQ.'FTFILTER') THEN
          TSTVAL = -654321.789
          VALUES(2) = TSTVAL
          VALUES(3) = TSTVAL
          VALUES(4) = TSTVAL
         CALL DECODE
     :   (CMD, PARAMS, 1, 4, VALUES,
     :   ' Real_part_entry ', OK)
         IF (VALUES(4).EQ.TSTVAL) VALUES(4) = VALUES(1)+1.0
         IF (OK) THEN
             INDEX = NINT(VALUES(1))
             INDEXB = NINT(VALUES(4))
           NPTS = 2001
           HOLD = 2001
           SNBRKS = 200
           CALL GETSTK (INDEX, NPTS, RFREQ, RRE, SNBRKS, SJBRKS,
     + STITLE, SWORV, OK)
           IF (.NOT.OK) GOTO 20
           CALL GETSTK (INDEXB, HOLD, RFREQ, RIM, SNBRKS, SJBRKS,
     + STITLE, SWORV2, OK)
           IF (.NOT.OK) GOTO 20
           IF ((HOLD.NE.NPTS).OR.
     +         (ABS((SWORV-SWORV2)/SWORV).GE.1.0E-05)) THEN
             WRITE (*,
     :     '(''   FTFILTER:  real & imaginary data mismatched'')')
             OK = .FALSE.
             GOTO 20
           ENDIF

           IF (VALUES(2).EQ.TSTVAL) THEN
              FC = (RFREQ(1)+RFREQ(NPTS))*0.5
           ELSE
              FC = VALUES(2)
           ENDIF
           IF (VALUES(3).EQ.TSTVAL) THEN
              FAC = MIN(RFREQ(NPTS),1.1*FC)
           ELSE
              FAC = VALUES(3)
           ENDIF
           CALL FILTER (RFREQ, RRE, RIM, NPTS, FC, FAC, RPS)
           ASIZE = ARMAX
           BSIZE = 1
           SNBRKS = 1
           SJBRKS(1) = NPTS
           STITLE = 'Filtered Real part of Fourier Transform'
           CALL UPUSH (ASIZE, RFREQ, RRE, NPTS, BSIZE, SJBRKS, SNBRKS,
     + STITLE, SWORV, OK)
           STITLE = 'Filtered Imaginary part of Fourier Transform'
           CALL UPUSH (ASIZE, RFREQ, RIM, NPTS, BSIZE, SJBRKS, SNBRKS,
     + STITLE, SWORV, OK)
           STITLE ='Weighting Function used to filter Fourier Transform'
           CALL UPUSH (ASIZE, RFREQ, RPS, NPTS, BSIZE, SJBRKS, SNBRKS,
     + STITLE, SWORV, OK)
         ELSE
   20      CONTINUE
         ENDIF
C
C  Command FTINV (reverse Fourier transform).
C
      ELSEIF (CMD.EQ.'FTINV') THEN
         VALUES(2) = 0.0
         CALL DECODE
     :   (CMD,PARAMS,1,2,VALUES,'Real_part_entry ',OK)
         IF (VALUES(2).EQ.0.0) VALUES(2) = (VALUES(1)+1.)
         IF (OK) THEN
           INDEX = NINT(VALUES(1))
           INDEXB = NINT(VALUES(2))
           NPTS = 2001
           HOLD = 2001
           SNBRKS = 200
           CALL GETSTK (INDEX, NPTS, RFREQ, RRE, SNBRKS, SJBRKS,
     + STITLE, SWORV, OK)
           IF (.NOT.OK) GOTO 30
           CALL GETSTK (INDEXB, HOLD, RFREQ, RIM, SNBRKS, SJBRKS,
     + STITLE, SWORV2, OK)
           IF (.NOT.OK) GOTO 30
           IF ((HOLD.NE.NPTS).OR.
     +         (ABS((SWORV-SWORV2)/SWORV2).GE.1.0E-05)) THEN
             WRITE (*,
     :       '(''   FTINV:  real & imaginary parts mismatched'')')
             OK = .FALSE.
             GO TO 30
           ENDIF
           DO 25, I = 1,NPTS
             FREQ(I-1) = DBLE(RFREQ(I))
             RE(I-1) = DBLE(RRE(I))
             IM(I-1) = DBLE(RIM(I))
   25      CONTINUE
           DWORV = DBLE(SWORV)
           CALL REVERX (FREQ, RE, IM, NPTS, DWORV, DX, DY)
           DO 27, I=1,NPTS
             SJX(I) = SNGL(DX(I))
             SJY(I) = SNGL(DY(I))
   27      CONTINUE
           SWORV = 1.0
           STITLE = 'Restored data (Fourier inversion) '
           ASIZE = ARMAX
           BSIZE = 1
           SNBRKS = 1
           SJBRKS(1) = NPTS
           CALL UPUSH (ASIZE, SJX, SJY, NPTS, BSIZE, SJBRKS, SNBRKS,
     + STITLE, SWORV, OK)
         ELSE
   30      CONTINUE
         ENDIF
C
C  Command PERIGRAM:- obtain the Periodogram (Scargle, 1982, ApJ 263:835)
C of the data in the current arrays and place the Periodogram in the current
C arrays
C
      ELSEIF (CMD.EQ.'PDGRAM') THEN
         IF (NPT .GT. ARMAX) THEN
           WRITE(*,*) '    PDGRAM: too many datum points '
           OK = .FALSE.
           GOTO 31
         ENDIF
         VALUES(4) = 0.05
         TSTVAL = 654321.789
         VALUES (1) = 0.0
         VALUES (2) = TSTVAL
         VALUES (3) = TSTVAL
         CALL DECODE (CMD, PARAMS, 0, 4, VALUES,
     :    ' ',OK)
         IF (.NOT.OK) GOTO 31
         PMASK = DBLE( VALUES(4) )
         CHISQ = DBLE( VALUES(1) )
         IF (VALUES(2).EQ.TSTVAL) THEN
            SCAT = DBLE(NPT-1)*0.5D0/(X(NPT)-X(1))
         ELSE
            SCAT = DBLE(VALUES(2))
         ENDIF
         IF (VALUES(3).EQ.TSTVAL) THEN
            DWORV = 1.D0 / (X(NPT)-X(1))
         ELSE
            DWORV = VALUES(3)
         ENDIF
         IF (VALUES(2).LE.0.0 .OR. VALUES(3).LE.0.0 .OR.
     :   VALUES(1).LT.0.0) THEN
            WRITE (*,
     :      '(''   PDGRAM:  negative frequencies are nonphysical'')')
            OK = .FALSE.
            GOTO 31
         ELSEIF (VALUES(1).GE.VALUES(2)) THEN
            WRITE (*,
     :      '(''   PDGRAM:  low frequency exceeds high frequency!'')')
            OK = .FALSE.
            GOTO 31
         ENDIF
         DO I = 1,NPT
           DX(I) = DBLE( X(I) )
           DY(I) = DBLE( Y(I) )
         ENDDO
         CALL PERIGRAM (DX, DY, NPT, PMASK, CHISQ, SCAT, DWORV,
     +                        X, Y, NPTS, OK)
         IF (OK) THEN
           NPT = NPTS
           NBRK = 1
           BRKS(1) = NPT
           CALL SSTRIP (TITLE)
           TITLE = 'Scargle Periodogram of '//TITLE
         ENDIF
   31    CONTINUE

C
C  Command WINDOW:- Obtain the window function (Scargle, 1982, ApJ 263:835)
C of the current X-values and place the window in the current arrays
C
      ELSEIF (CMD.EQ.'PDGWINDOW') THEN
         IF (NPT .GT. ARMAX) THEN
           WRITE(*,*) '    PDGWINDOW:  too many datum points '
           OK = .FALSE.
           GOTO 31
         ENDIF
         VALUES(1) = 0.0
         TSTVAL = 654321.789
         VALUES(2) = TSTVAL
         VALUES(3) = TSTVAL
         CALL DECODE (CMD, PARAMS, 0, 3, VALUES, ' ', OK)
         IF (.NOT.OK) GOTO 31
         PMASK = DBLE( VALUES(1) )
         IF (VALUES(2).EQ.TSTVAL) THEN
            CHISQ = 0.5D0*DBLE(NPT-1)/(X(NPT)-X(1))
         ELSE
            CHISQ = DBLE(VALUES(2))
         ENDIF
         IF (VALUES(3).EQ.TSTVAL) THEN
            SCAT = 1.D0 / (X(NPT)-X(1))
         ELSE
            SCAT = DBLE(VALUES(3))
         ENDIF
         DO I = 1,NPT
           DX(I) = DBLE( X(I) )
         ENDDO
         CALL WINDOW (DX, NPT, PMASK, CHISQ, SCAT, X, Y, NPTS, OK)
         IF (OK) THEN
           NPT = NPTS
           NBRK = 1
           BRKS(1) = NPT
           CALL SSTRIP(TITLE)
           TITLE = 'Scargle Window Function of '//TITLE
         ENDIF
C
C  Command XCORR - correlation
C
      ELSEIF (CMD.EQ.'XCORR') THEN
         TSTVAL = 654321.789
         VALUES(3) = TSTVAL
         VALUES(4) = TSTVAL
         VALUES(5) = TSTVAL
         CALL DECODE (CMD, PARAMS, 2, 5, VALUES,
     :   'Entry_1 Entry_2 ',OK)
         IF (OK) THEN
           INDEX = NINT( VALUES(1) )
           INDEXB = NINT( VALUES(2) )
           IF (VALUES(5).EQ.TSTVAL) THEN
              PMASK = 0.05
           ELSE
              PMASK = DBLE(VALUES(5))
           ENDIF
           NPTS = ARMAX
           SNBRKS = 200
           CALL GETSTK (INDEX, NPTS, SJX, SJY, SNBRKS, SJBRKS, STITLE,
     +                                  SWORV, OK)
           IF ( .NOT.OK ) GOTO 49
           RLIDH = (SJX(NPTS)-SJX(1))
           RLIDH = MIN (RLIDH, RLIDH*100./REAL(NPTS))
           IF (VALUES(3).EQ.TSTVAL) THEN
              CHISQ = 0.D0 - RLIDH
           ELSE
              CHISQ = DBLE(VALUES(3))
           ENDIF
           IF (VALUES(4).EQ.TSTVAL) THEN
              SCAT  = DBLE(RLIDH)
           ELSE
              SCAT = DBLE(VALUES(4))
           ENDIF
           DO 42, I=1,NPTS
             DX(I) = DBLE(SJX(I))
             DY(I) = DBLE(SJY(I))
   42      CONTINUE
           HOLD = ARMAX
           SNBRKS = 200
           CALL GETSTK (INDEXB, HOLD, REIX, REIY, SNBRKS, SJBRKS,
     +                     STITLE, SWORV, OK)
           IF (.NOT.OK) GOTO 49
           DO 43, I=1,HOLD
             EIX(I) = DBLE(REIX(I))
             EIY(I) = DBLE(REIY(I))
   43      CONTINUE
           CALL XCORRE (PMASK, CHISQ, SCAT, DX, DY, NPTS,
     +                  EIX, EIY, HOLD, VEL, XCOR, MODE, OK)
           IF ( .NOT.OK ) GOTO 49
           DO 44, I=1,NPTS
             SJX(I) = SNGL(DX(I))
             SJY(I) = SNGL(DY(I))
   44      CONTINUE
           WRITE(STITLE,'(A26,I4)') 'XCORR: interpolated entry ',INDEX
           ASIZE = ARMAX
           BSIZE = 1
           SNBRKS = 1
           SJBRKS(1) = NPTS
           CALL UPUSH (ASIZE, SJX, SJY, NPTS, BSIZE, SJBRKS, SNBRKS,
     +                    STITLE, SWORV, OK)
           DO 45, I=1,HOLD
             REIX(I) = SNGL(EIX(I))
             REIY(I) = SNGL(EIY(I))
   45      CONTINUE
           WRITE(STITLE,'(A26,I4)') 'XCORR: interpolated entry ',INDEXB
           ASIZE = ARMAX
           BSIZE = 1
           SNBRKS = 1
           SJBRKS(1) = HOLD
           CALL UPUSH (ASIZE, REIX, REIY, HOLD, BSIZE, SJBRKS, SNBRKS,
     +                    STITLE, SWORV, OK)
           WRITE(STITLE,'(A7,I4,A12,I4)') 'Entry ',INDEX,' xcor Entry '
     +                                       ,INDEXB
           ASIZE = ARMAX
           BSIZE = 1
           SNBRKS = 1
           SJBRKS(1) = MODE
           CALL UPUSH (ASIZE, VEL, XCOR, MODE, BSIZE, SJBRKS, SNBRKS,
     +                    STITLE, SWORV, OK)
         ELSE
   49      CONTINUE
         ENDIF
C
C  Command XCORVEL: convert cross-correlation X-axis from lags (measured in
C wavelength units) to velocity shifts (measured in kilometers per second).
C
      ELSEIF (CMD.EQ.'INTERP') THEN
         CALL DECODE (CMD, PARAMS, 1, 1, VALUES,
     :  'Order ',OK)
         IF (OK) THEN
           COEFS(1) = X(1)
           COEFS(2) = X(NPT)
           COEFS(3) = (X(NPT)-X(1))/(NPT-1.)
           ORDER = NINT(VALUES(1))
           IF (NPT.LT.2) GOTO 59
           NPTS = NPT
           DO I=1,NPTS
             DX(I) = DBLE(X(I))
             DY(I) = DBLE(Y(I))
           ENDDO
           CALL LAGUERRE (DX, DY, NPTS, COEFS(1), COEFS(2), COEFS(3),
     +                       ORDER, OK)
           IF (.NOT.OK) GOTO 59
           NPT = NPTS
           DO I=1,NPT
             X(I) = SNGL(DX(I))
             Y(I) = SNGL(DY(I))
           ENDDO
           STITLE = TITLE(:30)
        WRITE(TITLE,'(A15,I4,1X,A30)')'Lag. Intp. Order = ',ORDER,STITLE
           NBRK = 1
           BRKS(1) = NPT
         ELSE
   59      CONTINUE
         ENDIF
      ELSEIF (CMD.EQ.'PDGPEAK') THEN
         IF (NPT.LT.3) THEN
           WRITE(*,*) '    PDGPEAK:  too few data points to define peak'
           OK = .FALSE.
         ELSE
           CALL VPEAK (NPT, X, Y)
         ENDIF
C  Command not found
C
      ELSE
         OK = .FALSE.
      ENDIF

      END
