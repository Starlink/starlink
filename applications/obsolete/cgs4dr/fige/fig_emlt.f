C+
      SUBROUTINE FIG_EMLT (X,Y,NX,N1,N2,FWHM0,NOCENS,LINES,
     :                             STR,WID,BIN,CALIB,OUTSET,UNITS,LOGLU)
C
C     F I G _ E M L T
C
C     Analyses emission lines in a spectrum, producing a list of
C     their centers and widths, optionally from a center of moments
C     analysis, usually from a gaussian fit.  This is essentially
C     the bulk of the code from the SDRSYS routine EMLT, hacked
C     about dreadfully to turn it into the main routine of a Figaro
C     application.  The body of the code is unchanged, but all the
C     output formatting has been recoded (probably to its detriment)
C     and the routine now no longer assumes that the input specturm is
C     2048 elements.  It has also been modified to use a wavelengths
C     array rather than the start wavelength and dispersion used by
C     SDRSYS.  Note also that Figaro numbers channels from 1..NX,
C     while SDRSYS numbered channels from 0..NX-1, so the channel
C     numbers (but not the axis values) produced by this routine will
C     all be one greater than those produced by the original SDRSYS EMLT.
C
C     Parameters -   (">" input, "<" output, "!" modified, "W" workspace)
C
C     (>) X       (Real array) The axis values for the data.
C     (!) Y       (Real array) The data values.  This is modified by
C                 the program as a normal part of its processing,
C                 and should be regarded as destroyed.  If OUTSET is
C                 specified as true, Y is returned containing a synthetic
C                 spectrum generated from the fitted lines.
C     (>) NX      (Integer) The number of elements in the data.
C     (>) N1      (Integer) The number of the first element to be used
C                 in the analysis.
C     (>) N2      (Integer) The number of the last element to be used
C                 in the analysis.
C     (>) FWHM0   (Real) If non-zero, all lines fitted are constrained
C                 to this half-width (in pixels).
C     (>) NOCENS  (Integer) If set to 1, the center of moments analysis
C                 is bypassed.
C     (>) LINES   (Integer) If non-zero, only the LINES strongest lines
C                 in the spectrum will be analysed.
C     (W) STR     (Real array) Used to accumulate the line strengths.
C     (W) WID     (Real array) Used to accumulate the line widths.
C     (W) BIN     (Real array) Used to accumulate the line centers.
C                 STR,WID and BIN should all be of at least NX/5 +1 elements
C     (>) CALIB   (Logical) Indicates if the X values are to be used.
C     (>) OUTSET  (Logical) If true, a synthetic spectum based on the
C                 line analysis will be generated in Y.
C     (>) UNITS   (Character) The axis units.  Ideally, UNITS is ten
C                 characters long and is centered.  It is only used to
C                 generate the headings in the formatted output.
C     (>) LOGLU   (Integer) If non-zero, a logical unit number on which
C                 the reported lines are logged.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     FIG_GFIT    Fits a gaussian to an emission line.
C     FIG_XVALUE  Interpolates between values in the axis array.
C
C     History -
C
C     Sometime       Original SDRSYS version, John Straede.
C     26th Feb 1988  Original re-write for Figaro.  KS / AAO.
C     16th Aug 1988  Some formatting problems corrected.  KS/AAO.
C     25th Jan 1989  Bug in dispersion calculation when only a subset of
C                    the data used now corrected.  KS/AAO.
C     30th May 1990  Modified for use by CGS4. The routine was rejecting
C                    perfectly valid Infra-Red emission lines because its
C                    search window was too small and the lines, being
C                    oversampled, covered more bins that expected. Window
C                    size increased from 5 to 9 bins. A variable WINDOW
C                    is used so this may be altered more easily.  SMB/ROE.
C+
      IMPLICIT NONE
C
C   PARAMETERS
      LOGICAL OUTSET,CALIB
      INTEGER NX,N1,N2,NOCENS,LINES,LOGLU
      REAL X(NX),Y(NX),STR(NX/5+1),WID(NX/5+1),BIN(NX/5+1),FWHM0
      CHARACTER*(*) UNITS
C
C   FUNCTIONS
      REAL FIG_XVALUE
C
C   VARIABLES
      INTEGER MINSTR,N10,N11,STATUS,IGNORE,LINE,NHC,NL,NH,NLCP1X
      INTEGER N1002,N1004,NHCP1,NLCP1,N1004X,NHCP1X,N101,N102,NC
      INTEGER NLC,N142,N144L,N144,NDPA,N160,N180,N210,N230,NXL,NXH,N220
      INTEGER WINDOW,NMIN,NMAX
      REAL ARND,FA,STRMIN,DISPER,STRENS,FWHMS,YCL,YCH,SIGM,SIGMX
      REAL XCMOM,SIGMH,SIGML,XCENTR,S2NF,XC,FWHM,STREN,C0,PEAK,ALPHA
      REAL PEAKO2,XHW,XL,XH,XV,RL,RH,FWHMA,STRMAX
      CHARACTER LOG*64,FMTSTR*35
C
C   MESSAGES
      CHARACTER M1*33,M2*33,M1A*5,MH1A*58,MH1B*58,MH2A*43,MH2B*43
      DATA MH1A
     :   /'    Bin                    Width      Integrated    Peak '/
      DATA MH1B
     :   /'  Number  Angstroms    Bins Angstroms  Strength    Height'/
      DATA MH2A/'Centre of      Centroid          Integrated'/
      DATA MH2B/'  moment                          Strength '/
      DATA M1  /'Mean fullwidth at half maximum = '/
      DATA M2  /'                               = '/
      DATA M1A /' bins'/
C
C   ERROR MESSAGES
      CHARACTER EM2*16
      DATA EM2 /'No Gaussian Fits'/
C
C   DEFINE SIZE OF SEARCH WINDOW IN BINS
C
      WINDOW = 9
C
C   REMOVE SINGLE BIN FEATURES
C
      NMIN = N1 + 2
      NMAX = N2 - WINDOW + 1    ! This was -4 (i.e. -5+1)
C
      DO 10 N10 = 2,NX-1
C
         IF (Y(N10-1) .NE. 0.0)   GO TO 10
         IF (Y(N10+1) .NE. 0.0)   GO TO 10
C
            Y(N10) = 0.0
C
  10     CONTINUE
C
C   REMOVE DOUBLE BIN FEATURES
C
         DO 11 N11 = 2,NX-2
C
         IF (Y(N11-1) .NE. 0.0)   GO TO 11
         IF (Y(N11+2) .NE. 0.0)   GO TO 11
C
            Y(N11)   = 0.0
            Y(N11+1) = 0.0
C
  11     CONTINUE
C
C   FIND LINES
C
      IF (NOCENS .EQ. 1)   GO TO 99
      CALL PAR_WRUSER (' ',STATUS)
      CALL PAR_WRUSER (MH2A,STATUS)
      CALL PAR_WRUSER (MH2B,STATUS)
      CALL PAR_WRUSER (' ',STATUS)
      IF (LOGLU.NE.0) WRITE (LOGLU,'(/A)',IOSTAT=IGNORE) MH2A
      IF (LOGLU.NE.0) WRITE (LOGLU,'(A/)',IOSTAT=IGNORE) MH2B
  99  CONTINUE
      LINE = 0
      NHC = NMIN
      STRENS = 0.0
      FWHMS = 0.0
C
 100  CONTINUE
      NL = NHC
 1001 CONTINUE
      IF (NL .GT. NMAX)   GO TO 109
C                                   FINISHED
      NL = NL +1
      IF (Y(NL+1) .EQ. 0.0)   GO TO 1001
C                                        NEXT DATA POINT NULL - ADVANCE
      NH = NL + WINDOW    ! This was +5
      FWHM = FWHM0
      CALL FIG_GFIT (XC,FWHM,STREN,NLC,NHC,C0,S2NF,Y,NX,NL,NH)

      IF (STREN .LE. 0.0)   GO TO 100
      IF (NOCENS .EQ. 1)   GO TO 104
C
C   FIND LIMITS OF LINE
      NLCP1 = NLC +1
      NHCP1 = NHC +1
      YCL = Y(NLCP1)
      YCH = Y(NHCP1)
C
      DO 1002 N1002 = NLCP1,NHCP1
C
         NLCP1X = N1002
         IF (Y(N1002+1) .GT. YCL)   GO TO 1003
 1002    CONTINUE
C
 1003 CONTINUE
C
      DO 1004 N1004 = NLCP1,NHCP1
C
         N1004X = NLCP1 +NHCP1 -N1004
         NHCP1X = N1004X
         IF (Y(N1004X-1) .GT. YCH)   GO TO 1005
 1004    CONTINUE
C
 1005 CONTINUE
      NLCP1 = NLCP1X
      NHCP1 = NHCP1X
      IF (NLCP1 .GE. NHCP1)   GO TO 104
C
C   CENTRE OF MOMENT
      SIGM  = 0.0
      SIGMX = 0.0
C
      DO 101 N101 = NLCP1,NHCP1
C
         SIGM = SIGM +Y(N101)
         SIGMX = SIGMX + FLOAT(N101 -NLCP1)*Y(N101)
  101    CONTINUE
C
      XCMOM = FLOAT(NLCP1) + SIGMX /SIGM
C
C   CENTROID
      SIGMH = SIGM /2.
C
      DO 102 N102 = NLCP1,NHCP1
C
         SIGML = SIGMH
         SIGMH = SIGML -Y(N102)
         NC = N102 -2
         IF (SIGMH .LT. 0.0)   GO TO 103
  102    CONTINUE
C
  103 CONTINUE
      XCENTR = FLOAT(NC) + SIGML /Y(NC+2) +1.5
C
C   STRENGTH
      WRITE (LOG,'(F8.1,7X,F8.1,7X,G12.4)',IOSTAT=IGNORE)
     :                                           XCMOM,XCENTR,STREN
      CALL PAR_WRUSER (LOG,STATUS)
      IF (LOGLU.NE.0) WRITE (LOGLU,'(A)',IOSTAT=IGNORE) LOG
  104 CONTINUE
      IF (S2NF .LE. 0.0)   GO TO 100
      LINE = LINE +1
      STR(LINE) = STREN
      WID(LINE) = FWHM
      BIN(LINE) = XC
      IF (FWHM0 .GT. 0.0)   GO TO 100
      IF (FWHM .LE. 0.0)   GO TO 100
C
C   ACCUMULATE FULLWIDTH-HALFMAXIMUM WEIGHTED BY PEAK HEIGHT
      FWHMS = FWHMS + STREN
      STRENS = STRENS + STREN /FWHM
      GO TO 100
C
 109  CONTINUE
C
      IF (LINE .GT. 0)   GO TO 141
C                                  GAUSSIAN FITTED LINES FOUND
C     NO GAUSSIAN FITTED LINES
         CALL PAR_WRUSER (EM2,STATUS)
         IF (LOGLU.NE.0) WRITE (LOGLU,'(A)',IOSTAT=IGNORE) EM2
         GO TO 300
C
C   ?? DO WE HAVE MORE THAN WE WANT ??
  141 CONTINUE
      IF (LINES .LE. 0)   GO TO 150
C                                   WANT ALL THERE IS
      IF (LINE .LE. LINES)   GO TO 150
C                                      WANTED ALL WE GOT
         IF (LINE .LE. 1)   GO TO 150
C                                     ONLY ONE LINE - NOTHING TO REJECT
C     FIND WEAKEST LINE
         STRMIN = STR(1)
         MINSTR = 1
C
         DO 142 N142 = 2,LINE
C
C
            IF (STR(N142) .GT. STRMIN)   GO TO 142
C                                                  STRONGER
C           NEW WEAKEST LINE
               STRMIN = STR(N142)
               MINSTR = N142
C
  142       CONTINUE
C
C     REJECT WEAKEST LINE
         N144L = MINSTR + 1
         IF (N144L .GT. LINE)   GO TO 145
C                                         WEAKEST LINE IS LAST
            DO 144 N144 = N144L,LINE
C
               STR(N144 - 1) = STR(N144)
               WID(N144 - 1) = WID(N144)
               BIN(N144 - 1) = BIN(N144)
  144          CONTINUE
C
C     DECREMENT LINE COUNT
 145     CONTINUE
         LINE = LINE - 1
         GO TO 141
C                  SEE IF THERE ARE STILL TOO MANY LINES
C
 150  CONTINUE
C
C   LOG RESULTS
C
C
      DISPER = ABS(X(NX)-X(1))/FLOAT(NX-1)
      ARND = 0.05
      NDPA = 1
      IF (DISPER .GE. 2.0)   GO TO 151
      ARND = 0.005
      NDPA = 2
      IF (DISPER .GE. 0.2)   GO TO 151
      ARND = 0.0005
      NDPA = 3
      IF (DISPER .GE. 0.02)   GO TO 151
      ARND = 0.00005
      NDPA = 4
C
  151 CONTINUE
      IF (CALIB) THEN
         MH1B(11:20) = UNITS
         MH1B(29:38) = UNITS
      ELSE
         MH1B(11:20) = ' X-units  '
         MH1B(29:38) = ' X-units  '
      END IF
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER(MH1A,STATUS)
      CALL PAR_WRUSER(MH1B,STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      IF (LOGLU.NE.0) WRITE (LOGLU,'(/A)',IOSTAT=IGNORE) MH1A
      IF (LOGLU.NE.0) WRITE (LOGLU,'(A/)',IOSTAT=IGNORE) MH1B
C
C  OUTPUT FORMAT
      STRMAX = STR(1)
      DO 160 N160 = 1,LINE
         STREN = STR(N160)
         IF (CALIB) THEN
            FA = FIG_XVALUE (BIN(N160)+1.0,X,NX)
            FWHM = ABS (FIG_XVALUE (BIN(N160)+1.0+WID(N160)*.5,X,NX) -
     :                    FIG_XVALUE (BIN(N160)+1.0-WID(N160)*.5,X,NX))
            STREN = STREN * FWHM / WID(N160)
         END IF
         IF (STREN.GT.STRMAX) STRMAX=STREN
  160    CONTINUE
      FMTSTR = '(F8.2,F12.N,F7.2,F10.N,F10.3,F10.3)'
      FMTSTR(11:11) = CHAR (ICHAR('0') + NDPA)
      FMTSTR(22:22) = FMTSTR(11:11)
      IF ((STRMAX.GT.32000.0).OR.(STRMAX.LT.0.1))
     :                          FMTSTR(24:34) = '2(1PE10.3)'
C
      DO 180 N180 = 1,LINE
C
C     BIN NUMBER, WIDTH (BINS)
C     POSITION & WIDTH IN AXIS UNITS
         IF (.NOT.CALIB)   GO TO 163

C        POSITION (AXIS UNITS)
            FA = FIG_XVALUE (BIN(N180)+1.0,X,NX)
C
C        WIDTH (ANGSTROM)
            FWHM = ABS (FIG_XVALUE (BIN(N180)+1.0+WID(N180)*.5,X,NX) -
     :                    FIG_XVALUE (BIN(N180)+1.0-WID(N180)*.5,X,NX))
C
  163    CONTINUE
C
C     INTEGRATED STRENGTH
         STREN = STR(N180)
         IF (CALIB)   STREN = STREN * FWHM / WID(N180)
C
C     PEAK HEIGHT
  165    CONTINUE
         PEAK = 0.939437 *STR(N180) /WID(N180)
C
  169    CONTINUE
         WRITE (LOG,FMTSTR,IOSTAT=IGNORE) BIN(N180)+1.0,FA,WID(N180),
     :                                                FWHM,STREN,PEAK
         IF (.NOT.CALIB) THEN
            LOG(9:20)='      --'
            LOG(28:37)='      --'
         END IF
         CALL PAR_WRUSER (LOG,STATUS)
         IF (LOGLU.NE.0) WRITE (LOGLU,'(A)',IOSTAT=IGNORE) LOG
  180    CONTINUE
C
C   CONSTRUCT 'MARK' SCAN
C
      IF (.NOT.OUTSET)   GO TO 250
C
C   ZAP ORIGINAL DATA
C
      DO 210 N210 = 1,NX
C
         Y(N210) = 0.0
  210    CONTINUE
C
      DO 230 N230 = 1,LINE
C
         FWHM = WID(N230)
         ALPHA = 1.665109 /FWHM
         XC = BIN(N230)
         PEAKO2 = 0.469719 *STR(N230) /FWHM
         XHW = 4.163 *FWHM
         XL = XC -XHW
         NXL = IFIX(XL +0.5)
         NXL = MAX0(1,NXL)
         XH = XC +XHW
         NXH = IFIX(XH +1.5)
         NXH = MIN0(NXH,NX)
C
         DO 220 N220 = NXL,NXH
C
            XV = FLOAT(N220-1) -XC
            RL = (XV -0.25) *ALPHA
            RH = (XV +0.25) *ALPHA
            Y(N220) = Y(N220) + PEAKO2 *(EXP(-RL*RL) +EXP(-RH*RH))
  220       CONTINUE
C
  230    CONTINUE
  250 CONTINUE
      IF (STRENS .LE. 0.0)   GO TO 300
C
         FWHMA = FWHMS / STRENS
         WRITE (LOG,'(A,G10.4,A)',IOSTAT=IGNORE) M1,FWHMA,M1A
         CALL PAR_WRUSER(' ',STATUS)
         CALL PAR_WRUSER(LOG,STATUS)
         IF (LOGLU.NE.0) WRITE (LOGLU,'(/A)',IOSTAT=IGNORE) LOG
C
         IF (.NOT.CALIB)   GO TO 260
C
         FWHMA = FWHMA *DISPER
         WRITE (LOG,'(A,G10.4,A)',IOSTAT=IGNORE) M2,FWHMA,UNITS
         CALL PAR_WRUSER(LOG,STATUS)
         IF (LOGLU.NE.0) WRITE (LOGLU,'(A)',IOSTAT=IGNORE) LOG
C
  260    CONTINUE
C
  300 CONTINUE
      END

