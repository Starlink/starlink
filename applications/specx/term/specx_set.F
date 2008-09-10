*  History:
*     19 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*        Remove TABs.
*     17 Dec 1993 (hme):
*        Re-order IODATA common block to avoid alignment problems.
*     31 Dec 1993 (rp):
*        Insert changes from V6.3
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*     20 July 2000 (ajc):
*        Missing commas in FORMAT
*        Change TYPE * to PRINT *
*        Remove CARRIAGECONTROL from OPEN statement
*        Don't check specific "printer" filename.
*        Replace DISP='PRINT/DELETE' by STATUS='KEEP' in CLOSE statement
*         (was kept anyway)
*        Don't split strings across lines
*        Unused TEST, MFIL, NFILE, IFILE1, IFILE2, TEMPT, SUMST, APROMPT
*        Make TIDY .FALSE. in SET_LIST_FILE for File output (prevents
*         attempt to return LUN after error with file still open).
C-----------------------------------------------------------------------
#if HAVE_CONFIG_H
# include "config.h"
#endif
      SUBROUTINE SPECX_SET

C  ROUTINE FOR INPUT OF USER DEFINED PARAMETERS.
C  All queries are made to an ENTRY  of the form
C  Qnnaaa where Q stands for QUERY, nn is a two digit sequence and
C  aaa is an acronym for the calling command, e.g. SPS stands for
C  SET-PLOT-SCALES.

      INCLUDE   'FLAGCOMM'
      INCLUDE   'MAPHD'
      INCLUDE   'NEWXY'
      INCLUDE   'PLOT2D'
      INCLUDE   'SPECX_PARS'
      INCLUDE   'STAKPAR'
      INCLUDE   'CUBE'

      LOGICAL   DOQUAD
      LOGICAL*4 PRINT_OUTPUT
      LOGICAL*4 IEXIST, IOPENED
      INTEGER*4 ITEMP(3)
      REAL*4    AXLEN(2), CONT(2),
     &          YM(2),    PLEN(2),
     &          XLIMS(2),  YLIMS(2),
     &          TEMP(3)
      CHARACTER PROGNAME*40,
     &          MAPTIT(3)*11, AXTIT(3)*6,
     &          IREP*1,      STRING*80,   PROMPT*256,
     &          FORMAT*16
      CHARACTER LISTDEV*1, PRINT_FILE*40
      CHARACTER GSDNAME_FORMAT*3
      CHARACTER BOUNDS(2)*10
      DATA      BOUNDS      /'left,right', 'top,bottom'/

      COMMON /IODATA/ ILOUT2, PRINT_OUTPUT, PROGNAME, LISTDEV
      COMMON /NEWXY/  INEWXY1(6), ICHANGE
      COMMON /TITLES/ MAPTIT, AXTIT
      COMMON /STRING/ STRING
      COMMON /NOKEEP/ JPLOT

      EQUIVALENCE (AXLEN(1),AX1LEN)
      EQUIVALENCE (CONT(1),CONT0)
      EQUIVALENCE (YM(1),YMIN)
      EQUIVALENCE (PLEN(1),XLEN)
      EQUIVALENCE (XLIMS(1),XST)
      EQUIVALENCE (YLIMS(1),YST)

*     Functions

      INTEGER  GEN_ILEN

C  Group 1 options, contour mapping
C==================================

C----------------
C  SET_ZRANGE
C----------------

      ENTRY SET_ZRANGE

C  Read in range of binning in z-coordinate and whether average or 
C  integrated intensity

      PROMPT = ' '
      WRITE (PROMPT,1030) MAPTIT(LINK(3)), AXTIT(LINK(3)),
     &                   QBEG(LINK(3)), QEND(LINK(3))
 1030 FORMAT (A4,' range? (',A6,') [',F9.4,',',F9.4,'] ')
      TEMP(1) = QBEG(LINK(3))
      TEMP(2) = QEND(LINK(3))
      CALL GEN_GETR4A (PROMPT, TEMP, 2, ' ', TEMP, JDEF)
      IF (JDEF.EQ.0) THEN
        QBEG(LINK(3)) = TEMP(1)
        QEND(LINK(3)) = TEMP(2)
      END IF

      CALL GEN_YESNO ('Integrated intensity? (rather than average)',
     &                ISUM, ISUM, JDEF)

      RETURN

C----------------
C  SET-MAP-SIZE
C----------------

      ENTRY SET_MAP_SIZE (IERRSMS)
      IERRSMS = 0

      CALL GEN_GETR4A ('X & Y-axis lengths (mm)? (0 for auto)',
     &                  AXLEN, 2, '2(F5.0,1X)', AXLEN, JDEF)
      IF (JDEF.GE.0) CHANGE_PLOT = .FALSE.

      RETURN

C----------------
C  DEFINE-MAP
C----------------

      ENTRY Q02DM(IERR2)
      IERR2=0

C     Style of RA/Dec axis labels
      CALL GEN_YESNO ('Angular scales in D/M/S (otherwise arcsec)?',
     &                 DMS, DMS, ISTAT)

C     Find if valid points to be indicated
      CALL GEN_YESNO ('Mark the valid data points?',MVPTS,MVPTS,JDEF)

C     Smoothing
      CALL GEN_YESNO ('Apply smoothing to map?',SMOOTH,SMOOTH,JDEF)
      IF (SMOOTH) THEN
        CALL GEN_GETI4A('Number of extra points per pixel? (X,Y)',
     &                   INTERP_X,2,'I2,1X,I2',INTERP_X,JDEF)
      END IF
      IF (INTERP_X.GT.9) THEN
        INTERP_X = 9
        PRINT *,'Maximum of 9 interstitial points in X!'
      END IF
      IF (INTERP_Y.GT.9) THEN
        INTERP_Y = 9
        PRINT *,'Maximum of 9 interstitial points in Y!'
      END IF

C     Beam size

      CALL GEN_YESNO ('Indicate beam size on map?',
     &                 SHOW_BEAM, SHOW_BEAM, JDEF)
      IF (SHOW_BEAM) THEN
        CALL GEN_GETR4 ('Beam diameter? (FWHM, arcsec)',
     &                   BSIZE, 'F6.1',  BSIZE, JDEF)
      END IF

C     Line types
      ITEMP(1) = LTPOS
      ITEMP(2) = LTZ
      ITEMP(3) = LTNEG
      CALL GEN_GETI4A ('Line types for +ve, zero and -ve contours?'//
     &                 '(0,1,2 recommended)',
     &                  ITEMP, 3, 'I1,1X,I1,1X,I1', ITEMP, JDEF)
      LTPOS = ITEMP(1)
      LTZ   = ITEMP(2)
      LTNEG = ITEMP(3)

C     Plot header
      CALL GEN_YESNO ('Plot map header?',
     &                 SHOW_2DHEAD, SHOW_2DHEAD, JDEF)

      ERASE_PLOT = .TRUE.
      RETURN

C----------------
C  SET-MAP-ACCEPT
C----------------

      ENTRY SET_MAP_ACCEPT (IERRSMA)
      IERRSMA = 0

      CALL GEN_YESNO ('Replace existing map spectra?',
     &                 REPLACE_MAP_DATA, REPLACE_MAP_DATA, JDEF)
      CALL GEN_GETR4 ('Maximum distance (pixels) from nearest gridpt?',
     &                 MAP_TOL, 'F6.1', MAP_TOL, JDEF)

      RETURN

C--------------------
C  SET-CONTOUR-LEVELS
C--------------------

      ENTRY SET_CONTOURS (IERR17)
      IERR17 = 0

C  Auto-contouring required

      CALL GEN_YESNO ('Auto-contouring required?',ICAUTO,ICAUTO,JDEF)

C  Determine contour level, base level etc

      IF (ICAUTO) THEN
        CALL GEN_GETI4 ('Number of contours?',NCONT,'I2',NCONT,JDEF)

      ELSE
        CALL GEN_YESNO ('Set contours by hand?', CLEVELS_SET,
     &                   CLEVELS_SET, JDEF)

        IF (CLEVELS_SET) THEN
          IF (NCSET.EQ.0) THEN
            PROMPT = '"'' Enter up to 16 contour levels''"'

          ELSE
            PROMPT = ' '
            N1     = 1
            DO WHILE (N1.LE.NCSET)
              N2     = MIN (N1+8, NCSET)
              WRITE  (STRING, 1250, IOSTAT=IERR) (CLEVELS(I), I= N1,N2)
 1250         FORMAT (9(F7.2,1X))
              ILP    = GEN_ILEN (PROMPT)
              ILS    = GEN_ILEN (STRING)
              PROMPT = PROMPT(:ILP)//STRING(:ILS)
              IF (N2.EQ.NCSET) THEN
                PROMPT = PROMPT(:ILP+ILS) // ']''"'
              ELSE
                PROMPT = PROMPT(:ILP+ILS) // '''/2X'''
              END IF
              N1     = N1 + 9
            END DO
            PROMPT =   '"'' Enter up to 16 contour levels''/'' ['
     &               // PROMPT(:GEN_ILEN(PROMPT))
          END IF

          CALL GEN_GETR4A2 (PROMPT, CLEVELS, 16, ' ',
     &                      CLEVELS, N, JDEF)
          IF (JDEF.EQ.0 .AND. N.NE.0) NCSET = N
          WRITE (ILOUT, '(1X,I2,'' contours entered.'')') NCSET

        ELSE
          CALL GEN_GETI4 ('Number of contours?',NCONT,'I2',NCONT,JDEF)
          CALL GEN_GETR4A ('Lowest contour & contour interval (K)?',
     &                      CONT, 2, '2(F6.2,1X)', CONT, JDEF)
        END IF

C  Simple error checking

        IF(NCONT.GT.30)   THEN
          WRITE(6,1041)
        END IF
      END IF

      RETURN

C--------------------
C  SET-CONTOUR-LEVELS
C--------------------

      ENTRY SET_GREYSCALE (IERR4)
      IERR 4 = 0

*     Greyscale limits and whether automatic or not

      CALL ASK_GREY (AUTOGREY, GREYLIM)

*     Overlay contours?

      CALL GEN_YESNO ('Overlay contours?',
     &                 OVERCONT, OVERCONT, ISTAT)

*     Which colour table?

      CALL GEN_GETI4   ('Colour table? (greyscale=1)',
     &                  COLOUR_TABLE, 'I2', COLOUR_TABLE, ISTAT)
      CALL SETCOL5     (C5START, C5ROTAT, C5EXP)
      CALL COLOUR_PLOT (COLOUR_TABLE, IERR4)

      RETURN

C  Group 2 options, set scale for x-axis ( points, vel,freq,etc)
C===============================================================

C----------------
C   SET-X-SCALE
C----------------

      ENTRY ASK_X_SCALE (IERR3)
      IERR3=0

      PROMPT=' '
      WRITE(PROMPT,2000) NXS
 2000 FORMAT(' [',I1,'] ''"')
      CALL GEN_GETI4('"//'' Set units for X-scale:''/'//
     1'     '' Key:    1      Points scale''/'//
     2'     ''         2      Frequency scale''/'//
     3'     ''         3      Velocity scale''/'//
     4'     ''         4      User defined scale''//'//
     5'     ''    Current units are '//XAXIS_UNITS//'''///'//
     6'     ''    Key? '//PROMPT(:GEN_ILEN(PROMPT)),NXS,' ',NXS1,JDEF)

      IF (JDEF.LT.0 .OR. JDEF.GT.2) THEN
        IERR3 = 38
        RETURN
      ELSE IF (NXS1.LE.0.OR.NXS1.GE.5)   THEN
        IERR3 = 21
        RETURN
      ENDIF

      IF (NXS1.NE.NXS) THEN
        ISETSC        = .TRUE.
        CHANGE_SCALES = .FALSE.
      END IF

      IF (NXS1 .EQ. 1) THEN

C  PS: SET POINTS SCALE

        XAXIS_NAME  = 'Points'
        XAXIS_UNITS = 'Chans'

C  Both frequency and velocity

      ELSE IF (NXS1 .EQ. 2 .OR. NXS1 .EQ. 3) THEN

        CALL GEN_YESNO  ('Apply polynomial correction to'//
     &                   ' frequency scale?', FCORRECT, FCORRECT, JDEF)
        IF (JDEF.LT.0 .OR. JDEF.GT.2) THEN
          IERR3 = 38
          RETURN
        END IF


C  FS: SET FREQUENCY SCALE

        IF (NXS1 .EQ. 2) THEN

          IREP = 'A'
          IF (.NOT. ABS_FREQ) IREP = 'R'
          CALL GEN_GETSTR ('Absolute or relative frequencies? (A/R)',
     &                     IREP, 'A', IREP, JDEF)
  
          IF (JDEF.LT.0 .OR. JDEF.GT.2) THEN
            IERR3 = 38
            RETURN
          END IF

          CALL UUCASE (IREP)
          IF (IREP .EQ. 'A') THEN
            ABS_FREQ = .TRUE.
          ELSE IF (IREP .EQ. 'R') THEN
            ABS_FREQ = .FALSE.
          ELSE
            IERR3 = 21
            RETURN
          END IF
 
          IF (.NOT. ABS_FREQ) THEN
            IREP = 'F'
            IF (REST_REL) IREP = 'R'
            CALL GEN_GETSTR ('Relative to line rest freq. or to'//
     &                       ' other freq? (R/F)',
     &                        IREP, 'A', IREP, JDEF)

            IF (JDEF.LT.0 .OR. JDEF.GT.2) THEN
              IERR3 = 38
              RETURN
            END IF

            CALL UUCASE (IREP)
            IF (IREP .EQ. 'R') THEN
              REST_REL = .TRUE.
            ELSE IF (IREP .EQ. 'F') THEN
              REST_REL = .FALSE.
              CALL GEN_GETR8 ('Reference frequency (GHz)? ',
     &                         REF_FREQ, 'F11.6', REF_FREQ, JDEF)
            ELSE
              IERR3 = 21
              RETURN
            END IF

          END IF

          XAXIS_NAME = 'Frequency'
          IF (ABS_FREQ) THEN
            XAXIS_UNITS = 'GHz'
          ELSE
            XAXIS_UNITS = 'MHz'
          END IF

C  VS : VELOCITY SCALE

        ELSE IF (NXS1 .EQ. 3) THEN

          XAXIS_NAME  = 'Velocity'
          XAXIS_UNITS = 'km/s'

        END IF

C  XS : ANY OTHER SCALE - USER OPTION

      ELSE IF (NXS1 .EQ.4) THEN
        CALL GEN_GETR4  ('Factor?', XFAC(1), 'F6.2', XFAC, JDEF)
        CALL GEN_GETSTR ('Title?', XAXIS_NAME, 'A10', XAXIS_NAME, JDEF)
        CALL GEN_GETSTR ('Units',  XAXIS_UNITS, 'A6', XAXIS_UNITS, JDEF)
        DO NQ = 2,8
          XFAC(NQ) = XFAC(1)
        END DO

      END IF

      NXS       = NXS1
      MAPTIT(3) = XAXIS_NAME
      AXTIT(3)  = XAXIS_UNITS

      RETURN

C----------------
C   SET-LINE-REST-FREQUENCIES
C----------------

      ENTRY Q31SLRF(IERR31)
      IERR31=0

      DO NQ=1,NQMAX
       IF(DOQUAD(NQ))  THEN
        PROMPT=' '
        WRITE(PROMPT,2100) NQ,FCEN(NQ)
        CALL GEN_GETR8(PROMPT,FCEN(NQ),' ',FCEN(NQ),JDEF)
       END IF
      END DO
      RETURN

 2100 FORMAT('Receiver # ',I1,'  Line rest frequency? (GHz) [',
     &       F10.6,'] ')

C  Group 3 options,   set histogram parameters
C=============================================

*     Deleted - RP, 6/8/87

C  Group 4 options, set up state of program ( masking function, stack,etc)
C=========================================================================

C----------------
C  DEFINE-STACK
C----------------

      ENTRY Q06DS(IERR6)
      IERR6=0

      CALL DSTACK
      WRITE(6,4100) JSTK,LSTK-LHEAD
      RETURN

C----------------
C  SET-MASKING-FUNCTION (Now revised to SET-QUADRANT-DISPLAY)
C----------------

      ENTRY Q07SMF(IERR7)
      IERR7=0

      PROMPT=' '
      WRITE(PROMPT,4200) MASK
 4200 FORMAT('[',8(I1,1X),'] ''"')
      CALL GEN_GETI4A(
     &'"'' Define function to mask processing and '',/'//
     &''' display of individual quadrants:''/'//
     &''' MASK(n) = 1   Turns processing/display ON for quadrant n''/'//
     &'''         = 0   Turns quadrant OFF''///'//
     &'''$Type MASK '//PROMPT(:GEN_ILEN(PROMPT)),
     &   MASK,8,' ',MASK,JDEF)
      NMASK=0
      DO J=1,NQMAX
        IF(MASK(J).NE.0)   MASK(J)=1
        NMASK=NMASK+MASK(J)
      END DO
      RETURN


C  Group 5 options;  set plot parameters
C=======================================

C----------------
C  SET-PLOT-SIZE (sizes etc)
C----------------

      ENTRY SET_PLOT_SIZE (IERRSPS)
      IERRSPS = 0

      CALL GEN_GETR4A('X & Y-axis lengths (mm)?',PLEN,2,'2(F5.0,1X)',
     &            PLEN,JDEF)

      RETURN

C----------------
C  SET-PLOT-PARAMETERS  (was DEFINE-PLOT)
C----------------

      ENTRY Q09DP(IERR9)
      IERR9=0

      CALL GEN_YESNO ('Draw axes, axis-labels etc?',
     &                 .NOT.QUICK_PLOT, QUICK_PLOT, JDEF)
      QUICK_PLOT = .NOT.QUICK_PLOT

      IF (.NOT.QUICK_PLOT) THEN
        CALL GEN_YESNO ('Write out spectrum header info?',
     &                   SHOW_HEADER, SHOW_HEADER, JDEF)
        CALL GEN_GETR4 ('Character height (mm)?',
     &                   CHARHT, 'F4.1', CHARHT, JDEF)
      END IF

      ITICKX = 1
      ITICKY = 1

      RETURN

C----------------
C  SET-PLOT-SCALES
C----------------

      ENTRY Q10SPS(IERR10)
      IERR10 = 0

      CALL GEN_YESNO ('Do you want automatic scaling of X-axis?',
     &                 FREEX, FREEX, JDEF)
      IF (.NOT. FREEX) THEN
        CALL GEN_GETR4A('X-axis scale: Beginning and end?',XLIMS,2,
     &              '2(F7.2,1X)',XLIMS,JDEF)
      END IF

      CALL GEN_YESNO ('Do you want automatic scaling of Y-axis?',
     &                 FREEY, FREEY, JDEF)
      IF (.NOT. FREEY) THEN
        CALL GEN_GETR4A('Y-axis scale: Beginning and end?',YLIMS,2,
     &              '2(F7.2,1X)',YLIMS,JDEF)
      END IF

      ISETSC = FREEX .OR. FREEY
      CHANGE_SCALES = .FALSE.

      RETURN

C----------------
C  SET-INTERACTIVE
C----------------

      ENTRY SET_INTERACTIVE (IERR11A)
      CALL GEN_YESNO ('Interactive plotting?',
     &                 INTERACTIVE, INTERACTIVE, JDEF)
      RETURN


C  Group 7 options, set log and list files etc
C=============================================

C----------------
C  SET-GSD-FILENAME (determine GSD filename prefix)
C----------------

      ENTRY SET_GSD_FILENAME (IERR12A)
      IERR12A = 0

      IGSD = GEN_ILEN (GSDNAME)
      WRITE (GSDNAME_FORMAT, '(''A'',I2.2)') IGSD
      CALL GEN_GETSTR ('GSD filename prefix?', GSDNAME(:IGSD),
     &                 GSDNAME_FORMAT, GSDNAME, JDEF)
      RETURN

C----------------
C  SET-LIST-FILE (for list and print output)
C----------------

      ENTRY ASK_LIST_FILE (IERR13)
      IERR13 = 0

      IF (LISTDEV .EQ. 'P') THEN
        INQUIRE( ILOUT, EXIST=IEXIST, OPENED=IOPENED )
        IF( IEXIST .AND. IOPENED ) THEN
           CLOSE( ILOUT, STATUS='KEEP', IOSTAT=IERR )
        END IF
        ISTAT = IFREELUN (ILOUT)

      ELSE IF (LISTDEV.EQ. 'F') THEN
        LENP = GEN_ILEN (SAVE_FILE)
        INQUIRE (FILE=SAVE_FILE, EXIST=IEXIST, OPENED=IOPENED)
        IF (IEXIST .AND. IOPENED) THEN
          CLOSE (ILOUT, STATUS='KEEP', IOSTAT=IERR)
*          WRITE (6,'('' Your output is stored in file '',A)')
*     &           SAVE_FILE(1:LENP)
        END IF
        ISTAT = IFREELUN (ILOUT)

      ELSE IF (LISTDEV.EQ. 'N') THEN
        CLOSE (ILOUT, IOSTAT=IERR)
        ISTAT = IFREELUN (ILOUT)
      END IF

      CALL GEN_GETSTR ('List on terminal/printer/file/null (T/P/F/N)?',
     &                  LISTDEV, 'A', LISTDEV, JDEF)
      IF (JDEF.LT.0 .OR. JDEF.GT.2) THEN
        IERR13 = 21
        RETURN
      END IF

      CALL UUCASE (LISTDEV)
      IF (LISTDEV .EQ. 'T') THEN
        ILOUT = 6
        PRINT_OUTPUT = .FALSE.

      ELSE IF (LISTDEV .EQ. 'P') THEN
        ISTAT = IGETLUN (ILOUT, 'specx_set', .TRUE.)
        PRINT_OUTPUT = .TRUE.

      ELSE IF (LISTDEV .EQ. 'F') THEN
        WRITE (FORMAT,'(''A'',I2.2)') GEN_ILEN(SAVE_FILE)
        CALL GEN_GETSTR ('Name of file?',
     &                    SAVE_FILE, FORMAT, SAVE_FILE, JDEF)
        ISTAT = IGETLUN (ILOUT, 'specx_set', .FALSE.)
        OPEN (ILOUT, FILE=SAVE_FILE, STATUS='UNKNOWN',
#if HAVE_FC_OPEN_POSITIONAPPEND
     &        ACCESS='SEQUENTIAL',POSITION='APPEND',
#elif HAVE_FC_OPEN_ACCESSAPPEND
     &        ACCESS='APPEND',
#else
       Do not know how to append to a file
#endif
     &        IOSTAT=IERR)
        PRINT_OUTPUT = .FALSE.

      ELSE IF (LISTDEV .EQ. 'N') THEN
        ISTAT = IGETLUN (ILOUT, 'specx_set', .TRUE.)
        OPEN (ILOUT, FILE='/dev/null', STATUS='OLD', IOSTAT=IERR)
        IF (IERR.NE.0) THEN
          WRITE (6,*) 'Trouble assigning output to null file'
          WRITE (6,*) 'IO status: ', IERR
        ELSE
*         WRITE (6,*) 'Output assigned to /dev/null!'
        END IF
        PRINT_OUTPUT = .FALSE.

      ELSE
        IERR13 = 21
      END IF

      IF (IERR13.EQ.0) THEN
        ILOUT2 = ILOUT
      END IF

      RETURN

C----------------
C  SET-DUMP
C----------------

      ENTRY Q14SD (IERR14)
      IERR14 = 0

      CALL GEN_YESNO ('Do you want automatic dumping?',
     &                 DUMP_STATE, DUMP_STATE, JDEF)
      RETURN

C  Group 8 options,  observatory parameters and physical conditions
C==================================================================

C----------------
C  SET-SITE-PARAMETERS
C----------------

      ENTRY Q15SSP(IERR15)
      IERR15=0
      CALL GEN_GETSTR('Observatory title?',OBSTIT,'A20',OBSTIT,JDEF)
      CALL GEN_GETR8('Latitude (degrees)?',ALAT,'F8.4',ALAT,JDEF)
      CALL GEN_GETR8('Longitude (degrees)?',ALONG,'F9.4',ALONG,JDEF)
      CALL GEN_GETR8('Local time correction(hrs behind UT)?',TIMCOR,
     &           'F5.1',TIMCOR,JDEF)
      COSLAT=DCOS(ALAT*3.141592654D0/180.D0)
      RETURN

 1013 FORMAT(' Third co-ordinate is ',A4,/)
 1024 FORMAT(2F20.0)
 1041 FORMAT(' WARNING...Too many contours, Set to 30')
 1042 FORMAT(' WARNING...Negative contour interval, set to 2K')
 4100 FORMAT(' Stack defined '/T3,'Levels:',I3,/T3,'Length:',I4)

      END
