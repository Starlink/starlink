*
*  SUBROUTINE CALLED FROM PROGRAM ISPROG TO CHANGE OPTIONS
*
       SUBROUTINE OPTIONS(INFILE,MAXSTK,NONSTK,OK,STATUS)
       INCLUDE 'SAE_PAR'
       INTEGER STATUS
*
       COMMON /DMISOPT/ OPTCAL, OPTOK, BLEND, COLUMN, QUART, SCALE,
     :                  MODE, MODE1, THE, PUSH, VEL, V1, V2
       COMMON /IDHIRF/ NRF, MAXNRF, POSN, RF
       COMMON /IDHIRF1/ CTITLE
       CHARACTER*(40) CTITLE
       REAL POSN(500), RF(500)
       COMMON /MAD   / BINST, BINST1, BINST2
*
       LOGICAL OPTCAL, OPTOK, OK, BEEP, LTEMP, MTEMP
       COMMON /BEEP  / BEEP
       CHARACTER*1 BLEEP
       COMMON /BLEEP / BLEEP
       LOGICAL ODIR
       LOGICAL TFILE
*
       INTEGER BLEND, MODE, MODE1, PUSH, VEL
       INTEGER SLEN
*
       CHARACTER*15 VARNAM, COMMND, DEFDIR*8, LINE1*40
       CHARACTER*40 CFILE
       CHARACTER*(*) INFILE

       DATA IZERO, IONE, ITWO, IFIVE/0, 1, 2, 5/

*   Check inherited status.
       IF( STATUS .NE. SAI__OK ) RETURN

*   Load defaults

       ODIR = .FALSE.
       OPTION = -1
       TABLE = 0
       BASCOR = 0
       BINST1 = 0
       BINST2 = 0
       QUART = 0.25
       THE = 12
       LTEMP = .FALSE.
       MTEMP = .FALSE.
       IF (.NOT.OPTCAL) THEN
          VEL = 2
          MODE = 0
          MODE1 = 0
          BINST = 0
          BLEND = 0
          V1 = -100.
          V2 = +100.
          COLUMN = 0
          IF (INFILE.EQ.' ') THEN
             WRITE (*,'(''   ISOPT:  loading default options'')')
             MTEMP = .TRUE.
             GOTO 210
          ENDIF
       ENDIF

*    look for options file, if not found put into edit mode

  100  CONTINUE
       IF (INFILE.EQ.' ') THEN
          TFILE = .TRUE.
!          CLOSE (68)
!          OPEN (UNIT=68,FILE='TT:',STATUS='UNKNOWN')
          GOTO 200
       ENDIF
       TFILE = .FALSE.
       IERR = 0
       CLOSE (68)
       OPEN (UNIT=68,FILE=INFILE(1:SLEN(INFILE)),
     : STATUS='OLD',IOSTAT=IERR)
       IF (IERR.NE.0) THEN
          IF((INDEX(INFILE,']').EQ.0).AND.(INDEX(INFILE,':').EQ.0))THEN
             INFILE(1:) = 'OWNERDIR:'//INFILE(1:)
             CLOSE (68)
             OPEN (UNIT=68,FILE=INFILE(1:SLEN(INFILE)),
     :       STATUS='OLD',IOSTAT=IERR)
             IF (IERR.NE.0) INFILE(1:) = INFILE(10:)
          ENDIF
       ENDIF

*    reads default options

       IF (IERR.EQ.0) THEN
          GOTO 200
  150     CONTINUE
          IF (OK) THEN
             WRITE (*,'(''   ISOPT:  options loaded from '',A)')
     :       INFILE(1:SLEN(INFILE))
             OPTCAL = .TRUE.
!             CLOSE (68)
!             OPEN (UNIT=68,FILE='TT:',STATUS='UNKNOWN')
             TFILE = .TRUE.
             GOTO 210
          ELSE
             WRITE (*,
     :       '(''   ISOPT:  error loading options from '',A,A)')
     :       INFILE(1:SLEN(INFILE)), BLEEP
             CLOSE (68)
             CLOSE (69)
             GOTO 300
          ENDIF
       ENDIF
       WRITE (*,'(''   ISOPT:  error opening '',A,A)')
     : INFILE(1:SLEN(INFILE)), BLEEP

       TFILE = .TRUE.
!       CLOSE (68)
!       OPEN (UNIT=68,FILE='TT:',STATUS='UNKNOWN')

*    edit mode prompt

  200  CONTINUE
       IOS = 0
       IF ( TFILE ) THEN

         CALL RDSTR( ' ', '   ISOPT> ', ' ', COMMND, STATUS )
         IF( STATUS .NE. SAI__OK ) THEN
            OK = .FALSE.
            GO TO 300
         END IF

       ELSE
         READ (68,'(A)',IOSTAT=IOS,END=150) COMMND
       ENDIF
       I = INDEX(COMMND,'=')
       IF (I.NE.0) COMMND(I:I) = ' '
       I = INDEX(COMMND,':')
       IF (I.NE.0) COMMND(I:I) = ' '

*    interpret command, assign it to VARNAM

       IF (IOS.NE.0 .AND. IOS.NE.-1) THEN
          WRITE (*,'(''   ISOPT:  error reading option'',A,A)')
     :    COMMND(1:SLEN(COMMND)), BLEEP
          IF (.NOT.TFILE) THEN
             OK = .FALSE.
             GOTO 150
          ENDIF
          GOTO 200
       ELSE
          CALL SSTRIP(COMMND)
          CALL DTOUPP(COMMND)

*  Allow "comment cards"

          IF (((COMMND(1:1).EQ.'!') .OR. (COMMND(1:1).EQ.'*')) .AND.
     :        (.NOT.TFILE)) GOTO 200

          IF (COMMND.EQ.' ') GOTO 200
          CALL DPARSE(COMMND,VARNAM,' ',DELIM)

*    the edit loop, exit if command = QIS

          IF (VARNAM(1:3).EQ.'QIS') THEN
             OK = .TRUE.
             CLOSE (68)
             OPTCAL = .TRUE.
             IF (.NOT.TFILE) THEN
                WRITE (*,'(''   ISOPT:  options loaded from '',A)')
     :          INFILE(1:SLEN(INFILE))
                LTEMP = .TRUE.
                GOTO 210
             ENDIF
             GOTO 300
          ELSEIF (VARNAM(1:1).EQ.'Q') THEN
             WRITE (*,'(''   ISOPT:  quit using QIS'',A)') BLEEP
             OK = .TRUE.
             GOTO 200
          ELSE

*    list commands if command = H

             IF ( VARNAM(1:1).EQ.'H' ) THEN
                WRITE (*,
     :          '(''   ISOPT_H:''/
     :          ''     To modify options, the syntax is:'',
     :          ''  <Option> <operator> <value>''/
     :          ''     where the operator can be, optionally and'',
     :          '' equivalently, blank, "=", or ":"''/
     :          ''     Valid options are:''/
     :          ''      BINST - instrumental "b" value used for'',
     :          '' convolution when CONV=1''/
     :          ''      BLEND - if several lines from different'',
     :          '' transitions overlap, then you must''/
     :          ''       (1)  Calculate line profiles for the first'',
     :          '' transition (with SPACE=1 or 2);''/
     :          ''       (2)  Prepare for the next line'',
     :          '' (requiring, at least, use of ISATM);'')')
                WRITE (*, '(
     :          ''       (3)  Set BLEND=<n>, where <n> is the stack'',
     :          '' entry of the UNCONVOLVED,''/
     :          ''            VELOCITY SPACE profile saved from'',
     :          '' step (1);''/
     :          ''       (4)  Calculate the line profile for the'',
     :          '' current transition.''/
     :          ''            This is automatically blended with'',
     :          '' the previously calculated profile''/
     :          ''            (and BLEND reset to 0).   Steps (2)-(4)'',
     :          '' can be repeated as necessary.''/
     :          ''      CONV - controls convolution with instrumental'',
     :          '' response function''/
     :          ''        If CONV=2 you are prompted for a file'',
     :          '' containing a numerical tabulation'')')
                WRITE (*, '(
     :          ''        of the IRF;  this must be in VELOCITY'',
     :          '' space (units of km/s)''/
     :          ''      SPACE - profiles saved in velocity or '',
     :          ''wavelength space''/
     :          ''      V1, V2 - velocity range over which profiles '',
     :          ''are calculated''/
     :          ''        (ISCALC may extend these limits if '',
     :          ''necessary)''/
     :          ''     Other valid inputs are:''/
     :          ''      H   -  Help''/
     :          ''      L   -  List current options ''/
     :          ''      QIS -  Quit option editor'')')

*    list options if command = L

             ELSE IF ( VARNAM(1:1).EQ.'L' ) THEN
  210           CONTINUE
                WRITE (*,
     :          '(''   ISOPT_L:''/
     :          ''     BINST '',F10.3,
     :          '' : instrumental "b" value (km/s)''/
     :          ''     BLEND '',I10,  '' : 0   no blending''/
     :          21X,''   n   blend with stack entry "n"''/
     :          ''     CONV  '',I10,  '' : 0   no convolution''/
     :          21X,''   1   convolve with Gaussian'',
     :          '' [sigma = BINST/Sqrt(2)]''/
     :          21X,  ''   2   convolve with user-'',
     :          ''defined instrumental function'')') BINST,BLEND,MODE1
                WRITE (*, '(
     :          ''     SPACE '',I10,  '' : 0   save profile(s) as'',
     :          '' wavelengths''/
     :          21X,''   1   save profile(s) as'',
     :          '' velocities''/
     :          21X,''   2   save unconvolved profile as velocities,''/
     :          21X,''       convolved profiles as wavelengths''/
     :          ''     V1    '',F10.3,'' : start velocity for'',
     :          '' calculation''/
     :          ''     V2    '',F10.3,'' : end velocity for'',
     :          '' calculation'')')
     :          VEL,V1,V2
                IF (LTEMP) THEN
                   LTEMP = .FALSE.

*    assign flag OPTCAL to true

                   OPTCAL = .TRUE.
                   CLOSE (68)
                   GOTO 300
                ELSEIF (MTEMP) THEN
                   MTEMP = .FALSE.
                   GOTO 100
                ENDIF

*    for any other command

             ELSE

*    look for parameters on end of command line, assign them to VALUE

                CALL TOREAL(COMMND,VALUE,OK)
                IF (.NOT.OK) THEN
                   WRITE (*,
     :             '(''   ISOPT:  error decoding value'',A)') BLEEP
                   IF (.NOT.TFILE) GOTO 150

*    change value of option

                ELSEIF (VARNAM.EQ.'BINST') THEN
                   IF (VALUE.EQ.0.0) THEN
                      IF (MODE1.EQ.1) THEN
                         WRITE (*,
     :                   '(''   ISOPT_BINST:  setting'',
     :                   '' CONV to 0'',A)') BLEEP
                         MODE1 = 0
                      ENDIF
                      BINST = VALUE
                   ELSEIF (VALUE.LT.0.) THEN
                         WRITE (*,
     :                   '(''   ISOPT_BINST:  "b"  must be '',
     :                   ''positive'',A)') BLEEP
                   ELSE
                      BINST = VALUE
                   ENDIF
                ELSEIF (VARNAM.EQ.'BLEND') THEN
                   J = NINT(VALUE)
                   IF ((J.GE.0) .AND. (J.LE.MAXSTK)) THEN
                      BLEND = J
                      IF (BLEND.GT.NONSTK) THEN
                         WRITE (*,
     :                   '(''   ISOPT_BLEND:  warning, stack entry'',
     :                   I3,'' does not yet exist'',A)')
     :                   BLEND,BLEEP
                      ENDIF
                   ELSE
                      WRITE (*,
     :                '(''   ISOPT_BLEND:  illegal stack entry'',A)')
     :                BLEEP
                   ENDIF
                ELSEIF (VARNAM.EQ.'CONV') THEN
                   J = NINT(VALUE)
                   IF ((J.LT.0) .OR. (J.GT.2)) THEN
                      WRITE (*,
     :                '(''   ISOPT_CONV:  illegal value '',
     :                ''(must be 0-2)'',A)') BLEEP
                   ELSEIF (J.NE.2) THEN
                      MODE1 = J
                   ELSE


                      WRITE (*,
     :                '(''   ISOPT_CONV:  file containing data? '',$)')
  212                 CONTINUE
                      READ (*,'(A)') CFILE
                      CALL SSTRIP(CFILE)
                      CALL DTOUPP(CFILE)
                      IF (CFILE.EQ.'!!') THEN
                         WRITE (*,
     :                   '(''   ISOPT_CONV:  retaining old option'',A)')
     :                   BLEEP
                      ELSEIF (CFILE.EQ.' ') THEN
                         WRITE (*,
     :                   '(''   ISOPT_CONV:  please enter filename '',
     :                   ''for response function file:'',A,$)') BLEEP
                         GOTO 212
                      ELSE
                         CLOSE (69)
                         OPEN (UNIT=69,STATUS='OLD',FILE=CFILE,
     :                   IOSTAT=IHX)
                         IF (IHX.EQ.0) THEN
                            CFILE = ' '
                            READ (69,'(A)',IOSTAT=IHX) CTITLE
                            IF (IHX.EQ.0) READ (69,*,IOSTAT=IHX) NRF
                            IF (IHX.EQ.0) THEN
                               IF (NRF.GT.MAXNRF) THEN
                                  WRITE (*,
     :                            '(''   ISOPT_CONV:  too many points'',
     :                            '' in data file (max is'',I4,'')'',
     :                            A)') MAXNRF, BLEEP
                                  WRITE (*,
     :                            '(''     old options retained'')')
                               ELSE
                                  DO 214 I = 1, NRF
                                     IF (IHX.EQ.0) THEN
                                        READ (69,*,IOSTAT=IHX)
     :                                  POSN(I), RF(I)
                                     ENDIF
  214                             CONTINUE
                               ENDIF
                            ENDIF
                            IF (IHX.NE.0) THEN
                               WRITE (*,
     :                         '(''   ISOPT_CONV:  error reading from'',
     :                         '' data file;  CONV set to 0'',A)') BLEEP
                               MODE1 = 0
                            ELSE
                               MODE1 = 2
                            ENDIF
                         ELSE
                            WRITE (*,
     :                      '(''   ISOPT_CONV:  error opening '',A,
     :                      '';  retaining old options'',A)')
     :                      CFILE(1:SLEN(CFILE)), BLEEP
                         ENDIF
                      ENDIF
                      CLOSE (69)
                   ENDIF
                ELSEIF (VARNAM.EQ.'SPACE') THEN
                   J = NINT(VALUE)
                   IF ((J.GE.0) .AND. (J.LE.2)) THEN
                      VEL = J
                   ELSE
                      WRITE (*,
     :                '(''   ISOPT_SPACE:  argument must be 0-2'',A)')
     :                BLEEP
                   ENDIF
                ELSEIF (VARNAM.EQ.'V1') THEN
                   IF (VALUE.LT.V2) THEN
                      V1 = VALUE
                   ELSE
                      WRITE (*,
     :                '(''   ISOPT_V1:  velocity must be less than V2'',
     :                A)') BLEEP
                   ENDIF
                ELSEIF (VARNAM.EQ.'V2') THEN
                   IF (VALUE.GT.V1) THEN
                      V2 = VALUE
                   ELSE
                      WRITE (*,
     :                '(''   ISOPT_V2:  velocity must be greater than'',
     :                ''V1'',A)') BLEEP
                   ENDIF

*    command unrecognised

                ELSE
                   WRITE (*,
     :             '(''   ISOPT:  '',A,'' is not an option'',A)')
     :             VARNAM(1:SLEN(VARNAM)), BLEEP
                   IF (.NOT.TFILE) THEN
                      OK = .FALSE.
                      GOTO 150
                   ENDIF
                ENDIF
             ENDIF
             GOTO 200
          ENDIF
       ENDIF

  300  CONTINUE

       END
