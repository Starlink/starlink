*   SUBROUTINE CALLED FROM ISPROG TO GET ATM
*   DATA FROM FILE ATOMIC.DAT

       SUBROUTINE ISATM (WAVE0,WAVE,F1K,Q1,Q1K,ATM,OK,STATUS)

       INCLUDE 'SAE_PAR'
       INTEGER STATUS


       LOGICAL ATM,FOUND,OK,INITIAL,SEARCH,REVIEW
       INTEGER Q1,Q1K
       INTEGER SLEN
       REAL WAVE,WAVE0,F1K
       CHARACTER LABEL*15,BLANK
       CHARACTER*40 TXTINP
       CHARACTER*40 ATTXT, ATHEAD, ATHEDK, ATFILE
       CHARACTER*40 STEMP

       CHARACTER*80   prefix
       CHARACTER*80   prefix2
       CHARACTER*80   prefix3
       CHARACTER*80   prefix4
       INTEGER        plen,p2len,p3len,p4len

       COMMON /prefix1/ prefix,prefix2,prefix3,prefix4
       COMMON /prefix2/ plen,p2len,p3len,p4len

       CHARACTER * 1 BLEEP
       COMMON /BLEEP/ BLEEP

       COMMON /ATHEDC/ ATHEDK

       DATA FOUND /.FALSE./

       IF( STATUS .NE. SAI__OK ) RETURN

       REVIEW = .FALSE.

*    If wavelength not passed inquire user

       OK = .TRUE.
       INITIAL = .TRUE.
       IF (WAVE.EQ.0.0) THEN
          INITIAL = .FALSE.
          GO TO 500
       ENDIF

*    Try to locate atomic data file

   10  CONTINUE
       IF (WAVE.NE.0.0) THEN
          ATM = .FALSE.
          WAVE0 = 0.0
          F1K = 0.0
          Q1 = 0
          Q1K = 0.0
   15     CONTINUE
          ATFILE = 'ATOMIC.DAT'
          OPEN (UNIT=68,FILE=ATFILE,STATUS='OLD',
     :    IOSTAT=IOSDAT)
          IF (IOSDAT.NE.0) THEN
             CLOSE (68)
             ATFILE = prefix2(1:p2len)//'ATOMIC.DAT'
             OPEN (UNIT=68,FILE=ATFILE,STATUS='OLD',
     :       IOSTAT=IOSDAT)
          ENDIF
          IF (IOSDAT.NE.0) THEN
             CLOSE (68)
             ATFILE = prefix(1:plen)//'ATOMIC.DAT'
             OPEN (UNIT=68,FILE=ATFILE,STATUS='OLD',
     :       IOSTAT=IOSDAT)
          ENDIF
          IF (IOSDAT.NE.0) THEN
             WRITE (*,'(''   ISATM:  Unable to open '',A)')
     :       ATFILE(1:SLEN(ATFILE))
             CLOSE (68)
             GO TO 500
          END IF

*    Search until wavelength coincidence established

          DO I=1,1000

   20        CONTINUE
             ATTXT = ' '
             READ (68,'(A)',END=200) ATTXT
             IF (ATTXT(1:1).EQ.'!' .OR. ATTXT(1:1).EQ.'*') THEN
                GO TO 20
             ENDIF
             CALL SSTRIP(ATTXT)
             ATHEAD = ATTXT

   30        CONTINUE
             ATTXT = ' '
             READ (68,'(A)',END=200) ATTXT
             IF (ATTXT(1:1).EQ.'!' .OR. ATTXT(1:1).EQ.'*') THEN
                GO TO 30
             ENDIF

   40        CONTINUE
             STEMP = ' '
             READ (68,'(A)',END=200) STEMP(1:40)
             IF (STEMP(1:1).EQ.'!' .OR. STEMP(1:1).EQ.'*') THEN
                GO TO 40
             ENDIF

*    List to screen for review

             IF (REVIEW) THEN
                STEMP = ATHEAD(1:LEN(STEMP))
                CALL DTOUPP(STEMP)
                J = MIN(LEN(STEMP),LENR)
                CALL SSTRIP(STEMP)
                IF
     :          ((LENR.EQ.0) .OR.
     :          (STEMP(1:J).EQ.TXTINP(1:J))) THEN
                   IF (STEMP.EQ.' ') GOTO 20
                   STEMP(1:) = ATHEAD(1:)
                   STEMP(21:) = ATTXT(1:)
                   WRITE (*,'(''   '',A)') STEMP(1:SLEN(STEMP))
                ENDIF
                GOTO 20
             ENDIF

             READ (ATTXT,*,ERR=100) WAVE0, Q1, Q1K, F1K
             IF (ABS(WAVE0-WAVE).LT.0.1) THEN
                FOUND=.TRUE.
                ATHEDK = ATHEAD
                GOTO 300
             END IF
          END DO
          GO TO 300
  100     CONTINUE
          WRITE (*,'(''   ISATM:  error reading from '',A,A)')
     :    ATFILE(1:SLEN(ATFILE)),BLEEP
          CLOSE(68)
          GO TO 500
  200     CONTINUE
          IF (.NOT.REVIEW)
     :    WRITE (*,'(''   ISATM:  line'',F8.2,''A not found in '',
     :    A,A)') WAVE, ATFILE(1:SLEN(ATFILE)),BLEEP
          CLOSE (68)
          GOTO 500
* ..if matched then set flag ATM to true, else it is false..
  300     CONTINUE
          ATM=.TRUE.
          CLOSE(68)
       ENDIF

*
*   Write atomic data to screen
*

  490  CONTINUE
       WRITE (*,
     : '(''   ISATM:  '',A/
     :   ''           Wavelength (A) -             '',
     :   F8.2/
     :   ''           f (oscillator strength) - '',
     :   0PD11.4/
     :   ''           Gl (stat. weight of lower level) -'',
     :   I3/
     :   ''           Gu (stat. weight of lower level) -'',
     :   I3)') ATHEDK(1:SLEN(ATHEDK)), WAVE0, F1K, Q1, Q1K

*
*   Return if only acquiring data (non-edit)
*

       IF (INITIAL) GOTO 600

  500  CONTINUE
       CLOSE (68)
       INITIAL = .FALSE.
       TXTINP = ' '
       REVIEW = .FALSE.

       IF (OK) THEN
          CALL RDSTR( ' ', '   ISATM> ', ' ', TXTINP, STATUS )
       ELSE
          CALL RDSTR( ' ', '   ISATM> '//BLEEP, ' ', TXTINP, STATUS )
          OK = .TRUE.
       ENDIF

       IF( STATUS .NE. SAI__OK ) THEN
          OK = .FALSE.
          GO TO 600
       END IF

       I = INDEX(TXTINP,'=')
       IF (I.NE.0) TXTINP(I:I) = ' '
       I = INDEX(TXTINP,':')
       IF (I.NE.0) TXTINP(I:I) = ' '
       IF (TXTINP.EQ.' ') GOTO 500
       CALL SSTRIP(TXTINP)
       IF (TXTINP(1:1).EQ.'T' .OR. TXTINP(1:1).EQ.'t') THEN
          I = INDEX(TXTINP,' ')
          ATHEDK(1:) = TXTINP(I:)
          CALL SSTRIP (ATHEDK)
          GO TO 500
       ELSE
          CALL DTOUPP(TXTINP)
       ENDIF
       IF (TXTINP(1:1).EQ.'L') GO TO 490
       IF (TXTINP(1:1).EQ.'H') THEN
          WRITE (*,
     :    '(''   ISATM_H:''/
     :      ''   Syntax is <Parameter> <operator> <value>''/
     :      ''   The operator can be, optionally and'',
     :      '' equivalently, blank, "=", or ":"''/
     :      ''   Parameters are:''/
     :      ''       f   -  change oscillator strength''/
     :      ''       Gl  -  change lower stat. wt.''/
     :      ''       Gu  -  change upper stat. wt.''/
     :      ''       S   -  Search for atomic data in ATOMIC.DAT''/
     :      ''              (where <value> is wavelength in Ang.)''/
     :      ''       T   -  change title''/
     :      ''       W   -  change wavelength''/
     :      ''   Other valid commands are:''/
     :      ''       H   -  Help ''/
     :      ''       L   -  List current data ''/
     :      ''       R   -  review available atomic data''/
     :      ''              e.g. "R C" to review carbon lines,''/
     :      ''              "R C II" to review C+ lines, etc.''/
     :      ''       QIS -  return to DIPSO '')')
        ELSEIF (TXTINP(1:3).EQ.'QIS') THEN
           ATM = .TRUE.
           IF ((WAVE0.EQ.0.0).OR.(F1K.EQ.0.0).OR.
     :     (Q1.EQ.0) .OR. (Q1K.EQ.0)) THEN
              ATM = .FALSE.
              OK = .FALSE.
              WRITE (*,
     :        '(''   ISATM:  parameter(s) left undefined'')')
           ENDIF
           GOTO 600
        ELSEIF (TXTINP(1:1).EQ.'Q') THEN
           WRITE (*,
     :     '(''   ISATM:  to quit, use QIS'',A)') BLEEP
        ELSEIF (TXTINP(1:1).EQ.'W') THEN
           TXTINP(1:) = TXTINP((INDEX(TXTINP(1:),' ')):)
           CALL SSTRIP(TXTINP)
           CALL DECODE
     :     ('ISATM_W',TXTINP,1,1,TEMP,'Wavelength_(A) ',OK)
           IF (OK) WAVE0 = TEMP
        ELSEIF (TXTINP(1:1).EQ.'S') THEN
           TXTINP(1:) = TXTINP((INDEX(TXTINP(1:),' ')):)
           CALL SSTRIP(TXTINP)
           CALL DECODE
     :     ('ISATM_S',TXTINP,1,1,TEMP,'Wavelength_(A) ',OK)
           IF (OK) THEN
              WAVE = TEMP
              GO TO 10
           ENDIF
        ELSEIF (TXTINP(1:1).EQ.'R') THEN
           REVIEW = .TRUE.
           TXTINP(1:) = TXTINP((INDEX(TXTINP(1:),' ')):)
           CALL SSTRIP(TXTINP)
           IF (TXTINP.EQ.' ') THEN
              LENR = 0
           ELSE
              I = INDEX(TXTINP,' ')
              IF (TXTINP(I:).EQ.' ') THEN
                 LENR = I
              ELSE
  550            CONTINUE
                 IF (TXTINP(I+1:I+1).EQ.' ') THEN
                    TXTINP(I:) = TXTINP(I+1:)
                    GOTO 550
                 ENDIF
                 LENR = INDEX(TXTINP(I+1:),' ') + I
              ENDIF
           ENDIF
           WRITE (*,
     :     '(''   ISATM_R - data found in file:'')')
           GOTO 15
        ELSEIF (TXTINP(1:1).EQ.'F') THEN
           TXTINP(1:) = TXTINP((INDEX(TXTINP(1:),' ')):)
           CALL SSTRIP(TXTINP)
           CALL DECODE
     :     ('ISATM_f',TXTINP,1,1,TEMP,'Oscillator_strength ',OK)
           IF (OK) F1K = TEMP
       ELSEIF (TXTINP(1:2).EQ.'GU') THEN
          TXTINP(1:) = TXTINP((INDEX(TXTINP(1:),' ')):)
          CALL SSTRIP(TXTINP)
          CALL DECODE
     :    ('ISATM_Gu',TXTINP,1,1,TEMP,'G(upper) ',OK)
          IF (OK) Q1K = NINT(TEMP)
       ELSEIF (TXTINP(1:2).EQ.'GL') THEN
          TXTINP(1:) = TXTINP((INDEX(TXTINP(1:),' ')):)
          CALL SSTRIP(TXTINP)
          CALL DECODE
     :    ('ISATM_Gl',TXTINP,1,1,TEMP,'G(lower) ',OK)
          IF (OK) Q1 = NINT(TEMP)
       ELSE
          WRITE (*,
     :    '(''   ISATM: '',A,'' input not recognised'')')
     :    TXTINP(1:(INDEX(TXTINP,' ')-1))
          OK = .FALSE.
       ENDIF
       GO TO 500

  600  CONTINUE

       END
