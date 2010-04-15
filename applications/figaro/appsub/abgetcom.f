C+
      SUBROUTINE ABGETCOM(XMIN,XMAX,COMMAND,Q,NPOL,NQ,DELTLAM,
     :                                                   LIMIT,CENLAM)
C
C     A B G E T C O M
C
C     This is the menu command interpreter for ABLINE.  It reads a
C     command from the user and either handles it itself ('HELP' for
C     example) or sets one of the parameters for ABLINE which it is
C     passed.  Some commands - the ones that initiate processing,
C     such as 'CONT' - cause it to return to ABLINE.  If it returns,
C     it returns the command, expanded to the full command name.
C
C     Commands accepted
C
C         SIG   xxx     set new value for Q (Multiple of sigma)
C         DEG   nnn      "   "    "    "  NPOL (Polynomial degree)
C         ITN   nnn      "   "    "    "  NQ (Number of iterations)
C         LIMIT          "   "    "    "  LIMIT (Cut at designated limit)
C         NOLIM          "   "    "    "   "  (Search for channels)
C         WIDTH xxx      "   "    "    "  DELTLAM (Displayed width) (R)
C         CONT          fit continuum (R)
C         FIT           analyse a line and produce hard plot (R)
C         QUIT          exit program (R)
C         HELP          output help text
C         RECONT        repeat fit without reselecting (R)
C         XXXX          new centre wavelength: begin line analysis
C                       This command is given just by specifying a
C                       wavelength value.  COMMAND will be returned
C                       as 'XXXX'. (R)
C
C         (R) indicates this causes the routine to return.
C
C     Parameters -  (">" input, "!" modified, "<" output)
C
C     (>) XMIN     (Real) Minimum wavelength value
C     (>) XMAX     (Real) Maximum wavelength value
C     (<) COMMAND  (Character) Full, upper case, command causing return.
C     (!) Q        (Real) Multiple of sigma for continuum rejection
C     (!) NPOL     (Integer) Degree of polynomial for continuum fit
C     (!) NQ       (Integer) Number of iterations for continuum fit
C     (!) DELTLAM  (Real) Wavelength range displayed.
C     (!) LIMIT      (Logical) True if cut is to be at designated points
C     (!) CENLAM   (Real) central displayed wavelength
C
C                                           KS / AAO 4th Oct 1985
C     Modified:
C
C     24th Mar 1988 KS / AAO. Modified for GKS version of PGPLOT. Now
C                   uses GKD_ calls.
C     22nd Jul 1993 HME / UoE, Starlink.  Disuse GKD. Disuse PAR_Q*.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL LIMIT
      INTEGER NPOL, NQ
      REAL    XMIN, XMAX, Q, DELTLAM, CENLAM
      CHARACTER*(*) COMMAND
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_ENCODE, ICH_FOLD, ICH_KEY, ICH_NUMBR
C
C     Local variables
C
      LOGICAL CARRYON
      INTEGER INVOKE, KEY, NEXT, NUMSTA, STATUS
      REAL    VALUE
      CHARACTER*64 REPLY, STRING
C
C     Reply parameters - values are determined by the order in the
C     ICH_KEY call.
C
      INTEGER SIG, DEG, ITN, LIM, NOLIM, WIDTH, CONT, FIT,
     :        QUIT, HELP, QUEST, RECONT
      PARAMETER (SIG=1, DEG=2, ITN=3, LIM=4, NOLIM=5, WIDTH=6,
     :        CONT=7, FIT=8, QUIT=9, HELP=10, QUEST=11, RECONT=12)
C
C     This loop continues until a command is received that forces
C     a return to the main program.
C
      CARRYON=.TRUE.
      DO WHILE (CARRYON)
C
C        Issue prompt and get reply
C
         CALL PAR_CNPAR('CMD')
         CALL PAR_RDCHAR('CMD',' ',REPLY)
         IF (PAR_ABORT()) CARRYON=.FALSE.
         INVOKE=ICH_FOLD(REPLY)
C
C        First, see if the reply is a valid number, in which
C        case see if it is a valid center wavelength.
C
         STATUS=ICH_NUMBR(REPLY,1,', ',VALUE,NEXT)
         IF (STATUS.EQ.0) THEN
            IF ((VALUE.LT.XMIN).OR.(VALUE.GT.XMAX)) THEN
               CALL PAR_WRUSER('Center wavelength outside data range',
     :                                                         STATUS)
            ELSE
               CENLAM=VALUE
               COMMAND='XXXX'
               CARRYON=.FALSE.
            END IF
         ELSE
C
C           Otherwise, see if it is one of the recognised commands,
C
            KEY=ICH_KEY(REPLY,1,', ',
     :                   'SIG:DEG:ITN:LIMIT:NOLIM:WIDTH:CONT:FIT:'//
     :                           'QUIT:HELP:?:RECONT:','Abbr.',NEXT)
            IF (KEY.EQ.0) THEN
               IF (REPLY.NE.' ') THEN
                  CALL PAR_WRUSER(
     :              'Unrecognised command.  Use "?" or "HELP" for help',
     :                                                           STATUS)
               END IF
            ELSE
C
C              If it is a recognised command, see if it is followed
C              by a number.  (Not all commands need them, but we look
C              anyway.)
C
               NUMSTA=ICH_NUMBR(REPLY,NEXT,', ',VALUE,NEXT)
               IF (NUMSTA.GT.0) THEN
                  CALL PAR_WRUSER('Invalid number specified',STATUS)
               END IF
            END IF
C
            IF (KEY.EQ.SIG) THEN
C
C              'SIG xxx'
C
               IF (NUMSTA.NE.0) THEN
                  CALL PAR_CNPAR('SIG')
                  CALL PAR_RDVAL('SIG',0.,100.,Q,' ',VALUE)
                  IF (PAR_ABORT()) CARRYON=.FALSE.
                  Q=VALUE
               ELSE
                  IF ((VALUE.GT.100.).OR.(VALUE.LE.0.)) THEN
                     CALL PAR_WRUSER('Sig must be between 0 and 100',
     :                                                        STATUS)
                  ELSE
                     Q=VALUE
                  END IF
               END IF
C
            ELSE IF (KEY.EQ.DEG) THEN
C
C              'DEG n'
C
               IF (NUMSTA.NE.0) THEN
                  CALL PAR_CNPAR('DEG')
                  CALL PAR_RDVAL('DEG',0.,7.,FLOAT(NPOL),' ',VALUE)
                  IF (PAR_ABORT()) CARRYON=.FALSE.
                  NPOL=VALUE
               ELSE
                  IF ((VALUE.GT.7.).OR.(VALUE.LT.0.)) THEN
                     CALL PAR_WRUSER('Deg must be between 0 and 7',
     :                                                        STATUS)
                  ELSE
                     NPOL=VALUE
                  END IF
               END IF
C
            ELSE IF (KEY.EQ.ITN) THEN
C
C              'ITN nnn'
C
               IF (NUMSTA.NE.0) THEN
                  CALL PAR_CNPAR('ITN')
                  CALL PAR_RDVAL('ITN',0.,100.,FLOAT(NQ),' ',VALUE)
                  IF (PAR_ABORT()) CARRYON=.FALSE.
                  NQ=VALUE
               ELSE
                  IF ((VALUE.GT.100.).OR.(VALUE.LE.0.)) THEN
                     CALL PAR_WRUSER('Itn must be between 0 and 100',
     :                                                        STATUS)
                  ELSE
                     NQ=VALUE
                  END IF
               END IF
C
            ELSE IF (KEY.EQ.WIDTH) THEN
C
C              'WIDTH xxx'
C
               IF (NUMSTA.NE.0) THEN
                  CALL PAR_CNPAR('WIDTH')
                  CALL PAR_RDVAL('WIDTH',0.001,XMAX-XMIN,DELTLAM,
     :               ' ',VALUE)
                  IF (PAR_ABORT()) CARRYON=.FALSE.
                  DELTLAM=VALUE
                  COMMAND='WIDTH'
                  CARRYON=.FALSE.
               ELSE
                  IF ((VALUE.GT.(XMAX-XMIN)).OR.(VALUE.LE.0.)) THEN
                     STRING='Width must be between 0 and '
                     INVOKE=ICH_ENCODE(STRING,XMAX-XMIN,29,7,NEXT)
                     CALL PAR_WRUSER(STRING(:NEXT),STATUS)
                  ELSE
                     DELTLAM=VALUE
                     COMMAND='WIDTH'
                     CARRYON=.FALSE.
                  END IF
               END IF
C
            ELSE IF (KEY.EQ.LIM) THEN
C
C              'LIMIT'
C
               IF (LIMIT) THEN
                  CALL PAR_WRUSER('LIMIT was already specified',STATUS)
               END IF
               LIMIT=.TRUE.
C
            ELSE IF (KEY.EQ.NOLIM) THEN
C
C              'NOLIM'
C
               IF (.NOT.LIMIT) THEN
                  CALL PAR_WRUSER('NOLIM was already specified',STATUS)
               END IF
               LIMIT=.FALSE.
C
            ELSE IF (KEY.EQ.CONT) THEN
C
C              'CONT'
C
               COMMAND='CONT'
               CARRYON=.FALSE.
C
            ELSE IF (KEY.EQ.FIT) THEN
C
C              'FIT'
C
               COMMAND='FIT'
               CARRYON=.FALSE.
C
            ELSE IF (KEY.EQ.QUIT) THEN
C
C              'QUIT'
C
               COMMAND='QUIT'
               CARRYON=.FALSE.
C
            ELSE IF (KEY.EQ.RECONT) THEN
C
C              'RECONT'
C
               COMMAND='RECONT'
               CARRYON=.FALSE.
C
            ELSE IF ((KEY.EQ.HELP).OR.(KEY.EQ.QUEST)) THEN
C
C              'HELP'
C
               CALL FIG_HELP('abline',STATUS)
               IF (STATUS.NE.0) THEN
                  CALL PAR_WRUSER('Unable to access help text file',
     :                                                       STATUS)
               END IF
            END IF
         END IF
      END DO
C
      END
