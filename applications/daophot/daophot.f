C
C======================================================================
C
C This is the mainline program, DAOPHOT.
C
C                OFFICIAL DAO VERSION: 1991 April 18
C
C The purposes of this snatch of code are threefold:
C (1) to type out a message from the local DAOPHOT curator, letting
C     the user know of any recent changes in the code;
C (2) to look for a file named 'daophot.opt' in the current default
C     directory and, if it is found, to read in values for the optional
C     parameters [otherwise set them at default values]; and
C (3) to accept commands from the keyboard and call the appropriate
C     subroutines.
C
C
C=======================================================================
C
      IMPLICIT NONE

*  History:
*     19-Feb-1992 (NE)
*       Updated the number of arguments in calls to RDHEAD (in APPEND) and
*       MMM (in SKY).
*     17-Mar-1995 (GJP)
*       Replaced very negative, very large or very small numbers with
*       their PRM_PAR equivalents.
*     21-Mar-2000 (MBT)
*       Removed definition of some constants to external include file.
*     6-Jun-2000 (MBT)
*       Modified for dynamic memory allocation.

*  Global Constants:
      INCLUDE 'DAT_PAR'               ! HDS/DAT parameters
      INCLUDE 'CNF_PAR'               ! For CNF_PVAL function

      INTEGER NOPT, NCMD, MAXPSF, MAXEXP, MAXBOX, MAXSKY,
     .     MAXPAR
      PARAMETER (NOPT=21, NCMD=23, MAXPSF=145,
     .     MAXEXP=6, MAXPAR=6, MAXBOX=13, MAXSKY=10000)
C
C Parameters
C
C NOPT is the number of optional parameters which may be altered by the
C      user.
C
C NCMD is the number of commands are recognized by the program.
C
C                      WARNING
C
C IF ANY OF THE FOLLOWING THREE PARAMETERS IS CHANGED, IT MUST
C ALSO BE CHANGED IN THE FILE CONTAINING THE ROUTINE GETPSF.
C MAXPAR MUST BE CHANGED IN TWO PLACES IN THAT FILE.
C
C MAXPSF is the maximum size of the PSF arrays.
C
C MAXEXP is the maximum number of PSF lookup tables allowed.
C        MAXEXP must be at least 2, because I use PSF as scratch
C        space in FIND.
C
C MAXPAR is the maximum number of parameters allowed in the analytic
C        part of the model PSF.
C
C                End of WARNING
C
C MAXBOX is the length of the side of the largest box that will
C        be used for convolving the image in FIND.  MAXBOX must
C        be less than MAXPSF, because I use the array PSF as
C        scratch space in FIND.
C
C MAXSKY is the maximum number of pixels that will be used in any
C        sky determination.  This will affect the speed of the
C        sky determinations in the SKY and FIND routines (more
C        pixels means slower, of course).  It will also affect
C        the maximum outer sky radius in the PHOTOMETRY routine.
C
      CHARACTER*26 LBL(NOPT)
      CHARACTER*10 CMD(NCMD)
      CHARACTER*1 ANSWER, SPACE
      CHARACTER*(DAT__SZTYP) GENTYP
      INTEGER MAXSTR
      INTEGER IPWK, IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8
      INTEGER CMDWK2(NCMD)
      REAL PSF(MAXPSF,MAXPSF,MAXEXP), PAR(MAXPAR)
      REAL OPT(NOPT), OMIN(NOPT), OMAX(NOPT)
      REAL CMDWK1(NCMD)
C
      INTEGER MIN0, ICNVRT
C
      CHARACTER*132 MSGLINE
      CHARACTER*30 FILE, OPTFIL, MSGFIL, CASE
      REAL SKYMN, SKYMED, SKYMOD
      INTEGER I, K, NCOL, NROW, KEY, ISTAT, MAX, MAXB
C     INTEGER MADRID                      ! MIDAS
      LOGICAL OPEN
C
C     INCLUDE 'MID_INCLUDE:ST_DEF.INC'    ! MIDAS
C     COMMON /VMR/ MADRID                 ! MIDAS
      COMMON /SIZE/ NCOL, NROW
C     INCLUDE 'MID_INCLUDE:ST_DAT.INC'    ! MIDAS
C
      DATA SPACE / ' ' /
C
C Specify default values for the NOPT optional parameters.
C
      DATA LBL/' READ NOISE (ADU; 1 frame)',   !  1
     .         '    GAIN (e-/ADU; 1 frame)',   !  2
     .         'LOW GOOD DATUM (in sigmas)',   !  3
     .         '  HIGH GOOD DATUM (in ADU)',   !  4
     .         '            FWHM OF OBJECT',   !  5
     .         '     THRESHOLD (in sigmas)',   !  6
     .         ' LS (LOW SHARPNESS CUTOFF)',   !  7
     .         'HS (HIGH SHARPNESS CUTOFF)',   !  8
     .         ' LR (LOW ROUNDNESS CUTOFF)',   !  9
     .         'HR (HIGH ROUNDNESS CUTOFF)',   ! 10
     .         '            WATCH PROGRESS',   ! 11
     .         '            FITTING RADIUS',   ! 12
     .         '                PSF RADIUS',   ! 13
     .         '              VARIABLE PSF',   ! 14
     .         'FRACTIONAL-PIXEL EXPANSION',   ! 15
     .         '        ANALYTIC MODEL PSF',   ! 16
     .         ' EXTRA PSF CLEANING PASSES',   ! 17
     .         '   USE SATURATED PSF STARS',   ! 18
     .         '      PERCENT ERROR (in %)',   ! 19
     .         '      PROFILE ERROR (in %)',   ! 20
     .         '    MS (MAX STARS IN FILE)'/   ! 21
      DATA OPT / 0., 0., 7., 32766.5, 2.5, 4., 0.2, 1., -1., 1.,
     .           1., 2., 11., 0., 0., 1., 0., 0., 0.75, 5., 5000./
      DATA OMIN / 1.E-30, 1.E-30, 0., 0., 0.2 , 0., 0., 0.6, -2.,
     .            0., -2., 1., .5, -1.5, -0.5, -6.5, 0., 0., 0., 0.,
     .            10./
      DATA OMAX / 1.E30, 1.E30, 1.E30, 999999.99, 15., 1.E30, 0.6,
     .            2., 0., 2., 2., 20., 35., 2.5, 1.5, 6.5, 9.5, 1.,
     .            100., 100., 1000000./
C
C   LBL contains parameter names for displaying on the terminal.
C  OMIN and OMAX contain the minimum and maximum acceptable values for
C       the parameters.
C
C Define the NCMD legal command names.  Note that since a command is
C recognized by its first two letters, the first two letters of
C each command must be unique.
C
      DATA CMD/ 'HELP', 'OPTION', 'MONITOR', 'NOMONITOR', 'SORT',
     .     'OFFSET', 'GROUP', 'SELECT', 'APPEND', 'EXIT', 'ATTACH',
     .     'LIST', 'SKY', 'FIND', 'PHOTOMETRY', 'PSF', 'PEAK',
     .     'NSTAR', 'SUB*', 'ADD*', 'DUMP', 'FUDGE', 'PICK'/
C
C
C Set the generic type for workspace allocation.  This type must be
C large enough to be used for an INTEGER or for a REAL variable.
C
      GENTYP = '_INTEGER'
C
      OPEN = .FALSE.
C     CALL STSPRO ('-1')                                    ! MIDAS
C     CALL STECNT ('PUT', 1, 0, 0)                          ! MIDAS
      CALL FABORT
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Handle the message file.  Note that in the data statement above, the
C file is described as residing in logical device DAO:.  Therefore,
C this logical device must have been defined before the program is run.
C
C Type out the news headlines from the message file, ask whether the
C user wants to read further, and if he/she does, type out the
C messages.
C
      OPTFIL = CASE('daophot.opt')
      MSGFIL = CASE('dao:daophot.msg')
      CALL INFILE (2, MSGFIL, ISTAT)
      IF (ISTAT .LT. 0) GO TO 2010
C
C This little section reads in and types out the headlines.  A
C question mark (?) in column 1 of the input marks the query whether the
C user wishes to continue reading.
C
 1010 CALL TBLANK                                    ! Type a blank line
 1020 CALL RDCHAR (2, MSGLINE, K, ISTAT)
      IF (ISTAT .LT. 0) THEN
         GO TO 1020
      ELSE IF (ISTAT .GT. 0) THEN
         GO TO 2010
      END IF
      IF (MSGLINE(1:1) .EQ. '?') GO TO 1030
      IF (K .LE. 0) GO TO 1010
      WRITE (6,610) MSGLINE(1:K)
  610 FORMAT (1X, A)
      GO TO 1020
C
 1030 CALL GETYN (MSGLINE(2:K), ANSWER)
C
C Read the user's answer.  If it is 'Y' or 'y', continue reading in and
C typing out the message file, until another backslash encountered in
C column 1 marks the end of a section.  If the answer is anything
C except 'Y' or 'y', close the message file and go on to the next
C section of the program.
C
      IF (ANSWER .EQ. 'Y') GO TO 1010
      CALL CLFILE (2)
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Set up the values of the optional parameters.
C
C (1) Call OPTION with OPTFIL = 'daophot.opt' to set initial values
C for the optional parameters.  If the file isn't there, the routine
C will check that the default values (specified in the data statement
C above) are valid, and return here with those values intact.
C
 2010 CALL OPTION (OPTFIL, NOPT, LBL, OPT, OMIN, OMAX,
     .     'OPT>', ISTAT)
      MAXSTR=INT(OPT(21))
C
C Set OPTFIL to 'KEYBOARD INPUT' for the next time the user uses the
C 'OPTION' command.
C
      OPTFIL='KEYBOARD INPUT'
C
C-----------------------------------------------------------------------
C
C SECTION 3
C
C Accept and execute commands, one after another.
C
C The 22 commands currently defined are:  (1) HELP, (2) OPT, (3) MON,
C (4) NOMON, (5) SORT, (6) OFFSET, (7) GROUP, (8) SELECT, (9) APPEND,
C (10) EXIT, (11) ATTACH, (12) LIST, (13) SKY, (14) FIND, (15) PHOT,
C (16) PSF, (17) PEAK, (18) NSTAR, (19) SUB*, (20) ADD*, (21) DUMP,
C and (22) FUDGE.
C
C Command 13 is 'open a picture file'.  Commands 1-12 can be executed
C without a picture file open; commands 14-23 require that a picture
C file be open.
C
 3000 CONTINUE
      CALL TBLANK
      CALL INQUIR ('Command:', 8)
      CALL RDCHAR (5, MSGLINE, K, ISTAT)
      IF (ISTAT .GT. 0) THEN
         IF (OPEN) CALL CLPIC ('DATA')
         CALL TBLANK
         CALL BYEBYE
      ELSE IF ((ISTAT .LT. 0) .OR. (K .LT. 2)) THEN
         GO TO 3000
      END IF
C
      KEY=ICNVRT(MSGLINE(1:2))
      IF (KEY .EQ. ICNVRT('HE')) THEN
C
C We use the ICNVRT function here, instead of just checking MSGLINE(1:2)
C against the first two characters of each command, just in case the
C user is using lower case letters.
C
         CALL HELP (CMD, NCMD, CMDWK1, CMDWK2)
C
      ELSE IF (KEY .EQ. ICNVRT('OP')) THEN
         CALL TBLANK
         OPTFIL = 'KEYBOARD INPUT'
         CALL GETNAM ('File with parameter values:', OPTFIL)
         CALL OPTION (OPTFIL, NOPT, LBL, OPT, OMIN, OMAX,
     .        'OPT>', ISTAT)
         MAXSTR=INT(OPT(21))
C
      ELSE IF (KEY .EQ. ICNVRT('MO')) THEN
         OPT(11)=1.
C
      ELSE IF (KEY .EQ. ICNVRT('NO')) THEN
         OPT(11)=0.
C
      ELSE IF (KEY .EQ. ICNVRT('SO')) THEN
C
C Note that allocation of the first work array here is not done properly;
C the SORTER routine packs other data types into this array.  Allocating
C it properly would require some rewriting of SORTER; without this the
C code is vulnerable to problems if assumptions about the relative sizes
C of some data types turn out to be wrong.
C
         CALL DAO_ALLOC( '_REAL', MAXSTR*80, IP1 )
         CALL DAO_ALLOC( '_REAL', MAXSTR, IP2 )
         CALL DAO_ALLOC( '_INTEGER', MAXSTR, IP3 )
         CALL DAO_ALLOC( '_INTEGER', MAXSTR, IP4 )
         CALL DAO_ALLOC( '_INTEGER', MAXSTR, IP5 )
         CALL SORTER(%VAL(CNF_PVAL(IP1)), MAXSTR*80,
     :               %VAL(CNF_PVAL(IP2)), %VAL(CNF_PVAL(IP3)),
     .        %VAL(CNF_PVAL(IP4)), %VAL(CNF_PVAL(IP5)), MAXSTR, OPT(11))
         CALL DAO_DEALL( IP5 )
         CALL DAO_DEALL( IP4 )
         CALL DAO_DEALL( IP3 )
         CALL DAO_DEALL( IP2 )
         CALL DAO_DEALL( IP1 )
C
      ELSE IF (KEY .EQ. ICNVRT('OF')) THEN
         CALL OFFSET
C
      ELSE IF (KEY .EQ. ICNVRT('SE')) THEN
         CALL DAO_ALLOC( '_INTEGER', MAXSTR, IP1 )
         CALL DAO_ALLOC( '_REAL', MAXSTR, IP2 )
         CALL DAO_ALLOC( '_REAL', MAXSTR, IP3 )
         CALL DAO_ALLOC( '_REAL', MAXSTR, IP4 )
         CALL DAO_ALLOC( '_REAL', MAXSTR, IP5 )
         CALL DAOSLT (%VAL(CNF_PVAL(IP1)), %VAL(CNF_PVAL(IP2)),
     :                %VAL(CNF_PVAL(IP3)), %VAL(CNF_PVAL(IP4)),
     .        %VAL(CNF_PVAL(IP5)), MAXSTR )
         CALL DAO_DEALL( IP5 )
         CALL DAO_DEALL( IP4 )
         CALL DAO_DEALL( IP3 )
         CALL DAO_DEALL( IP2 )
         CALL DAO_DEALL( IP1 )
C
      ELSE IF (KEY .EQ. ICNVRT('AP')) THEN
         CALL APPEND
C
      ELSE IF (KEY .EQ. ICNVRT('PI')) THEN
         CALL DAO_ALLOC( '_INTEGER', MAXSTR, IP1 )
         CALL DAO_ALLOC( '_REAL', MAXSTR, IP2 )
         CALL DAO_ALLOC( '_REAL', MAXSTR, IP3 )
         CALL DAO_ALLOC( '_REAL', MAXSTR, IP4 )
         CALL DAO_ALLOC( '_REAL', MAXSTR, IP5 )
         CALL DAO_ALLOC( '_INTEGER', MAXSTR, IP6 )
         CALL PCKPSF (%VAL(CNF_PVAL(IP1)), %VAL(CNF_PVAL(IP2)),
     :                %VAL(CNF_PVAL(IP3)), %VAL(CNF_PVAL(IP4)),
     .        %VAL(CNF_PVAL(IP5)), %VAL(CNF_PVAL(IP6)),
     :        MAXSTR, OPT(12), OPT(13))
         CALL DAO_DEALL( IP6 )
         CALL DAO_DEALL( IP5 )
         CALL DAO_DEALL( IP4 )
         CALL DAO_DEALL( IP3 )
         CALL DAO_DEALL( IP2 )
         CALL DAO_DEALL( IP1 )
C
      ELSE IF (KEY .EQ. ICNVRT('EX')) THEN
         IF (OPEN) THEN
            CALL CLPIC ('DATA')
            CALL DAO_DEALL( IPWK )
         END IF
         CALL TBLANK
         CALL BYEBYE
C
      ELSE IF (KEY .EQ. ICNVRT('AT')) THEN
C
C This is an ATTACH command.  First, get the file name (if any) out of
C the command line.  Then allocate a non-specific work array - for various
C tasks more workspace will be needed but enough of them require an
C integer or real array the size of the image that it is efficient to
C allocate one of those here.
C
C *** NOTE *** we allocate an _INTEGER array on the assumption that
C the _INTEGER type is as large, or larger than, _REAL type.
C
         FILE=' '
         DO 3100 I=1,K
            IF (MSGLINE(I:I) .EQ. ' ') THEN
               FILE=MSGLINE( I+1 : MIN0(K,132))
               GO TO 3110
            END IF
 3100    CONTINUE
 3110    CONTINUE
         IF ( OPEN ) THEN
            CALL DAO_DEALL( IPWK )
         END IF
         CALL ATTACH (FILE, OPEN)
         IF ( OPEN ) THEN
            CALL DAO_ALLOC( GENTYP, NCOL*NROW, IPWK )
         END IF
C
      ELSE IF (KEY .EQ. ICNVRT('LI')) THEN
         IF (OPEN) THEN
            CALL LIST (FILE)
         ELSE
            CALL STUPID ('   No picture file has been ATTACHed.')
         END IF
C
      ELSE IF (KEY .EQ. ICNVRT('SK')) THEN
         IF (OPEN) THEN
            CALL DAO_ALLOC( '_REAL', MAXSKY, IP1 )
            CALL DAO_ALLOC( '_REAL', MAXSKY, IP2 )
            CALL DAO_ALLOC( '_INTEGER', MAXSKY, IP3 )
            CALL SKY (%VAL(CNF_PVAL(IP1)), %VAL(CNF_PVAL(IP2)),
     :                %VAL(CNF_PVAL(IP3)), MAXSKY, OPT(4),
     .           SKYMN, SKYMED, SKYMOD, K)
            CALL DAO_DEALL( IP3 )
            CALL DAO_DEALL( IP2 )
            CALL DAO_DEALL( IP1 )
            WRITE (6,6) SKYMN, SKYMED, K
    6       FORMAT ('              Clipped mean and median =', 2F9.1/
     .         '   Number of pixels used (after clip) =', I7)
         ELSE
            CALL STUPID ('   No picture file has been ATTACHed.')
         END IF
C
      ELSE IF (KEY .EQ. ICNVRT('FI')) THEN
         IF (OPEN) THEN
            CALL DAO_ALLOC( '_REAL', MAXBOX*NCOL, IP1 )
            CALL DAO_ALLOC( '_REAL', MAXBOX*NCOL, IP2 )
            CALL DAO_ALLOC( '_REAL', MAXBOX*MAXBOX, IP3 )
            CALL DAO_ALLOC( '_LOGICAL', MAXBOX*MAXBOX, IP4 )
            CALL FIND(%VAL(CNF_PVAL(IP1)), %VAL(CNF_PVAL(IP2)),
     :                %VAL(CNF_PVAL(IPWK)), %VAL(CNF_PVAL(IP3)),
     .                %VAL(CNF_PVAL(IP4)),
     :                NCOL*NROW, MAXBOX, NCOL, MAXSKY, OPT,
     .                NOPT)
            CALL DAO_DEALL( IP4 )
            CALL DAO_DEALL( IP3 )
            CALL DAO_DEALL( IP2 )
            CALL DAO_DEALL( IP1 )
         ELSE
            CALL STUPID ('   No picture file has been ATTACHed.')
         END IF
C
      ELSE IF (KEY .EQ. ICNVRT('PH')) THEN
         IF (OPEN) THEN
            K = MAXEXP/2
            MAX = MAXPSF*MAXPSF*K
            K = K+1
            CALL PHOTSB (%VAL(CNF_PVAL(IPWK)),
     :                   PSF, PSF(1,1,K), MAX, NCOL, NROW,
     .           OPT(11))
         ELSE
            CALL STUPID ('   No picture file has been ATTACHed.')
         END IF
C
      ELSE IF (KEY .EQ. ICNVRT('PS')) THEN
         IF (OPEN) THEN
            CALL DAO_ALLOC( '_REAL', MAXSTR, IP1 )
            CALL DAO_ALLOC( '_REAL', MAXSTR, IP2 )
            CALL DAO_ALLOC( '_REAL', MAXSTR, IP3 )
            CALL DAO_ALLOC( '_REAL', MAXSTR, IP4 )
            CALL DAO_ALLOC( '_INTEGER', MAXSTR, IP5 )
            CALL GETPSF (%VAL(CNF_PVAL(IPWK)), NCOL, NROW, PAR, PSF,
     .           %VAL(CNF_PVAL(IP1)), %VAL(CNF_PVAL(IP2)),
     :           %VAL(CNF_PVAL(IP3)), %VAL(CNF_PVAL(IP4)),
     :           %VAL(CNF_PVAL(IP5)),
     .           MAXSTR, OPT, NOPT)
            CALL DAO_DEALL( IP5 )
            CALL DAO_DEALL( IP4 )
            CALL DAO_DEALL( IP3 )
            CALL DAO_DEALL( IP2 )
            CALL DAO_DEALL( IP1 )
         ELSE
            CALL STUPID ('   No picture file has been ATTACHed.')
         END IF
C
      ELSE IF (KEY .EQ. ICNVRT('GR')) THEN
         IF (OPEN) THEN
            CALL DAO_ALLOC( '_INTEGER', MAXSTR, IP1 )
            CALL DAO_ALLOC( '_REAL', MAXSTR, IP2 )
            CALL DAO_ALLOC( '_REAL', MAXSTR, IP3 )
            CALL DAO_ALLOC( '_REAL', MAXSTR, IP4 )
            CALL DAO_ALLOC( '_REAL', MAXSTR, IP5 )
            CALL DAO_ALLOC( '_INTEGER', MAXSTR, IP6 )
            CALL DAO_ALLOC( '_INTEGER', MAXSTR, IP7 )
            CALL DAO_ALLOC( '_INTEGER', MAXSTR, IP8 )
            CALL GROUP (PAR, MAXPAR, PSF, MAXPSF, MAXEXP,
     :                  %VAL(CNF_PVAL(IP1)),
     .           %VAL(CNF_PVAL(IP2)), %VAL(CNF_PVAL(IP3)),
     :           %VAL(CNF_PVAL(IP4)), %VAL(CNF_PVAL(IP5)),
     .           %VAL(CNF_PVAL(IP6)), %VAL(CNF_PVAL(IP7)),
     :           %VAL(CNF_PVAL(IP8)), MAXSTR, OPT(12),
     .           OPT(13))
            CALL DAO_DEALL( IP8 )
            CALL DAO_DEALL( IP7 )
            CALL DAO_DEALL( IP6 )
            CALL DAO_DEALL( IP5 )
            CALL DAO_DEALL( IP4 )
            CALL DAO_DEALL( IP3 )
            CALL DAO_DEALL( IP2 )
            CALL DAO_DEALL( IP1 )
         ELSE
            CALL STUPID ('   No picture file has been ATTACHed.')
         END IF
C
      ELSE IF (KEY .EQ. ICNVRT('PE')) THEN
         IF (OPEN) THEN
            MAXB = (MAXPSF-7)/2
            CALL DAO_ALLOC( '_REAL', MAXB*MAXB, IP1 )
            CALL DAOPK (PAR, MAXPAR, PSF, MAXPSF, MAXEXP,
     :                  %VAL(CNF_PVAL(IP1)),
     .           MAXB, OPT(11), OPT(12), OPT(19), OPT(20))
            CALL DAO_DEALL( IP1 )
         ELSE
            CALL STUPID ('   No picture file has been ATTACHed.')
         END IF
C
      ELSE IF (KEY .EQ. ICNVRT('NS')) THEN
         IF (OPEN) THEN
            CALL NSTAR (PAR, MAXPAR, PSF, MAXPSF, MAXEXP,
     :                  %VAL(CNF_PVAL(IPWK)),
     .           NCOL, NROW, OPT(11), OPT(12), OPT(19), OPT(20))
         ELSE
            CALL STUPID ('   No picture file has been ATTACHed.')
         END IF
C
      ELSE IF (KEY .EQ. ICNVRT('SU')) THEN
         IF (OPEN) THEN
            CALL SUBSTR (PAR, MAXPAR, PSF, MAXPSF, MAXEXP,
     .           %VAL(CNF_PVAL(IPWK)), NCOL, NROW, OPT(11))
         ELSE
            CALL STUPID ('   No picture file has been ATTACHed.')
         END IF
C
      ELSE IF (KEY .EQ. ICNVRT('AD')) THEN
         IF (OPEN) THEN
            CALL ADDSTR (PAR, MAXPAR, PSF, MAXPSF, MAXEXP,
     .           %VAL(CNF_PVAL(IPWK)), NCOL, NROW, OPT(11))
         ELSE
            CALL STUPID ('   No picture file has been ATTACHed.')
         END IF
C
      ELSE IF (KEY .EQ. ICNVRT('DU')) THEN
         IF (OPEN) THEN
            CALL DUMP (%VAL(CNF_PVAL(IPWK)), NCOL, NROW)
         ELSE
            CALL STUPID ('   No picture file has been ATTACHed.')
         END IF
C
      ELSE IF (KEY .EQ. ICNVRT('FU')) THEN
         IF (OPEN) THEN
            CALL FUDGE (FILE, %VAL(CNF_PVAL(IPWK)), NCOL)
         ELSE
            CALL STUPID ('   No picture file has been ATTACHed.')
         END IF
      ELSE
C
C Minor problems.
C
         CALL STUPID ('   Unrecognized command.')
      END IF
      GO TO 3000
      END!
C
C#######################################################################
C
      SUBROUTINE  HELP (CMD, NCMD, RCMD, NUMBER)
C
C=======================================================================
C
C This subroutine produces a simple listing on the terminal of all of
C the elements of the character vector CMD.  They are sorted into
C alphabetical order by the first two characters.
C
C             OFFICIAL DAO VERSION:  1991 April 18
C
C Argument
C
C CMD is a character array containing the names of the defined commands.
C
C=======================================================================
C
      IMPLICIT NONE
C
      INTEGER ICNVRT, I, NCMD
C
C NCMD is the total number of defined commands.
C
      CHARACTER*10 CMD(NCMD)
      REAL RCMD(NCMD)
      INTEGER NUMBER(NCMD)
C
C-----------------------------------------------------------------------
C
      WRITE (6,610)
  610 FORMAT (/' The commands currently recognized are:'/)
C
C Determine the numerical equivalent of the first two characters in each
C of the defined commands.
C
      DO 1010 I=1,NCMD
 1010 RCMD(I)=FLOAT(ICNVRT(CMD(I)(1:2)))
C
C Sort the commands into alphabetical order.
C
      CALL QUICK (RCMD, NCMD, NUMBER)
C
C Now type the command names out on the screen.
C
      WRITE (6,611) (CMD(NUMBER(I)), I=1,NCMD)
  611 FORMAT (1X, 5A14)
      WRITE (6,612)
  612 FORMAT (/' Any command may be abbreviated down to its first two',
     .     ' characters.')
C
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  SKY  (D, S, INDEX, MAX, HIBAD, SKYMN, SKYMED,
     .     SKYMOD, N)
C
C=======================================================================
C
C This subroutine estimates an average sky value for a picture by taking
C individual pixels scattered over the picture.  The brightness values
C are sorted, and the modal value is estimated using the MMM subroutine.
C
C               OFFICIAL DAO VERSION:  1991 April 18
C
C=======================================================================
C
      IMPLICIT NONE
      INTEGER MAX
C
C MAX    is the maximum number of sky pixels we can deal with,
C        given the limited amount of working space.
C
      REAL S(MAX), D(MAX)
      INTEGER INDEX(MAX)
C
      REAL HIBAD, SKYMN, SKYMED, SKYMOD, SKYSIG, SKYSKW
      INTEGER NCOL, NROW, ISTEP, LX, LY, NX, NY, IROW, I, N
      INTEGER ISTAT, IFIRST
      COMMON /SIZE/ NCOL, NROW
C
C-----------------------------------------------------------------------
C
C The spacing between pixels that will be included in the sample is
C estimated by the ratio of the total number of pixels in the picture to
C the maximum number of pixels that can be accomodated in the vector S.
C
      ISTEP = NCOL*NROW/MAX+1
C
C Go through the disk file reading a row at a time and extracting every
C ISTEP-th pixel.  If ISTEP is not equal to 1, make sure that the
C starting pixel for each row is staggered.
C
      LX = 1
      NX = NCOL
      NY = 1
      IFIRST = 0
      N = 0
      DO IROW=1,NROW
         LY = IROW
         CALL RDARAY ('DATA', LX, LY, NX, NY, MAX, D, ISTAT)
         IF (ISTAT .NE. 0) RETURN
         IFIRST = IFIRST + 1
         IF (IFIRST .GT. ISTEP) IFIRST = IFIRST - ISTEP
         I = IFIRST
 1010    IF (ABS(D(I)) .LE. HIBAD) THEN
            N = N+1
            S(N) = D(I)
            IF (N .EQ. MAX) GO TO 1100
            I = I + ISTEP
         ELSE
            I = I+1
         END IF
         IF (I .LE. NCOL) GO TO 1010
      END DO
C
C Sort these values, then estimate the mode.
C
 1100 CONTINUE
      CALL QUICK (S, N, INDEX)
C
C Correct the number of arguments in the call to MMM
C Nick Eaton, Durham University, 19 Feb 1992.
C
      CALL MMM (S, N, HIBAD, SKYMN, SKYMED, SKYMOD, SKYSIG,
     .     SKYSKW)
      WRITE (6,610) SKYMOD, SKYSIG
  610 FORMAT (/' Approximate sky value for this frame =', F9.1/
     .         ' Standard deviation of sky brightness =', F10.2/)
C
C Normal return.
C
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  APPEND
C
C=======================================================================
C
C A simple subroutine to append two DAOPHOT stellar data files,
C omitting the superfluous file header.
C
C=======================================================================
C
      IMPLICIT NONE
      CHARACTER*132 LINE
      CHARACTER*30 IFILE1, IFILE2, SWITCH, CASE*4
      REAL R1, R2, R3, R4, R5, R6, R7
      INTEGER ISTAT, K, I1, I2, I3
C
C-----------------------------------------------------------------------
C
      CALL TBLANK                                   ! Type a blank line
      IFILE1=' '
      CALL GETNAM ('First input file:', IFILE1)
      IF (IFILE1 .EQ. 'END OF FILE') THEN
         IFILE1 = ' '
         RETURN
      END IF
C
  950 CALL INFILE (1, IFILE1, ISTAT)
      IF ((IFILE1 .EQ. 'END OF FILE') .OR.
     .     (IFILE1 .EQ. 'GIVE UP')) THEN
         IFILE1 = ' '
         RETURN
      END IF
C
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//IFILE1)
         IFILE1 = 'GIVE UP'
         GO TO 950
      END IF
C
      IFILE2=' '
  960 CALL GETNAM ('Second input file:', IFILE2)
      IF ((IFILE2 .EQ. 'END OF FILE') .OR.
     .     (IFILE2 .EQ. 'GIVE UP')) THEN
         CALL CLFILE (1)
         RETURN
      END IF
      CALL INFILE (2, IFILE2, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//IFILE2)
         IFILE2 = 'GIVE UP'
         GO TO 960
      END IF
C
      IFILE1 = SWITCH(IFILE1, CASE('.cmb'))
  970 CALL GETNAM ('Output file:', IFILE1)
      IF ((IFILE1 .EQ. 'END OF FILE') .OR.
     .     (IFILE1 .EQ. 'GIVE UP')) THEN
         CALL CLFILE (1)
         CALL CLFILE (2)
         RETURN
      END IF
      CALL OUTFIL (3, IFILE1, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening output file '//IFILE1)
         IFILE1 = 'GIVE UP'
         GO TO 970
      END IF
C
C-----------------------------------------------------------------------
C
C Copy first file's header and data verbatim into the output file.
C
 2000 CALL RDCHAR (1, LINE, K, ISTAT)
      IF (ISTAT .GT. 0) GO TO 2900
      IF (ISTAT .LT. 0) GO TO 2000
      IF (K .LE. 0) LINE=' '
      K = MAX0(1,K)
      WRITE (3,310) LINE(1:K)
  310 FORMAT (A)
      GO TO 2000
C
 2900 CALL CLFILE (1)
C
C-----------------------------------------------------------------------
C
C Add to the output file the stellar data, but not the header, from the
C second input file.
C
C Correct the number of arguments in the call to RDHEAD
C Nick Eaton, Durham University, 19 Feb 1992.
C
      I1=-1
      CALL RDHEAD (2, I1, I2, I3, R1, R2, R3, R4, R5, R6, R7)
C
C RDHEAD will leave the pointer positioned at the top of the input
C file's stellar data whether there was a header there or not.  Now
C copy the remainder of the second input file verbatim into the output
C file.
C
 3010 CALL RDCHAR (2, LINE, K, ISTAT)
      IF (ISTAT .GT. 0) THEN
         CALL CLFILE (2)
         CALL CLFILE (3)
         RETURN
      END IF
      IF (ISTAT .LT. 0) GO TO 3010
      K=MAX0(1,K)
      WRITE (3,310) LINE(1:K)
      GO TO 3010
      END!
C
C#######################################################################
C
      SUBROUTINE  DAOSLT (ID, XC, YC, MAG, SKY, MAX)
C
C=======================================================================
C
C This is a simple subroutine which selects groups within a certain
C range of sizes from a group file, and puts them into a new group file.
C
C              OFFICIAL DAO VERSION: 1991 April 18
C
C=======================================================================
C
      IMPLICIT NONE

*  History:
*     17-Mar-1995 (GJP)
*     Replaced very negative, very large or very small numbers
*     with their PRM_PAR equivalents.

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

      INTEGER MAX
C
C MAX is the largest number of stars that can be held in working space.
C
      CHARACTER*30 COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL, EXTEND
      CHARACTER CASE*3
      REAL XC(MAX), YC(MAX), MAG(MAX), SKY(MAX)
      REAL SIZE(2)
      INTEGER ID(MAX)
C
      REAL ALOG10
C
      CHARACTER*4 PLSTR, PLGRP
      REAL LOBAD, HIBAD, THRESH, AP1, PHPADU, RONOIS, DUM, RADIUS
      INTEGER ISTAT, NL, NCOL, NROW, MINGRP, MAXGRP
      INTEGER NGRP, NTOT, I, NSTAR, LENGRP
C
      COMMON /FILNAM/ COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Setup.
C
C Ascertain the name of the input group file and open it.
C
      CALL TBLANK                                   ! Type a blank line
  950 CALL GETNAM ('Input group file:', GRPFIL)
      IF ((GRPFIL .EQ. 'END OF FILE') .OR.
     .     (GRPFIL .EQ. 'GIVE UP')) THEN
         GRPFIL = ' '
         RETURN
      END IF
C
      CALL INFILE (2, GRPFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//GRPFIL)
         GRPFIL = 'GIVE UP'
         GO TO 950
      END IF
C
      CALL RDHEAD (2, NL, NCOL, NROW, LOBAD, HIBAD, THRESH, AP1,
     .     PHPADU, RONOIS, DUM)
      IF (NL .NE. 3) THEN
         CALL STUPID ('Not a group file.')
         CALL CLFILE (2)
         GRPFIL = 'GIVE UP'
         GO TO 950
      END IF
C
C Get the desired range of group sizes.
C
      CALL GETDAT ('Minimum, maximum group size:', SIZE, 2)
      IF (SIZE(1) .LE. VAL__MINR) GO TO 9010           ! CTRL-Z was entered
      MINGRP=NINT(SIZE(1))
      MAXGRP=NINT(SIZE(2))
C
C Get the name of the output group file and open it.
C
      MAGFIL=GRPFIL
  960 CALL GETNAM ('Output group file:', MAGFIL)
      IF ((MAGFIL .EQ. 'END OF FILE') .OR.
     .     (MAGFIL .EQ. 'GIVE UP')) THEN
         CALL CLFILE (2)
         MAGFIL = ' '
         RETURN
      END IF
C
      MAGFIL = EXTEND(MAGFIL, CASE('grp'))
      CALL OUTFIL (3, MAGFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening output file '//MAGFIL)
         MAGFIL = 'GIVE UP'
         GO TO 960
      END IF
      CALL WRHEAD (3, 3, NCOL, NROW, 6, LOBAD, HIBAD, THRESH, AP1,
     .     PHPADU, RONOIS, RADIUS)
      NGRP=0
      NTOT=0
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Actually do it.
C
C Read in the next group of stars.
C
 2000 I=0                                       ! Begin loop over groups
 2010 I=I+1                                     ! Begin loop over stars
C
      IF (I .GT. MAX) THEN                 ! Too many stars in group
         CALL STUPID ('/Group too large.')
         WRITE (6,6) MAX
    6    FORMAT (I10, ' is the largest number of stars I can',
     .        ' possibly consider.  I''m throwing it out.'/,
     .        'You could increase the MS parameter and try again.'/)
C
C Look for the next blank line.
C
 2015    CALL RDSTAR (2, 3, ID(1), XC(1), YC(1), MAG(1), SKY(1))
         IF (ID(1) .GT. 0) GO TO 2015
         IF (ID(1) .EQ. 0) GO TO 2000
         GO TO 9000
      END IF
C
      CALL RDSTAR (2, 3, ID(I), XC(I), YC(I), MAG(I), SKY(I))
C
      IF (ID(I) .LT. 0) GO TO 2020         ! End-of-file was encountered
      IF (ID(I) .EQ. 0) GO TO 2030         ! Blank line was encountered
      GO TO 2010
C
C Either a blank line or an EOF has been encountered.  If there
C have been some stars read in since the last blank line, we
C want to write out the group, if appropriate.  Otherwise,
C if EOF was encountered, then return; if a blank line, then keep
C trying to start reading a new group.
C
 2020 IF (I .EQ. 1) GO TO 9000        ! EOF and no stars in group
 2030 IF (I .EQ. 1) GO TO 2000        ! Blank line and no stars in group
      NSTAR=I-1
C
C NSTAR is the number of stars in the current group.  If this is outside
C the range of group sizes being selected, start reading the next group.
C Otherwise, write the group into the output file and increment the
C accumulators before reading the next group.
C
      IF ((NSTAR .LT. MINGRP) .OR. (NSTAR .GT. MAXGRP)) GO TO 2000
      NGRP=NGRP+1
      NTOT=NTOT+NSTAR
      DO 2040 I=1,NSTAR
 2040 WRITE (3,320) ID(I), XC(I), YC(I), MAG(I), SKY(I)
  320 FORMAT (1X, I5, 4F9.3)
      WRITE (3,320)                                 ! Write a blank line
      GO TO 2000
C
C-----------------------------------------------------------------------
C
C Normal return.
C
C Type out the number of stars and the number of groups NEATLY.
C
 9000 PLSTR='s in'
      IF (NTOT .EQ. 1) PLSTR=' in '
      PLGRP='s.  '
      IF (NGRP .EQ. 1) PLGRP='.   '
      LENGRP=INT(ALOG10(NGRP+0.5))+2
      IF (NTOT .EQ. 1)LENGRP=LENGRP-1
      WRITE (6,690) NTOT, PLSTR, NGRP, PLGRP
  690 FORMAT (/I6, ' star', A4, I5, ' group', A4/)
      CALL CLFILE (3)
 9010 CALL CLFILE (2)
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  DUMP  (F, NCOL, NROW)
C
C=======================================================================
C
C A trivial subroutine to type the brightness values in a small
C subarray of the picture onto the terminal.
C
C            OFFICIAL DAO VERSION:  1991 April 18
C
C=======================================================================
C
      IMPLICIT NONE

*  History:
*     17-Mar-1995 (GJP)
*     Replaced very negative, very large or very small numbers
*     with their PRM_PAR equivalents.

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

C
      INTEGER NSQUARE, NCOL, NROW
      PARAMETER  (NSQUARE=21)
C
      REAL F(NCOL,NROW)
      REAL COORDS(2), D(NSQUARE*NSQUARE)
C
      REAL AMIN1, AMAX1
      INTEGER NINT
C

      REAL SIZE
      INTEGER NBOX, NHALF, LX, LY, ISTAT
      INTEGER I, J, N, NX, NY
C
C Parameter
C
C NSQUARE is the side of the largest square subarray that can be
C         comfortably fit on the terminal screen.  NSQUARE = 21 is
C         for 24-line terminals, to accomodate the array, a two-line
C         header across the top, and and a query at the bottom.  In
C         fact, if the user specifies SIZE = 21, then one of the header
C         lines at the top will be lost.
C         (Terminal must have 132-column capability to prevent
C         wraparound.)
C
C
C-----------------------------------------------------------------------
C
      CALL TBLANK                                   ! Type a blank line
      CALL GETDAT ('Box size:', SIZE, 1)
      IF (SIZE .LE. VAL__MINR) RETURN                  ! CTRL-Z was entered
      NBOX=MAX0(1, MIN0(NINT(SIZE), NSQUARE))
      NHALF=(NBOX-1)/2
C
 1000 CALL GETDAT ('Coordinates of central pixel:', COORDS, 2)
C
C Return to calling program upon entry of CTRL-Z or invalid coordinates.
C
      IF ((COORDS(1) .LE. 0.5) .OR. (COORDS(2) .LE. 0.5)) RETURN
      IF ((NINT(COORDS(1)) .GT. NCOL) .OR.
     .     (NINT(COORDS(2)) .GT. NROW)) RETURN
C
      LX = NINT(COORDS(1))-NHALF
      LY = NINT(COORDS(2))-NHALF
      NX = NINT(COORDS(1))+NHALF - LX + 1
      NY = NINT(COORDS(2))+NHALF - LY + 1
      CALL RDARAY ('DATA', LX, LY, NX, NY, NCOL, F, ISTAT)
C
C LX and LY are the lower limits of the box in X and Y; NX and NY are
C the number of pixels in the box in X and Y.  They will have been
C modified by RDARAY if the box would have extended outside the
C picture.
C
      WRITE (6,609) (I, I=LX,LX+NX-1)
  609 FORMAT (/7X, 21I6)
      WRITE (6,610) ('------', I=1,NX)
  610 FORMAT (6X, '+', 21A6)
C
      N=0
      DO 1020 J=NY,1,-1
      WRITE (6,611) LY+J-1,
     .     (NINT(AMAX1(-99999.,AMIN1(99999.,F(I,J)))), I=1,NX)
  611 FORMAT (1X, I4, ' |', 21I6)
      DO 1010 I=1,NX
      N=N+1
 1010 D(N)=F(I,J)
 1020 CONTINUE
C
      CALL QUICK (D, N, F)
C
      IF (NBOX .LT. NSQUARE) CALL TBLANK
      WRITE (6,612) NINT(AMAX1(-99999.,AMIN1(99999.,D(1)))),
     .     NINT(AMAX1(-99999.,AMIN1(99999.,
     .     0.5*D((N+1)/2)+0.5*D((N/2)+1)))),
     .     NINT(AMAX1(-99999.,AMIN1(99999.,D(N))))
  612 FORMAT(26X, 'Minimum, median, maximum: ', 3I7)
C
      GO TO 1000
C
      END!
C
C#######################################################################
C
      SUBROUTINE  OFFSET
C
C=======================================================================
C
C A simple routine to read in an arbitrary DAOPHOT stellar data file,
C shift the stars' x,y coordinates by a constant amount, and write
C out an otherwise identical data file.
C
C            OFFICIAL DAO VERSION:  1991 April 18
C
C=======================================================================
C
      IMPLICIT NONE

*  History:
*     17-Mar-1995 (GJP)
*     Replaced very negative, very large or very small numbers
*     with their PRM_PAR equivalents.

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

      CHARACTER*133 LINE1, LINE2
      CHARACTER*30 FILE, SWITCH
      CHARACTER CASE*4
      REAL DELTA(4)
      REAL LOBAD, COLMAX, ROWMAX, HIBAD, THRESH, AP1, PHPADU
      REAL READNS, FRAD, X, Y, AMAG
      INTEGER I, ISTAT, IDDEL, NL, NCOL, NROW, ITEMS, NLINE1
      INTEGER NLINE2, ID
      LOGICAL WROTE
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Get ready.
C
      CALL TBLANK                                   ! Type a blank line
C
C Get input file name.
C
      FILE=' '
  950 CALL GETNAM ('Input file name:', FILE)
      IF ((FILE .EQ. 'END OF FILE') .OR. (FILE .EQ. 'GIVE UP')) RETURN
      CALL INFILE (2, FILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//FILE)
         FILE = 'GIVE UP'
         GO TO 950
      END IF
C
C Get offsets.
C
      DO I=1,4
         DELTA(I)=0.
      END DO
      CALL GETDAT ('Additive offsets ID, DX, DY, DMAG:', DELTA, 4)
      IF (DELTA(1) .LE. VAL__MINR) THEN
         CALL CLFILE (1)
         RETURN
      END IF
      IDDEL = NINT(DELTA(1))
C
C Get output file name.
C
      FILE = SWITCH(FILE, CASE('.off'))
  960 CALL GETNAM ('Output file name:', FILE)
      IF ((FILE .EQ. 'END OF FILE') .OR. (FILE .EQ. 'GIVE UP')) THEN
         CALL CLFILE (2)
         RETURN
      END IF
      CALL OUTFIL (3, FILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening output file '//FILE)
         FILE = 'GIVE UP'
         GO TO 960
      END IF
C
C Copy input file's header into output file.
C
      COLMAX=VAL__MAXR
      ROWMAX=VAL__MAXR
      NL=-1
      CALL RDHEAD (2, NL, NCOL, NROW, LOBAD, HIBAD, THRESH, AP1,
     .     PHPADU, READNS, FRAD)
C
      IF (NL .LE. 0) GO TO 2000                     ! No header in input
C
      ITEMS=6
      IF (FRAD .GT. 0.001) ITEMS=7
      CALL WRHEAD (3, NL, NCOL, NROW, ITEMS, LOBAD, HIBAD, THRESH,
     .     AP1, PHPADU, READNS, FRAD)
      COLMAX=FLOAT(NCOL)+0.5
      ROWMAX=FLOAT(NROW)+0.5
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Copy the data file, line by line, altering X and Y as we go.
C
 2000 CALL RDCHAR (2, LINE1, NLINE1, ISTAT)
      IF (ISTAT .GT. 0) GO TO 9000
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error reading data from input file.')
         WRITE (6,*) LINE1(1:NLINE1)
         CALL TBLANK
         RETURN
      END IF
      READ (LINE1(2:33), 220, IOSTAT=ISTAT) ID, X, Y, AMAG
  220 FORMAT (I5, 3F9.3)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error reading data from input file.')
         WRITE (6,*) LINE1(1:NLINE1)
         CALL TBLANK
         RETURN
      END IF
      IF (NLINE1 .LE. 1) THEN
C
C A blank line was encountered.  The toggle, WROTE, prevents more than
C one blank output line in a row from being produced.
C
         IF (WROTE) WRITE (3,*) ' '
         WROTE=.FALSE.
         GO TO 2000
      END IF
C
C Normal continuation.
C
      IF (NL .EQ. 2) CALL RDCHAR (2, LINE2, NLINE2, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error reading data from input file.')
         WRITE (6,*) LINE1(2:NLINE2)
         CALL TBLANK
         RETURN
      END IF
      ID=ID+IDDEL
      X=X+DELTA(2)
      Y=Y+DELTA(3)
      AMAG=AMAG+DELTA(4)
C
C Any star which winds up outside the picture after offsetting will be
C discarded.  (This implicitly assumes that the user will only be
C offsetting the coordinates in order to match them with another frame
C taken with the same device.  Otherwise the scale and possibly the
C orientation would be different, and a simple offset would not be a
C good enough transformation.)
C
      IF ((X .LT. 0.5) .OR. (X .GT. COLMAX) .OR. (Y .LT. 0.5) .OR.
     .     (Y .GT. ROWMAX)) GO TO 2000
C
      WRITE (LINE1(2:33),220) ID, X, Y, AMAG
      WRITE (3,301) LINE1(1:NLINE1)
  301 FORMAT (A)
      IF (NL .EQ. 2) WRITE (3,301) LINE2(1:NLINE2)
      WROTE=.TRUE.
      GO TO 2000
C
C-----------------------------------------------------------------------
C
C Normal return.
C
 9000 CALL CLFILE (3)
      CALL CLFILE (2)
      RETURN
      END!
