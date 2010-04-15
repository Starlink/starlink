      PROGRAM ASTROM
*+
*  - - - - - - -
*   A S T R O M
*  - - - - - - -
*
*  !!! Version for DECstation, SUN SPARCstation etc !!!
*
*  This module is a front-end, the function of which is to open all
*  the required files and supply I/O unit numbers to the mainline
*  routine ASTRML.  See the source of ASTRML for details of the ASTROM
*  plate reduction utility.
*
*  This version is for the Unix systems on Starlink DECstations and
*  Suns.  The program expects a list of arguments of the form
*  `keyword=value'.  It interprets these keywords as filenames, filename
*  prefixes or options as described below.
*
*  The recognised keywords are:
*
*    input     data
*    report    full report
*    summary   synopsis + errors
*    log       easily-parseable log file, reporting status and errors
*    fits      prefix for generated FITS files
*    wcsstyle  style of FITS WCS headers generated
*
*  If the 'input' keyword is omitted, it defaults to 'astrom.dat', and if
*  the 'report' keyword is omitted, it defaults to 'astrom.lis'.  Either of
*  the input or the report filename may be given as '-', indicating the
*  standard input or standard output respectively.  You may give a
*  filename without the keyword: the first such name is assigned to
*  'input', and the second to 'report'.
*
*  Called:  IARGC, GETARG, tpt_OPW, tpt_OPR, tpt_ASTRML
*
*  P T Wallace   Starlink   28 June 1994
*
*  Norman Gray, Starlink, 2001--2003: Interface expanded to match extra
*  arguments to astrml().
*
*-

      IMPLICIT NONE

      INCLUDE 'params.inc'

*      INTEGER NFILE
*      PARAMETER (NFILE=100)
*      CHARACTER*(NFILE) TERM,INP,REP,SYN,FILE,LOGFIL,FITSFN
      CHARACTER*(NFILE) FILE,FITSFN,FITSWCSSTYLE
      CHARACTER*(NFILE) ARG,ARGNAME,ARGVALUE
      INTEGER LUINP,LUREP,LUSYN,LULOG,LU,I,J,N
      INTEGER ARGN
      INTEGER ACTION
      LOGICAL ANONARG

      INTEGER IARGC

*   Defaults
      LUINP=0                   ! sentinel value
      LUREP=0                   ! sentinel value
      LUSYN=UNITOUT             ! standard output
      LULOG=0
      FITSFN=' '                ! suppresses generation of FITS files
      FITSWCSSTYLE=DEFWCSSTYLE
      ACTION=0                  ! Call astrml

      ARGN=1
      DO WHILE (ARGN.LE.IARGC())
         CALL GETARG(ARGN,ARG)
         I=1
         DO WHILE (ARG(I:I).EQ.' ')
            I=I+1
            IF (I.EQ.NFILE) GO TO 9015
         ENDDO

         J=I
         IF (ARG(1:1).EQ.'-') THEN
            ARGNAME=ARG
            ANONARG=.FALSE.
         ELSE
            DO WHILE (ARG(J:J).NE.'=' .AND. ARG(J:J).NE.' ')
               J=J+1
               IF (J.EQ.NFILE) GO TO 9015
            ENDDO
            IF (ARG(J:J).EQ.' ') THEN
               ANONARG=.TRUE.
               ARGVALUE=ARG(I:J-1)
            ELSE
               ANONARG=.FALSE.
               ARGNAME=ARG(I:J-1)
               J=J+1
               I=J
               DO WHILE (ARG(J:J).NE.' ')
                  J=J+1
                  IF (J.GE.NFILE) GO TO 9015
               ENDDO
               ARGVALUE=ARG(I:J-1)
            ENDIF
         ENDIF

         IF ((ANONARG .AND. (LUINP.EQ.0))
     :        .OR. ARGNAME(1:5).EQ."input") THEN
*         Have we specified stdin?
            IF (ARGVALUE(1:2).EQ.'- ') THEN
               LUINP=UNITIN
*               write(*,2000)'input',lu,'stdin'
            ELSE
               LUINP=11
               LU=LUINP
               FILE=ARGVALUE
               CALL OPR(LU,FILE,J)
*               write(*,2000)'input',lu,file
               IF (J.NE.0) GO TO 9020
            ENDIF
         ELSE IF ((ANONARG .AND. (LUREP.EQ.0))
     :           .OR. ARGNAME(1:6).EQ."report") THEN
*         Have we specified stdout?
            IF (ARGVALUE(1:2).EQ.'- ') THEN
               LUREP=UNITOUT
*               write(*,2000)'report',lu,'stdout'
            ELSE
               LUREP=13
               LU=LUREP
               FILE=ARGVALUE
               CALL OPW(LU,FILE,J)
*               write(*,2000)'report',lu,file
               IF (J.NE.0) GO TO 9020
            ENDIF
         ELSE IF (ARGNAME(1:7).EQ."summary") THEN
            LUSYN=16
            LU=LUSYN
            FILE=ARGVALUE
            CALL OPW(LU,FILE,J)
*            write(*,2000)'summary',lu,file
            IF (J.NE.0) GO TO 9020
         ELSE IF (ARGNAME(1:3).EQ."log") THEN
            LULOG=19
            LU=LULOG
            FILE=ARGVALUE
            CALL OPW(LU,FILE,J)
*            write(*,2000)'log',lu,file
            IF (J.NE.0) GO TO 9020
         ELSE IF (ARGNAME(1:4).EQ."fits") THEN
            FITSFN=ARGVALUE
         ELSE IF (ARGNAME(1:8).EQ."wcsstyle") THEN
            FITSWCSSTYLE=ARGVALUE
         ELSE IF (ARGNAME(1:6).EQ."--help") THEN
            ACTION=1
         ELSE IF (ARGNAME(1:9).EQ."--version") THEN
            ACTION=2
         ELSE IF (ARGNAME(1:2).EQ."-V") THEN
            ACTION=2
         ELSE
            GO TO 9016
         ENDIF
         ARGN=ARGN+1
      ENDDO

      IF (LUINP.EQ.0) THEN
*      input= specifier wasn't given
         LUINP=11
         LU=LUINP
         FILE='astrom.dat'
         CALL OPR(LU,FILE,J)
*         write(*,2000) 'input',lu,file
         IF (J.NE.0) GO TO 9020
      ENDIF
      IF (LUREP.EQ.0) THEN
*      report= specifier wasn't given
         LUREP=13
         LU=LUREP
         FILE='astrom.lis'
         CALL OPW(LU,FILE,J)
*         write(*,2000) 'report',lu,file
         IF (J.NE.0) GO TO 9020
      ENDIF

* 2000 format (1x,a,': unit ',i2,' connected to ',a)

*   Don't do anything with the FITS filename template -- leave that to astrml

      IF (ACTION.EQ.0) THEN
*      Call the ASTROM mainline program and exit when finished
         CALL ASTRML(LUINP,LUREP,LUSYN,LULOG,FITSFN,FITSWCSSTYLE)

      ELSE IF (ACTION.EQ.1) THEN
*      Show help
         WRITE(*,
     :    '("Usage: astrom [options] key=value ...")')
         WRITE(*,
     :   '("  input=..    ASTROM input file [default=astrom.dat]")')
         WRITE(*,
     :   '("  report=..   Report output file [default=astrom.lis]")')
         WRITE(*,
     :   '("  summary=..  Summary file [default:terminal]")')
         WRITE(*,
     :   '("  log=..      Log file [default:discarded")')
         WRITE(*,
     :   '("  fits=..     FITS file prefix [default:none generated]")')
         WRITE(*,
     :   '("  wcsstyle=.. WCS style for FITS [default varies]")')
         WRITE(*,
     :   '("  --help      Show this help")')
         WRITE(*,
     :   '("  --version   Show version")')
         WRITE(*,
     :   '("  -V          ditto")')

      ELSE IF (ACTION.EQ.2) THEN
         WRITE(*,'("ASTROM version ",a)') ASTROMVERSION

      ELSE
         WRITE(*,'("ASTROM BUG -- impossible action (",i3,")!")') action
      ENDIF

      GO TO 9999

*  Errors
 9010 CONTINUE
      WRITE (*,'(''Please run ASTROM from the correct script!'')')
      GO TO 9999

 9015 CONTINUE
      I=1
      DO WHILE (I.LE.NFILE.AND.ARG(I:I).NE.' ')
         I=I+1
      ENDDO
      WRITE (*,'(''Unparseable argument '',A)') ARG(1:I)
      GO TO 9999

 9016 CONTINUE
      I=1
      DO WHILE (I.LE.NFILE.AND.ARGNAME(I:I).NE.' ')
         I=I+1
      ENDDO
      WRITE (*,'(''Unrecognised parameter '',A)') ARGNAME(1:I)
      GO TO 9999

 9020 CONTINUE
      N=NFILE
      DO WHILE (FILE(N:N).EQ.' '.AND.N.GT.1)
         N=N-1
      END DO
      WRITE (*,'(''Can''''t open unit'',I3,'', file '',A,'' !'')')
     :                                                       LU,FILE(:N)

*  Exit
 9999 CONTINUE

      END


