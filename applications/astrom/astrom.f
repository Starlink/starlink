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
*  prefixes or options as described below.  The program works in
*  conjunction with a shell script, which slightly enhances the
*  interface by defaulting keywords.
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

      INTEGER IARGC

*   Defaults
      LUINP=UNITIN              ! standard input
      LUREP=UNITOUT             ! standard output
      LUSYN=UNITOUT
      LULOG=0
      FITSFN=' '                ! suppresses generation of FITS files
      FITSWCSSTYLE=DEFWCSSTYLE

      ARGN=1
      DO WHILE (ARGN.LE.IARGC())
         CALL GETARG(ARGN,ARG)
         I=1
         DO WHILE (ARG(I:I).EQ.' ')
            I=I+1
            IF (I.EQ.NFILE) GO TO 9015
         ENDDO
         J=I
         DO WHILE (ARG(J:J).NE.'=')
            J=J+1
            IF (J.EQ.NFILE) GO TO 9015
         ENDDO
         ARGNAME=ARG(I:J-1)
         J=J+1
         I=J
         DO WHILE (ARG(J:J).NE.' ')
            J=J+1
            IF (J.GE.NFILE) GO TO 9015
         ENDDO
         ARGVALUE=ARG(I:J-1)
         IF (ARGNAME(1:5).EQ."input") THEN
            LUINP=11
            LU=LUINP
            FILE=ARGVALUE
            CALL OPR(LU,FILE,J)
            IF (J.NE.0) GO TO 9020
         ELSE IF (ARGNAME(1:6).EQ."report") THEN
            LUREP=13
            LU=LUREP
            FILE=ARGVALUE
            CALL OPW(LU,FILE,J)
            IF (J.NE.0) GO TO 9020
         ELSE IF (ARGNAME(1:7).EQ."summary") THEN
            LUSYN=16
            LU=LUSYN
            FILE=ARGVALUE
            CALL OPW(LU,FILE,J)
            IF (J.NE.0) GO TO 9020
         ELSE IF (ARGNAME(1:3).EQ."log") THEN
            LULOG=19
            LU=LULOG
            FILE=ARGVALUE
            CALL OPW(LU,FILE,J)
            IF (J.NE.0) GO TO 9020
         ELSE IF (ARGNAME(1:4).EQ."fits") THEN
            FITSFN=ARGVALUE
         ELSE IF (ARGNAME(1:8).EQ."wcsstyle") THEN
            FITSWCSSTYLE=ARGVALUE
         ELSE
            GO TO 9016
         ENDIF
         ARGN=ARGN+1
      ENDDO


*   Don't do anything with the FITS filename template -- leave that to astrml

*  Call the ASTROM mainline program and exit when finished
      CALL ASTRML(LUINP,LUREP,LUSYN,LULOG,FITSFN,FITSWCSSTYLE)
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

      
