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
*  Suns.  It works in conjunction with a shell script, which invokes
*  the ASTROM program with four simulated command-line arguments:
*
*     1   terminal identifier
*     2   input file; data
*     3   output file; full report
*     4   output file; synopsis + errors
*
*  Called:  IARGC, GETARG, tpt_OPW, tpt_OPR, tpt_ASTRML
*
*  P T Wallace   Starlink   28 June 1994
*
*  Norman Gray, 2001-06-08: I added two further arguments, when I added
*    further arguments to astrml().  The fifth argument is a filename
*    prefix; if present, then this will be used to generate a filename
*    for a FITS-WCS file.  The sixth argument is a filename; if present, 
*    and not the same as argument one, it specifies a file
*    which will receive essentially the same information as the synopsis
*    file, but more easily parseable by machine; if not present, no such
*    information is generated.
*    
*-

      IMPLICIT NONE

      INTEGER NFILE
      PARAMETER (NFILE=100)
      CHARACTER*(NFILE) TERM,INP,REP,SYN,FILE,LOGFIL,FITSFN
      INTEGER LUINP,LUREP,LUSYN,LULOG,LU,J,N

      INTEGER IARGC



*  Verify there are the right number of arguments
*      IF (IARGC().NE.4) GO TO 9010
      IF (IARGC().LT.4.OR.IARGC().GT.6) GO TO 9010

*  Pick them up
      CALL GETARG(1,TERM)
      CALL GETARG(2,INP)
      CALL GETARG(3,REP)
      CALL GETARG(4,SYN)
      IF (IARGC().GT.4) THEN
         CALL GETARG(5,FITSFN)
      ELSE
         FITSFN=''
      ENDIF
      IF (IARGC().GT.5) THEN
         CALL GETARG(6,LOGFIL)
      ELSE
         LOGFIL=TERM
      ENDIF

*  Open the files
      IF (INP.EQ.TERM) THEN
         LUINP=5
      ELSE
         LUINP=11
         LU=LUINP
         FILE=INP
         CALL OPR(LU,FILE,J)
         IF (J.NE.0) GO TO 9020
      END IF

      IF (REP.EQ.TERM) THEN
         LUREP=6
      ELSE
         LUREP=13
         LU=LUREP
         FILE=REP
         CALL OPW(LU,FILE,J)
         IF (J.NE.0) GO TO 9020
      END IF

      IF (SYN.EQ.TERM) THEN
         LUSYN=6
      ELSE
         LUSYN=16
         LU=LUSYN
         FILE=SYN
         CALL OPW(LU,FILE,J)
         IF (J.NE.0) GO TO 9020
      END IF
      
      IF (LOGFIL.EQ.TERM) THEN
         LULOG=0
      ELSE
         LULOG=19
         LU=LULOG
         FILE=LOGFIL
         CALL OPW(LU,FILE,J)
         IF (J.NE.0) GO TO 9020
      ENDIF

*   Don't do anything with the FITS filename template -- leave that to astrml

*  Call the ASTROM mainline program and exit when finished
      CALL ASTRML(LUINP,LUREP,LUSYN,LULOG,FITSFN)
      GO TO 9999

*  Errors
 9010 CONTINUE
      WRITE (*,'(''Please run ASTROM from the correct script!'')')
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
