      PROGRAM COCO
*+
*  - - - - -
*   C O C O
*  - - - - -
*
*  !!!  Version for Sun/Solaris, Sun/SunOS, Alpha/OSF-1,  !!!
*  !!!  DECstation/Ultrix and Convex/ConvexOS             !!!
*
*  This module is a front-end, the function of which is to open all
*  the required files and supply I/O unit numbers to the mainline
*  routine COCOML.  See the source of COCOML for details of the COCO
*  celestial coordinate conversion utility.
*
*  This version is for the Unix systems on Starlink DECstations and
*  Suns.  It works in conjunction with a shell script, which invokes
*  the COCO program with seven simulated command-line arguments:
*
*     1   terminal identifier
*     2   output file; raw output for redirection
*     3   output file; echo of input
*     4   output file; report (including error warnings)
*     5   input file; commands and data (primary)
*     6   output file; prompts
*     7   output file; error warnings
*
*  Called:  IARGC, GETARG, OPW, OPR, COCOML
*
*  P T Wallace   Starlink   27 June 1994
*-

      IMPLICIT NONE

      INTEGER NFILE
      PARAMETER (NFILE=100)
      CHARACTER*(NFILE) TERM,OUT,ECH,REP,INP,PRO,ERR,FILE
      INTEGER LUOUT,LUECH,LUREP,LUINP2,LUINP1,LUPRO,LUERR,LU,J,N

      INTEGER IARGC




*  Verify there are the right number of arguments
      IF (IARGC().NE.7) GO TO 9010

*  Pick them up
      CALL GETARG(1,TERM)
      CALL GETARG(2,OUT)
      CALL GETARG(3,ECH)
      CALL GETARG(4,REP)
      CALL GETARG(5,INP)
      CALL GETARG(6,PRO)
      CALL GETARG(7,ERR)

*  Open the files
      IF (OUT.EQ.TERM) THEN
         LUOUT=6
      ELSE
         LUOUT=11
      END IF
      LU=LUOUT
      FILE=OUT
      CALL OPW(LU,FILE,J)
      IF (J.NE.0) GO TO 9020

      IF (ECH.EQ.TERM) THEN
         LUECH=6
      ELSEIF (ECH.EQ.OUT) THEN
         LUECH=11
      ELSE
         LUECH=12
      END IF
      LU=LUECH
      FILE=ECH
      CALL OPW(LU,FILE,J)
      IF (J.NE.0) GO TO 9020

      IF (REP.EQ.TERM) THEN
         LUREP=6
      ELSEIF (REP.EQ.OUT) THEN
         LUREP=11
      ELSEIF (REP.EQ.ECH) THEN
         LUECH=12
      ELSE
         LUREP=13
      END IF
      LU=LUREP
      FILE=REP
      CALL OPW(LU,FILE,J)
      IF (J.NE.0) GO TO 9020

      LUINP2=14

      IF (INP.EQ.TERM) THEN
         LUINP1=5
      ELSE
         LUINP1=15
      END IF
      LU=LUINP1
      FILE=INP
      CALL OPR(LU,FILE,J)
      IF (J.NE.0) GO TO 9020

      IF (PRO.EQ.TERM) THEN
         LUPRO=6
      ELSEIF (PRO.EQ.OUT) THEN
         LUPRO=11
      ELSEIF (PRO.EQ.ECH) THEN
         LUPRO=12
      ELSEIF (PRO.EQ.REP) THEN
         LUPRO=13
      ELSE
         LUPRO=16
      END IF
      LU=LUPRO
      FILE=PRO
      CALL OPW(LU,FILE,J)
      IF (J.NE.0) GO TO 9020

      IF (ERR.EQ.TERM) THEN
         LUERR=6
      ELSEIF (ERR.EQ.OUT) THEN
         LUERR=11
      ELSEIF (ERR.EQ.ECH) THEN
         LUERR=12
      ELSEIF (ERR.EQ.REP) THEN
         LUERR=13
      ELSEIF (ERR.EQ.PRO) THEN
         LUERR=16
      ELSE
         LUERR=17
      END IF
      LU=LUERR
      FILE=ERR
      CALL OPW(LU,FILE,J)
      IF (J.NE.0) GO TO 9020

*  Call the COCO mainline program and exit when finished
      CALL COCOML(LUOUT,LUECH,LUREP,LUINP2,LUINP1,LUPRO,LUERR)
      GO TO 9999

*  Errors
 9010 CONTINUE
      WRITE (*,'(''Please run COCO from the correct script!'')')
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
