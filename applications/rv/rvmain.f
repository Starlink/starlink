      PROGRAM RV
*+
*  - - -
*   R V
*  - - -
*
*  !!! Version for DECstation, SUN SPARCstation etc !!!
*
*  This module is a front-end, the function of which is to open all
*  the required files and supply I/O unit numbers to the mainline
*  routine RVML.  See the source of RVML for details of the RV
*  radial velocity utility.
*
*  This version is for the Unix systems on Starlink DECstations and
*  Suns.  It works in conjunction with a shell script, which invokes
*  the RV program with five simulated command-line arguments:
*
*     1   terminal identifier
*     2   output file: report (including error warnings)
*     3   input file: commands/data
*     4   output file: prompts/help/errors
*     5   output file; echo of input
*
*  Called:  IARGC, GETARG, tpt_OPW, tpt_OPR, tpt_RVML
*
*  P T Wallace   Starlink   28 June 1994
*-

      IMPLICIT NONE

      INTEGER NFILE
      PARAMETER (NFILE=100)
      CHARACTER*(NFILE) TERM,REP,INP,PRO,ECH,FILE
      INTEGER LUREP,LUINP,LUPRO,LUECH,LU,J,N

      INTEGER IARGC




*  Verify there are the right number of arguments
      IF (IARGC().NE.5) GO TO 9010

*  Pick them up
      CALL GETARG(1,TERM)
      CALL GETARG(2,REP)
      CALL GETARG(3,INP)
      CALL GETARG(4,PRO)
      CALL GETARG(5,ECH)

*  Open the files
      IF (REP.EQ.TERM) THEN
         LUREP=6
      ELSE
         LUREP=13
         LU=LUREP
         FILE=REP
         CALL OPW(LU,FILE,J)
         IF (J.NE.0) GO TO 9020
      END IF

      IF (INP.EQ.TERM) THEN
         LUINP=5
      ELSE
         LUINP=15
         LU=LUINP
         FILE=INP
         CALL OPR(LU,FILE,J)
         IF (J.NE.0) GO TO 9020
      END IF

      IF (PRO.EQ.TERM) THEN
         LUPRO=6
      ELSE
         LUPRO=16
         LU=LUPRO
         FILE=PRO
         CALL OPW(LU,FILE,J)
         IF (J.NE.0) GO TO 9020
      END IF

      IF (ECH.EQ.TERM) THEN
         LUECH=6
      ELSE
         LUECH=17
         LU=LUECH
         FILE=ECH
         CALL OPW(LU,FILE,J)
         IF (J.NE.0) GO TO 9020
      END IF

*  Call the RV mainline program and exit when finished
      CALL RVML(LUREP,LUINP,LUPRO,LUECH)
      GO TO 9999

*  Errors
 9010 CONTINUE
      WRITE (*,'(''Please run RV from the correct script!'')')
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
