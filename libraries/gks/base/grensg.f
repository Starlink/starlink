C# IL>=a, OL>=1
      SUBROUTINE GRENSG(ISGNO,ISGNEW)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  RENAME SEGMENT
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User level rename segment.
*
*  MAINTENANCE LOG
*  ---------------
*     18/11/83  JGW   Original version stabilised
*     18/11/83  AS    Include CHECK.INC, other minor changes
*     16/12/83  JRG   Error 917->1017
*     03/01/84  JRG   Error check after entering workstations
*     20/03/84  JRG   Use GKSOPS not GKSOPW (correct bug I163)
*     25/04/84  CJW   Change name of open segment? (I)
*     15/01/86  DRJF  Bug fix S116. Report error 120 if renaming a segment
*                     to an invalid name.
*     20/01/87  ARG   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     INP   ISGNO   old segment name
*     INP   ISGNEW  new segment name
*
      INTEGER ISGNO, ISGNEW
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    RGN: regenerate
*     Modify /SL/     KSTRWK, KSFAWK
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     IARRAY Integer array for inquire segment directory
*     RARRAY Real array for inquire segment directory
*
      INTEGER IARRAY(KSGISZ)
      REAL    RARRAY(KSGRSZ)
*
*  ERRORS
*  ------
*    120    Specified segment name is invalid
*    121    Specified segment name is already in use
*    122    Specified segment does not exist
*
*----------------------------------------------------------------------------

*     -- validate call & argument
      CALL GKPRLG(ERENSG,GWSOP,GSGOP)
      IF (KERROR.NE.0) GOTO 999

      IF (ISGNO.LT.1.OR.ISGNEW.LT.1) THEN
         KERROR = 120
         GOTO 999
      ENDIF
      IF (KSGLST.NE.KNIL)
     :    CALL GKDRGE(KSGLST,ISGNO,KSGISZ,KSGRSZ,IARRAY,RARRAY)
      IF (KERROR.EQ.-1017 .OR. KSGLST.EQ.KNIL) KERROR = 122
      IF( KERROR.NE.0 ) GOTO 999

*     -- validation OK, update F/E then call all w/s --
*     =================================================
      CALL GKDRRN(KSGLST,ISGNO,ISGNEW)
      IF (KERROR.NE.0) THEN
         KERROR = 121
         GOTO 999
      ENDIF
      KWI1 = ISGNO
      KWI2 = ISGNEW
      CALL GKSOPS(KRENSG,1,KDAT,1,QDAT,QDAT,1,CH)
      IF( KERROR.NE.0 ) GOTO 999
      IF (KOPSG .EQ. ISGNO) KOPSG = ISGNEW
      RETURN

  999 CONTINUE
      CALL GKERR(KERROR)
      END
