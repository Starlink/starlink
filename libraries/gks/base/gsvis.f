C# IL>=a, OL>=0
      SUBROUTINE GSVIS(ISGNO,ISGVIS)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET VISIBILITY
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User level set segment visibility.
*
*  MAINTENANCE LOG
*  ---------------
*     18/11/83  JGW   Original version stabilised
*     18/11/83  AS    Include CHECK.INC, other minor changes
*     22/11/83  AS    Change error number 917 to 1017
*     16/12/83  JRG   Pick up other errors from GKDRGE
*     04/01/83  JRG   KCVIS only set if this segment is open; more error
*                     checking
*     02/10/85  JRG   Remove blank line after END (bug S96)
*     20/01/87  ARG   IS conversion. Error numbers changed. Added check
*                     on visibility value.
*
*  ARGUMENTS
*  ---------
*     INP     ISGNO    segment name
*     INP     ISGVIS   visibility { visible,invisible }
*
      INTEGER ISGNO, ISGVIS
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    RGN: regenerate
*     Modify /SL/     KSTRWK, KSFAWK
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkops.cmn'
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
*      7    GKS state error
*    120    Invalid segment name
*    122    Segment does not exist
*   2000    Enumeration type out of range
*
*----------------------------------------------------------------------------

*     -- validate call & argument
      CALL GKPRLG(ESVIS,GWSOP,GSGOP)
      IF (KERROR.NE.0) GOTO 999

      IF (ISGNO.LT.1) THEN
         KERROR = 120
         GOTO 999
      ENDIF

      IF (ISGVIS .NE. GINVIS .AND. ISGVIS .NE. GVISI) THEN
         KERROR = 2000
         GOTO 999
      ENDIF
      IF (KSGLST.NE.KNIL)
     :    CALL GKDRGE(KSGLST,ISGNO,KSGISZ,KSGRSZ,IARRAY,RARRAY)
      IF (KERROR .EQ. -1017 .OR. KSGLST .EQ. KNIL) KERROR = 122
      IF( KERROR.NE.0 ) GOTO 999

*     -- validation OK, update segment details then send w/s --
*     =========================================================
      IARRAY(KSGVIS) = ISGVIS
      CALL GKDRPU(KSGLST,ISGNO,KSGISZ,KSGRSZ,IARRAY,RARRAY)
      IF( KERROR.NE.0 ) GOTO 999
      KWI1 = ISGNO
      KWI2 = KSNOPN
      KWI3 = GNEMPT
      IF (KOPS.EQ.GSGOP .AND. ISGNO.EQ.KOPSG) THEN
         KWI2 = KSOPN
         CALL GKCSSG(KWI3)
      ENDIF
      KWI4 = IARRAY(KSGVIS)
      KWI5 = ISGVIS
      CALL GKSOPS(KSVIS,1,KDAT,1,QDAT,QDAT,1,CH)
      IF( KERROR.NE.0 ) GOTO 999
      IF( KOPS.EQ.GSGOP .AND. ISGNO.EQ.KOPSG ) KCVIS = ISGVIS
      IF (KRGN) THEN
         CALL GKRGN
         IF (KERROR .NE. 0) GOTO 999
      ENDIF
      RETURN

  999 CONTINUE
      CALL GKERR(KERROR)
      END
