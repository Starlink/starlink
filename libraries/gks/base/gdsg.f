C# IL>=a, OL>=1
      SUBROUTINE GDSG(ISGNO)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  DELETE SEGMENT
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User level delete segment.
*
*  MAINTENANCE LOG
*  ---------------
*     18/11/83  JGW   Original version stabilised
*     18/11/83  AS    Include CHECK.INC, other minor changes
*     25/11/83  AS    Maintain KNUMSG, number of segments in use
*     16/12/83  JRG   Error 917->1017; correct test before GKERR call
*     19/12/83  JRG   Check operating state before using open segment name
*      20/3/84  JRG   Remove call to CSS (->GKSOPS for bug I163)
*     16/03/88  RMK   Changed error 1017 to -1017 (S316).
*
*  ARGUMENTS
*  ---------
*     INP   ISGNO  segment name
*
      INTEGER ISGNO
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    RGN: regenerate
*     Modify /SL/     KSTRWK, KSFAWK
*     Read   /OPS/    Operating State
*     Modify /ERR/    KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
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
*      7   GKS state error.
*    120   Segment name invalid.
*    122   Segment name not known.
*    125   Segment is open.
*
*-----------------------------------------------------------------------

*     -- validate call & argument
      CALL GKPRLG(EDSG,GWSOP,GSGOP)
      IF (KERROR.NE.0) GOTO 999

      IF (ISGNO.LT.1) THEN
         KERROR = 120
         GOTO 999
      ENDIF
      IF (KSGLST.NE.KNIL)
     :    CALL GKDRGE(KSGLST,ISGNO,KSGISZ,KSGRSZ,IARRAY,RARRAY)
      IF (KERROR.EQ.-1017 .OR. KSGLST.EQ.KNIL) KERROR=122
      IF( KERROR.NE.0 ) GOTO 999
      IF (KOPS.EQ.GSGOP .AND. ISGNO.EQ.KOPSG) THEN
         KERROR = 125
         GOTO 999
      ENDIF

*     -- validation OK, pass request to all open w/s --
*     =================================================
      KWI1  = ISGNO
      CALL GKSOPS(KDSG,1,KDAT,1,QDAT,QDAT,1,CH)
      KNUMSG = KNUMSG - 1
      IF (KERROR.NE.0)  GOTO 999
      IF (KRGN) THEN
          CALL GKRGN
          IF (KERROR.NE.0) GOTO 999
      ENDIF
      CALL GKDRDE(KSGLST,ISGNO)

  999 CONTINUE
      IF (KERROR.NE.0)  CALL GKERR(KERROR)
      END
