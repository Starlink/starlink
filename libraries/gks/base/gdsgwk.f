C# IL>=a, OL>=1
      SUBROUTINE GDSGWK(IWK,ISGNO)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  DELETE SEGMENT FROM WORKSTATION
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User level delete segment from workstation.
*
*  MAINTENANCE LOG
*  ---------------
*     18/11/83  JGW   Original version stabilised
*     18/11/83  AS    Include CHECK.INC, other minor changes
*     25/11/83  AS    Maintain KNUMSG, number of segments in use
*     16/12/83  JRG   Error 917 -> 1017, 122 -> 123
*     19/12/83  JRG   Check operating state before using open segment name
*       3/1/84  JRG   Comment correction
*      20/3/84  JRG   (a) CSS request moved (-> (GKSONS for bug I163)
*                     (b) Make KWI2 a PARAMETER (bug I164)
*     20/01/87  ARG   IS conversion. Error number changed.
*     27/03/90  RMK   Added checks that wkstn is not WISS or MI (S385).
*     30/04/90  RMK   Added checks on workstation identifier.
*
*  ARGUMENTS
*  ---------
*     INP   IWK       workstation identifier
*     INP   ISGNO     segment name
*
      INTEGER IWK, ISGNO
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    RGN: regenerate
*     Modify /SL/     KSTRWK, KSFAWK
*     Read   /OPS/    Operating state
*     Modify /ERR/    KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkerr.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkdt.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkops.cmn'
*
*  LOCALS
*  ------
*     IARRAY Integer array for inquire segment directory
*     RARRAY Real array for inquire segment directory
*
      INTEGER IARRAY(KSGISZ)
      INTEGER I, IWKTYP
      REAL    RARRAY(KSGRSZ)
*
*  ERRORS
*  ------
*      7   GKS in wrong state.
*     20   Specified workstation identifier is invalid
*     25   Specified workstation is not open
*     33   Specified workstation is of category MI
*     35   Specified workstation is of category INPUT
*    120   Segment name invalid.
*    123   Segment not known to w/s.
*    125   Segment is open.
*
*----------------------------------------------------------------------------

*     -- validate call & argument --
*     ==============================

      CALL GKPRLG(EDSGWK,GWSOP,GSGOP)
      IF (KERROR.NE.0) GOTO 999

*     -- check workstation identifier --
      IF (IWK.LT.1) THEN
         KERROR = 20
         GOTO 999
      ENDIF

*     -- check workstation category--
*     -- first, need to work out what the workstation type is --
      CALL GKFIND(KWKID, KWK, IWK, 25, I)
      IF (KERROR.NE.0) GOTO 999
      IWKTYP = KLAWKT(KWTYIX(I))
      CALL GKSONW(IWKTYP,KQWKCA,1,KDAT,1,QDAT,QDAT,1,CH)
*     -- now check the category --
      IF (KWI1.EQ.GINPUT) THEN
         KERROR = 35
         GOTO 999
      ELSE IF (KWI1.EQ.GMI) THEN
         KERROR = 33
         GOTO 999
      ENDIF

      IF (ISGNO.LT.1) THEN
         KERROR = 120
         GOTO 999
      ENDIF
      IF (KSGLST.EQ.KNIL) THEN
         KERROR = 123
      ELSE
         CALL GKDRGE(KSGLST,ISGNO,KSGISZ,KSGRSZ,IARRAY,RARRAY)
         IF (KERROR .EQ. -1017) THEN
*           -- seg not found in directory
            KERROR = 123
         ENDIF
      ENDIF
      IF( KERROR.NE.0 ) GOTO 999
      IF (KOPS.EQ.GSGOP .AND. ISGNO.EQ.KOPSG) THEN
         KERROR = 125
         GOTO 999
      ENDIF

*     -- validation OK, pass request to the specified w/s --
*     =================================================

      KWI1  = ISGNO
      CALL GKSONS(IWK,KDSG,1,KDAT,1,QDAT,QDAT,1,CH)
      IF (KERROR.NE.0)  GOTO 999
      IF (KWI2.EQ.GABSNT) THEN
         KERROR = 123
         GOTO 999
      ENDIF



*     -- update segment entry --
*     ==========================
      IARRAY(KSGNWS) = IARRAY(KSGNWS) - 1
      IF ( IARRAY(KSGNWS).EQ.0 ) THEN
         CALL GKDRDE(KSGLST,ISGNO)
         KNUMSG = KNUMSG - 1
      ELSE
         CALL GKDRPU(KSGLST,ISGNO,KSGISZ,KSGRSZ,IARRAY,RARRAY)
      ENDIF

*     -- regenerate if necessary --
*     =============================
      IF (KRGN) THEN
         CALL GKRGN
         IF (KERROR.NE.0) GOTO 999
      ENDIF
      RETURN

  999 CONTINUE
      CALL GKERR(KERROR)
      END
