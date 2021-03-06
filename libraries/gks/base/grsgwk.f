C# IL>=a, OL>=1
      SUBROUTINE GRSGWK(IWKID)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  REDRAW ALL SEGMENTS ON WORKSTATION
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To clear the display surface and redraw all visible segments
*
*  MAINTENANCE LOG
*  ---------------
*     05/05/83   JRG  Original version stabilized
*     14/06/83   JRG  Make call to REDRAW ALL SEGMENTS ON WORKSTATION entry
*                     to workstation driver
*      23/6/83   JRG  Changes to use KERROR for error reporting
*
*  ARGUMENTS
*  ---------
*     INP   IWKID  Workstation identifier of workstation to be operated on
*
      INTEGER IWKID
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/ Inspect KWDONE after workstation calls
*     Read   /GKYERR/ KERROR
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     INFA   Reply from GKDDA (ignored in this subroutine)
*
      INTEGER INFA
*
*  ERRORS
*  ------
*        7   Wrong GKS state
*       20   Workstation Identifier invalid
*       25   Workstation not open
*  33,35,36  Workstation Category not suitable for this routine
*
*---------------------------------------------------------------------


*   GKS Prologue
      CALL GKPRLG (ERSGWK,GWSOP,GSGOP)
      IF( KERROR.GT.0 ) GOTO 990

*   Send REDRAW ALL SEGMENTS ON WORKSTATION to workstation. If refused,
*   then split into individual actions.
      CALL GKSONW(IWKID,KRSGWK, 1,KDAT, 1,QDAT,QDAT,1,CH)
      IF( KERROR.NE.0 ) GOTO 990
      IF( KWDONE.EQ.KRFUSE ) THEN

*   Perform deferred actions. INFA on return is ignored in this routine.
        CALL GKDDA(IWKID,INFA)
        IF( KERROR.NE.0 ) GOTO 990

*   Clear display surface
        CALL GKCLDS(IWKID,GCONDI)
        IF( KERROR.NE.0 ) GOTO 990

*   Redraw all visible segments
        CALL GKRDRW(IWKID)
        IF( KERROR.NE.0 ) GOTO 990
*   No need to call GKTOLD here as GKCLDS has done it
      ELSE
        CALL GKTOLD
      ENDIF
      GOTO 999

*   Report error
  990 CALL GKERR(KERROR)

*   Finish
  999 CONTINUE
      END
