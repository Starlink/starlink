      SUBROUTINE GUWK(IWKID, IRGEN)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  UPDATE WORKSTATION
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*    To update the workstation. The actions are:
*      perform deferred actions;
*      if specified (by IRGEN) and if pending then
*           clear display surface (+ transfer requested
*                     workstation transformation to current +
*                     DisplaySurfaceEmpty <- EMPTY
*                     NewFrameActionNecessaryAtUpdate <- NO )
*           redraw all visible segments
*      endif
*
*  MAINTENANCE LOG
*  ---------------
*     05/07/83  JRG   Original version stabilized
*     28/09/83  AS    Change subroutine name
*     20/01/87  PKY   IS conversion. Error number changes. Change
*                     SUPPRESS to POSTPONE and change GSUPP to GPOSTP.
*
*  ARGUMENTS
*  ---------
*     INP   IWKID  Workstation identifier of workstation to be operated on
*     INP   IRGEN  Regeneration flag (value PERFORM or POSTPONE)
*
      INTEGER IWKID, IRGEN
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/ Set KWI1 before call to workstation; inspect
*                     KWDONE after call
*     Read   /GKYERR/ KERROR
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     INFA   Reply from GKDDA
*
      INTEGER INFA
*
*  ERRORS
*  ------
*        7   Wrong GKS state
*       20   Workstation Identifier invalid
*       25   Workstation not open
*  33,35,36  Workstation Category not suitable for this routine
*     2000  Enumeration type out of range
*
*---------------------------------------------------------------------


*   GKS Prologue
      CALL GKPRLG (EUWK,GWSOP,GSGOP)
      IF( KERROR.GT.0 ) GOTO 990

*   Check value of IRGEN
      IF(.NOT. (IRGEN.EQ.GPERFO .OR. IRGEN.EQ.GPOSTP) ) THEN
          KERROR=2000
          GOTO 990
      ENDIF

*   Send UPDATE WORKSTATION to workstation. If refused,
*   then split into individual actions.
      KWI1=IRGEN
      CALL GKSONW(IWKID,KUWK, 1,KDAT, 1,QDAT,QDAT,1,CH)
      IF( KERROR.NE.0 ) GOTO 990
      IF( KWDONE.EQ.KRFUSE ) THEN

*   Perform deferred actions. INFA on return is ignored in this routine.
        CALL GKDDA(IWKID,INFA)
        IF( KERROR.NE.0 ) GOTO 990

*   If pending and if specified, then regenerate
        IF( INFA.EQ.GYES .AND. IRGEN.EQ.GPERFO ) THEN

*   Clear display surface
          CALL GKCLDS(IWKID,GCONDI)
          IF( KERROR.NE.0 ) GOTO 990

*   Redraw all visible segments
          CALL GKRDRW(IWKID)
          IF( KERROR.NE.0 ) GOTO 990
*   No need to call GKTOLD here as GKCLDS has done it
        ENDIF
      ELSE
        CALL GKTOLD
      ENDIF
      GOTO 999

*   Report error
  990 CALL GKERR(KERROR)

*   Finish
  999 CONTINUE
      END
