C# IL>=a, IL>=0
      SUBROUTINE GOPWK(IWKID,ICONID,IWTYPE)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  GOPWK
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To open another workstation.
*
*  MAINTENANCE LOG
*  ---------------
*     16/03/83  JRG   Installed
*     30/03/83  JRG   CHECK.INC included
*     29/04/83  JRG   Reports error returned by workstation driver
*     13/06/83  JRG   Special treatment for metafile input.
*                     Replace IDAT,RDAT by KDAT,QDAT (in COMMON)
*     23/06/83  JRG   Changes to use KERROR for error reporting
*     10/11/83  RSK   Bug fix (GKS Operating State now set conditionally)
*     15/01/86  DRJF  Bug fix (Reports error 23 now instead of 22, when
*                     opening a workstation with a workstation type which
*                     does not exist) S116.
*     30/01/86  MGC   WISS - workstation type 3 error checking (Level 2a)
*     04/07/86  DCS   Only check workstation type against available
*                     workstation types, so that -1 is not errored
*                     (S116/S199).
*     20/01/87  DCS   IS conversion. Remove initialisation of metafile
*                     index and Metafile Input Attribute List.
*     22/01/87  PKY   IS conversion. Add error number comment.
*     01/06/87  RMK   Merged ICL and RAL versions of this routine.
*     22/02/90  RMK   Added check that con id / wkstn type pair has
*                     not been used already (S77).
*     01/05/90  RMK   Reordered checking so that error 28 rather than
*                     24 generated if WISS already open.
*     25/09/90  KEVP  Check validity of workstation type before checking
*                     whether workstation type exists (C37).
*
*  ARGUMENTS
*  ---------
* Inp     IWKID  Workstation identifier
* Inp     ICONID Connection identifier
* Inp     IWTYPE Workstation type
*
      INTEGER IWKID,ICONID,IWTYPE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYOPS/ Change GKS Operating State if first workstation
*     Read   /GKYDT/  List of workstation types
*     Modify /GKYWCA/ Set up KWI1 before entry to Workstation Call Layer
*     Read   /GKYWCB/ List of workstation identifiers ... to check it
*                     is not already present
*     Modify /GKYERR/ KERROR
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkdt.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     J      Loop counter
*     IWTIX  Index to workstation type

      INTEGER J,IWTIX
*
*  ERRORS
*  ------
*        8  GKS not in proper state: should be GKOP, WSOP, WSAC or SGOP
*       20  Specified workstation identifier is invalid
*       22  Specified workstation type is invalid
*       23  Specified workstation type does not exist
*       24  Specified workstation is open
*       28  Workstation Independent Segment Storage is already open
*
*---------------------------------------------------------------------


*   GKS Prologue
      CALL GKPRLG(EOPWK,GGKOP,GSGOP)
      IF( KERROR.GT.0 ) GOTO 990

*   Check for error (20): Specified Workstation identifier is invalid
      IF( IWKID.LT.1 ) THEN
        KERROR=20
        GOTO 990
      ENDIF

*   Check for error (22): Specified Workstation type is invalid
      IF( IWTYPE.LT.1 ) THEN
        KERROR=22
        GOTO 990
      ENDIF

*   Check for error (28): Wiss already open
      IF( IWTYPE.EQ.3 ) THEN
        DO 160 J=1,KNOPWK
          IF( KWKC(KOPPT(J)).EQ.GWISS ) THEN
            KERROR=28
            GOTO 990
          ENDIF
  160   CONTINUE
      ENDIF
*   Here: WISS workstation type 3 not already open

*   Check for error (23): Specified Workstation type does not exist
      DO 100 IWTIX=1,KAWKT
        IF( KLAWKT(IWTIX).EQ.IWTYPE ) GOTO 110
  100 CONTINUE
      KERROR=23
      GOTO 990
*   Here: workstation type has been found and IWTIX indexes it
  110 CONTINUE

*   Check for error (24): Specified Workstation is open
      DO 150 J=1,KWK
        IF( KWKID(J).EQ.IWKID ) THEN
          KERROR=24
          GOTO 990
        ENDIF
  150 CONTINUE
*   Here: workstation identifier not found, which is what we want

*   Check that this connection identifier and workstation type pair
*   haven't been opened already - generate error 26 if they have
      DO 155 J=1,KNOPWK
        IF (KCID(KOPPT(J))           .EQ. ICONID .AND.
     :      KLAWKT(KWTYIX(KOPPT(J))) .EQ. IWTYPE) THEN
          KERROR = 26
          GOTO 990
        ENDIF
  155 CONTINUE

*   End of preliminary error checking
*---------------------------------------

*   Update Workstation Control Block
      CALL GKOWCB(IWKID,IWTIX,KWKIX)
      IF( KERROR.NE.0 ) GOTO 990

*   Enter workstation driver via GKSONW. The latter attempts to
*   trap error 20, but that has already been checked.
      KWI1=ICONID
      CALL GKSONW(IWKID,KOPWK, 1,KDAT, 1,QDAT,QDAT, 1,CH)

*   Check for error detected in workstation driver.
*   If error has occurred, then we need to
*   remove this new workstation from the Workstation Control Block.
      IF( KERROR.NE.0 ) THEN
        CALL GKCWCB(IWKID)
        GOTO 990
      ENDIF

*   Set Workstation Category in W.C.B. (from KWI1)
      KWKC(KWKIX)=KWI1

*   Set GKS Operating State. Note that test is needed because, if operating
*   state is as advanced as GSGOP, we don't want it to revert to GWSOP.
      IF( KOPS.EQ.GGKOP ) KOPS=GWSOP
      GOTO 999

*   Here to report error number KERROR
  990 CALL GKERR(KERROR)

*   Here to quit
  999 CONTINUE
      END
