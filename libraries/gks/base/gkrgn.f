      SUBROUTINE GKRGN
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front End
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Cause a regeneration to happen on all workstations that
*     have indicated that it is necessary.
*
*  MAINTENANCE LOG
*  ---------------
*     05/05/83   JRG  Original version stabilized
*     14/06/83   JRG  Regenerate does not depend on the
*                     'New Frame Action Necessary on Update' flag
*   23/6/83     Changes to use KERROR for error reporting
*
*  ARGUMENTS
*  ---------
*     None
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYWCA/ KRGN and KWRGN array
*                     set to .FALSE. after regeneration
*     Read   /GKYWCB/ List of workstation identifiers in W.C.B.
*     Read   /GKYERR/ KERROR
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     J     Loop counter
*     IWKID Workstation identifier of workstation being regenerated
*     INFA  Reply from GKDDA indicating whether new frame action
*           is necessary.
*
      INTEGER J,IWKID,INFA
*
*-------------------------------------------------------------------------

*   For each workstation, see if it reports a need for regeneration.
*   This is indicated in KWRGN.
*   If any errors occur, skip remainder of actions for this workstation.
      DO 200 J=1,KWK
        IF( KWRGN(J) ) THEN
            IWKID=KWKID(J)
*         Perform deferred actions
            CALL GKDDA(IWKID,INFA)
            IF( KERROR.NE.0 ) GOTO 180
*         Clear display surface and redraw all visible segments.
*         Note that we do not test whether 'New Frame Action Necessary
*         on Update' is YES:  KWRGN has already told us that we
*         need to regenerate.
            CALL GKCLDS(IWKID,GCONDI)
            IF( KERROR.NE.0 ) GOTO 180
            CALL GKRDRW(IWKID)

*         Regeneration now not needed on this workstation
  180       KWRGN(J)=.FALSE.
        ENDIF
  200 CONTINUE

* Regeneration now not needed at all
      KRGN=.FALSE.

      END
