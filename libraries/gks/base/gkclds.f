C# IL>=a, OL>=0
      SUBROUTINE GKCLDS(IWKID,ICOFL)
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
*     Any need in the front end to clear the display surface
*     passes through this routine, which makes the call to the
*     workstation call layer. After the call has returned,
*     the flags indicating the state on all active workstations
*     of the transformations, text attributes and fill area
*     attributes are set to be 'out of date'.
*
*  MAINTENANCE LOG
*  ---------------
*     05/05/83   JRG  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP   IWKID  Workstation Identifier of workstation on which
*                  display surface is to be cleared.
*     INP   ICOFL  Clear control flag (possible values are
*                  'condit' or 'always').
*
      INTEGER IWKID,ICOFL
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYWCA/ KWI1 written to before call, KDAT & QDAT arrays used
*                     as dummies.
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*     None
*
*---------------------------------------------------------------------


*   Set up data for and make call to workstation call layer to
*   clear display surface.
      KWI1=ICOFL
      CALL GKSONW(IWKID, KCLDS, 1,KDAT, 1,QDAT,QDAT,1,CH)

*   Set transformation, fill area and text flags to be out of date.
      CALL GKTOLD

*   Finish
      END
