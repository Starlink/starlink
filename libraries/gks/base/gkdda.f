C# IL>=a, OL>=0
      SUBROUTINE GKDDA(IWKID,INFA)
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
*     To perform deferred actions. This routine is responsible for
*     for sending the appropriate command to the workstation driver.
*     This routine also returns to the caller an indication of
*     whether a new frame action is necessary.
*
*  MAINTENANCE LOG
*  ---------------
*     05/05/83   JRG  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP   IWKID  Workstation Identifier of workstation on which
*                  deferred actions are to be performed.
*     OUT   INFA   'New Frame Action Necessary on Update' from this
*                  workstation (possible values 'yes' or 'no').
*
      INTEGER IWKID,INFA
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/ Dummy arrays KDAT & QDAT, return value KWI1.
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


*   Call Workstation driver
      CALL GKSONW(IWKID, KDDA, 1,KDAT, 1,QDAT,QDAT,1,CH)

*   Transfer 'new frame action necessary'
      INFA=KWI1

      END
