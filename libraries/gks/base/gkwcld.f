C# IL>=a, OL>=0
      SUBROUTINE GKWCLD
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation Utility
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To perform any necessary bookkeeping in Workstation State List
*     when a display surface is cleared. The actions needed are:
*        - transfer requested workstation transformation to
*          current, if pending, and mark it not pending
*        - indicate that new frame action is no longer necessary
*        - indicate that display surface is no longer empty
*
*  MAINTENANCE LOG
*  ---------------
*     05/05/83   JRG  Original version stabilized
*      9/11/83   JRG  Minor comment change
*
*  ARGUMENTS
*  ---------
*     None
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/ To obtain the Workstation Index
*     Modify /GKYWSL/ Several Workstation State List entries
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*     IWX    Holds local copy of Workstation Index
*
      INTEGER IWX
*
*---------------------------------------------------------------------


*   Local copy of Workstation Index
      IWX=KWKIX

*   If workstation transformation update is pending,
*   move requested transformation to current and set notpending
      IF( KWKTUP(IWX).EQ. GPEND ) THEN
*       workstation window
          QCWWXL(IWX)=QRWWXL(IWX)
          QCWWXR(IWX)=QRWWXR(IWX)
          QCWWYB(IWX)=QRWWYB(IWX)
          QCWWYT(IWX)=QRWWYT(IWX)
*       workstation viewport
          QCWVXL(IWX)=QRWVXL(IWX)
          QCWVXR(IWX)=QRWVXR(IWX)
          QCWVYB(IWX)=QRWVYB(IWX)
          QCWVYT(IWX)=QRWVYT(IWX)
*       update state
          KWKTUP(IWX)=GNPEND
      ENDIF

*   New frame action now not necessary
      KNFAUP(IWX)=GNO

*   Display surface is now empty
      KDSMT(IWX)=GEMPTY

*   Finish
      END
