C# IL>=a, OL>=0
      SUBROUTINE GCLKS
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Close GKS
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To close GKS.
*
*  MAINTENANCE LOG
*  ---------------
*      2/11/83  JRG   Created
*
*  ARGUMENTS
*  ---------
*     None
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /ERR/    Inspect KERROR
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     2      GKS in wrong state
*
*---------------------------------------------------------------------


*   GKS Prologue
      CALL GKPRLG(ECLKS,GGKOP,GGKOP)
      IF( KERROR.NE.0 ) GOTO 990

*   Remainder of actions for closing GKS. They are in a separate
*   subroutine to allow Emergency Close GKS to call them.
      CALL GKCLKS
      GOTO 999

*   Here to report errors
  990 CALL GKERR(KERROR)

*
  999 CONTINUE
      END
