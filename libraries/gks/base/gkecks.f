C# IL>=a, OL>=0
      SUBROUTINE GKECKS
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
*     Internal routine to call emergency close. Does everything except
*     call GKS Prologue, which is called from the GKS Function
*     Emergency Close GKS. Separate routine allows (e.g.) GKS Prologue
*     and GKERR to call Emergency Close without incest.
*
*  MAINTENANCE LOG
*  ---------------
*      2/11/83  JRG   Created
*     16/02/84  JRG   Currently, does not Close Workstation. Put it
*                     back in when workstation interface defined for this
*
*  ARGUMENTS
*  ---------
*     None
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /WCB/    Alter workstation lists
*     Read   /WCA/    Use dummy data
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  ERRORS
*  ------
*     No errors can be reported by Emergency Close GKS
*
*---------------------------------------------------------------------


* ???? Need to add statements to CLOSE SEGMENT and update all open
* ???? workstations (the latter could be incorporated with CLOSE WORKSTATION)

*   Deactivate all workstations .... just set the list length to zero
      KNACWK=0

* ???? Next statement commented out for the moment, because it may
* ???? cause problems in workstations. Essentially, the workstations
* ???? should have an entrypoint that just performs a disconnection
* ???? from the device as cleanly as possible.
*     KWI1=3
*     CALL GKSOPW(KCLWK, 1,KDAT, 1,QDAT,QDAT, 1,CH)

*   Close GKS
      CALL GKCLKS

*
      END
