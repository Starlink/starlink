C# IL>=a, OL>=0
      SUBROUTINE GECLKS
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Emergency Close GKS
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To close GKS regardless of the current state. This routine
*     may be called by the user at any time. The effect of this
*     routine can also be achieved from a call internal to GKS
*     (this would happen if some irretrievable problem occurs ...
*     usually a bug). However such an internal call must use
*     an internal emergency close GKS (GKECKS). Incest would
*     would occur if (e.g.) GKPRLG or GKERR called GECLKS.
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
*     Only include files are for PARAMETERs
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
*
*  ERRORS
*  ------
*     None are allowed to be reported in Emergency Close GKS
*
*---------------------------------------------------------------------


*   GKS Prologue. Any GKS state is valid. Reason why Emergency Close GKS
*   is calling GKS Prologue is that we need to ensure that GKS is
*   initialised (in the sense that the GKS Operating State has a valid
*   value .... upon loading it does not).
*   The "routine identifier" is KNIL to make Emergency Close GKS
*   look like an inquiry (it shares the property that errors are not
*   reported).
      CALL GKPRLG(KNIL, GGKCL,GSGOP)

*   Remainder of actions are carried out by internal routine.
      CALL GKECKS

*
      END
