C# IL>=a, OL>=0
      SUBROUTINE GKCSAC(NCD, STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Add string to CSS segment
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*     17/08/90  KEVP  Removed Declarations of LOCALS (S342).
*
*  ARGUMENTS
*  ---------
*     INP NCD    Number of characters
*     INP STR    String
*
      INTEGER NCD
      CHARACTER *(*) STR
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkcss.cmn'
*
*  LOCALS
*  ------
*
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------
      END
