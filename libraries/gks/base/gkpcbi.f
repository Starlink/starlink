C# IL>=a, OL>=0
      SUBROUTINE GKPCBI(IENTRY)
*
* (C) COPYRIGHT ICL & SERC  1985
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CSS : Begin item (part of pick echoplay interface)
*
*  MAINTENANCE LOG
*  ---------------
*     01/08/85  MGC   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT IENTRY  - Entrypoint code
*
      INTEGER IENTRY
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkerr.cmn'
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

      CALL GKCSBI
      IENTRY=KCSENT

      END
