C# IL>=a, OL>=0
      SUBROUTINE GKCSRL
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
*     Release read segment (reading to end also releases)
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*     29/12/83  CJW   Correct locking of pages
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkcss.cmn'
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      IF ((KCSRDN.NE.KNIL) .AND. (KCSRDX.NE.KNIL)) THEN
         CALL GKCSUL(KCSRDX)
         KCSRDN = KNIL
         KCSRDX = KNIL
      END IF
      END
