C# IL>=a, OL>=0
      SUBROUTINE GKCSSG(ISTAT)
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
*     Return status of open segment
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP ISTAT  Status of open segment

      INTEGER ISTAT
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkfls.cmn'
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


      IF (KCSFLS .EQ. KFLOP) THEN
         ISTAT = KCSSTA
      ELSE
         ISTAT = KNIL
      END IF

      END
