C# IL>=a, OL>=0
      SUBROUTINE GKCSLK(INDEX)
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
*     CSS : Lock a Page in memory
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP INDEX  Page index
*
      INTEGER INDEX
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkcss.cmn'
*
*
*  COMMENTS
*  --------
*     The Lock flag of page INDEX is incremented by one.
*     This puts a FREE page into state UNLOCK and an UNLOCKed page
*     into state LOCK.
*
*     FREE   : The page is not in use
*     UNLOCK : The page is not in use but it contains data not yet
*              written to disk. It may be swopped out.
*     LOCK   : The page is busy and cannot be swopped out.
*
*---------------------------------------------------------------------

      KCSTYP(INDEX) = KCSTYP(INDEX) + 1
      END
