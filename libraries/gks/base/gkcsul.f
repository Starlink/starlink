C# IL>=a, OL>=0
      SUBROUTINE GKCSUL(INDEX)
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
*     CSS : Unlock a Page in memory
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*     02/10/85  JRG   Removed blank line at end of file (bug S96)
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
*     The lock flag of page INDEX is decremented by one.
*     This puts a UNLOCKed page into state FREE and an LOCKed page
*     into state UNLOCK.
*
*     FREE   : The page is not in use
*     UNLOCK : The page is not in use but it contains data not yet
*              written to disk. It may be swopped out.
*     LOCK   : The page is busy and cannot be swopped out.
*
*---------------------------------------------------------------------

      KCSTYP(INDEX) = KCSTYP(INDEX) - 1
      END
