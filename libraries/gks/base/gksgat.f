C# IL>=a, OL>=2
      SUBROUTINE GKSGAT(IWKID)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  Front End Utility
*  Author:           MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Sets state of output attributes on workstation(s).
*
*   * This is an initial version until the issue in GIN137 is resolved.
*     This version sets the catch-up flags to indicate that
*     transformation, polyline, polymarker, text, and fill area
*     attributes are out-of-date.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilised
*
*  ARGUMENTS
*  ---------
*     INP  IWKID  workstation identifier (see comment)
*
      INTEGER IWKID
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /SL/     KSTRWK..KSFAWK
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.par'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     None
*
*  ERRORS
*  ------
*     None
*
*  COMMENTS
*  --------
*  The workstation identifier supplied to this routine may be a
*  valid workstation identifier, or it may be nil (denoting all
*  active workstations).
*
*---------------------------------------------------------------------

*     -- set catch up flags --
      KSTRWK = KHANGE
      KSPLWK = KHANGE
      KSPMWK = KHANGE
      KSTXWK = KHANGE
      KSFAWK = KHANGE

      RETURN
      END
