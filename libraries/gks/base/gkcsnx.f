C# IL>=a, OL>=0
      SUBROUTINE GKCSNX(INDEX)
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
*     CSS : Swop Page for next free record
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*      7/02/84  CJW   Use GKCSGT instead of GKCSIN
*                     This handles free chain blocks in memory
*
*  ARGUMENTS
*  ---------
*     I/O INDEX  Page number
*
      INTEGER INDEX
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkcss.cmn'
*
*
*  LOCALS
*  ------
*     IFREE  Page number of a free record
*
      INTEGER IFREE
*
*  COMMENTS
*  --------
*     Selects a new record and ensures the current record is chained
*     Finds an unused record and a page to hold it in.
*
*---------------------------------------------------------------------

      IF (KCSFCH .NE. KNIL) THEN
         CALL GKCSGT(KCSFCH, IFREE)
         KCSFCH = KCSNXT(IFREE)
      ELSE
         CALL GKCSFP(IFREE)
         KCSREC(IFREE) = KCSNFR
         KCSNFR = KCSNFR + 1
*        lock the page so that in can't be swopped out
         CALL GKCSLK(IFREE)
         CALL GKCSLK(IFREE)
      END IF

      KCSNXT(IFREE) = KNIL


      IF (INDEX .NE. KNIL) THEN
*        Link old record to new
         KCSNXT(INDEX) = KCSREC(IFREE)

*        Unlock the old record
         CALL GKCSUL(INDEX)
      END IF

      INDEX = IFREE

      END
