C# IL>=a, OL>=0
      SUBROUTINE GKCSFP(INDEX)
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
*     CSS : Looks for a free page.
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*     19/01/87  PKY   IS conversion. Error number changes.
*
*  ARGUMENTS
*  ---------
*     OUT INDEX  Index of free page
      INTEGER INDEX
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkcss.cmn'
*
*
*  LOCALS
*  ------
*     IPAGE  Page number ( loop index )
*
      INTEGER IPAGE
*
*  ERRORS
*  ------
*    -2013 No free pages in CSS
*
*  COMMENTS
*  --------
*       There are three cases -
*
*     - Finds a free page
*     - Cannot find a free page but find an unlocked page. This
*       is made free by outputing the page.
*     - No free or unlocked page can be found. This is a BUG. Someone
*       has been grabbing pages and not letting go.
*
*       INDEX is FREE (already is free or is FREEd by GKCSOT)
*
*---------------------------------------------------------------------


      INDEX = 1
      DO 1 IPAGE = 1, KCSPAG
         IF (KCSTYP(IPAGE) .EQ. KCSFRE) THEN
            INDEX = IPAGE
            RETURN
         ELSE IF(KCSTYP(IPAGE) .LT. KCSTYP(INDEX)) THEN
            INDEX = IPAGE
         END IF
    1 CONTINUE
      IF (KCSTYP(INDEX) .GE. KCSLOK) THEN
         CALL GKBUG(-2013,'GKCSFP')
         INDEX = KNIL
      ELSE
         CALL GKCSOT(INDEX)
      END IF

      END
