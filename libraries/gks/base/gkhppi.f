C# IL>=a, OL>=0
      SUBROUTINE GKHPPI( INDHP, IOFF, ISIZE, IARRAY)
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
*     Put Integer data into heap
*
*  MAINTENANCE LOG
*  ---------------
*     17/05/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     05/03/84  CJW   More explicit bugs
*     19/01/87  PKY   IS conversion. Error number changes.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP INDHP   Heap Index (as returned by GKHPAL)
*     INP IOFF    Offset within allocation
*     INP ISIZE   Amount to move
*     INP IARRAY  Array of size ISIZE (or greater) to receive the data
*
      INTEGER INDHP, IOFF, ISIZE, IARRAY(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify   /GKHP/    Access data in heap
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
*
*  LOCALS
*  ------
*     I      Loop index
*     IADI   Address of beginning of heap allocation (derived index)
*
      INTEGER I, IADI
*
*  HEAP USAGE
*  ----------
*     Access only
*
*  ERRORS
*  ------
*    -2004 Documented condition to be satisfied by parameter(s) of
*          internal routine is not satisfied
*    -2016 Invalid Heap pointer
*    -2019 Internal inconsistency in heap management
*
*  COMMENTS
*  --------
*     This routine knows about the heap structure - it can do more
*     checking that the caller.
*
*     The following are all considered to be bugs -
*
*    (-2004) Size Bad :
*                         ISIZE < 0
*                         (IOFF + ISIZE) > KHPSZI
*                         (IOFF + ISIZE) > KHPESI(KHPLXI(INDHP))
*
*    (-2004) Offset Bad :
*                         IOFF < 0
*
*    (-2016) Heap Index Bad:
*                         INDHP < 0
*                         INDHP > KHPXSI
*                         KHPXI(INDHP) = KNIL
*
*    (-2019) Heap Not Consistent
*                         KHPXPI(KHPLXI(INDHP) <> INDHP
*
*---------------------------------------------------------------------


*     Check Arguments
      IF (      (ISIZE .LT. 0) .OR. (IOFF .LT. 0)       .OR.
     :          ((IOFF + ISIZE) .GT. KHPSZI)            .OR.
     :          ((IOFF + ISIZE) .GT. KHPESI(KHPLXI(INDHP)))) THEN
         CALL GKBUG(-2004, 'GKHPPI')
      ELSE IF ((INDHP .LT. 0) .OR. (INDHP .GT. KHPXSI) .OR.
     :          (KHPXI(INDHP) .EQ. KNIL)) THEN
         CALL GKBUG(-2016, 'GKHPPI')
      ELSE IF (KHPXPI(KHPLXI(INDHP)) .NE. INDHP) THEN
         CALL GKBUG(-2019, 'GKHPPI')

      ELSE
         IADI = KHPXI(INDHP) + IOFF
         DO 1 I = 1, ISIZE
            KHP(IADI) = IARRAY(I)
            IADI = IADI + 1
    1    CONTINUE
      END IF


      END
