C# IL>=a, OL>=0
      SUBROUTINE GKHPGR( INDHP, IOFF, ISIZE, RARRAY)
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
*     Get Real data from heap
*
*  MAINTENANCE LOG
*  ---------------
*     17/05/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     05/03/84  CJW   More explicit bugs
*     22/01/87  JCS   IS conversion. Error changes.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP INDHP   Heap Index
*     INP IOFF    Offset within allocation
*     INP ISIZE   Amount to move
*     OUT RARRAY  Array of size ISIZE (or greater) to receive the data
*
      INTEGER INDHP, IOFF, ISIZE
      REAL RARRAY(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read     /GKHP/    Access data in heap
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
*
*  LOCALS
*  ------
*     I      Loop index
*     IADR   Address of beginning of heap allocation (derived index)
*
      INTEGER I, IADR
*
*  HEAP USAGE
*  ----------
*     Access only
*
*  ERRORS
*  ------
*    -2004 Documented condition to be satisfied by parameter(s) of
*          internal routine is not satisfied
*    -2016 Invalid heap pointer
*    -2019 Internal inconsistency in heap management
*
*  COMMENTS
*  --------
*     This routine knows about the heap structure - it can do more
*     checking that the caller.
*
*     The following are all considered to be bugs -
*
*    (-2004) Size bad :
*                         ISIZE < 0
*                         (IOFF + ISIZE) > KHPSZI
*                         (IOFF + ISIZE) > KHPESI(KHPLXI(INDHP))
*
*    (-2004) offset bad :
*                         IOFF < 0
*
*    (-2016) Heap index bad :
*                         INDHP < 0
*                         INDHP > KHPXSI
*                         KHPXI(INDHP) = KNIL
*
*    (-2019) Heap not consistent :
*                         KHPXPI(KHPLXI(INDHP) <> INDHP
*
*---------------------------------------------------------------------


*     Check Arguments

      IF (      (ISIZE .LT. 0) .OR. (IOFF .LT. 0)       .OR.
     :          ((IOFF + ISIZE) .GT. KHPSZR)            .OR.
     :          ((IOFF + ISIZE) .GT. KHPESR(KHPLXR(INDHP)))) THEN
           CALL GKBUG(-2004,'GKHPGR')
      ELSE IF ((INDHP .LT. 0) .OR. (INDHP .GT. KHPXSR)  .OR.
     :          (KHPXR(INDHP) .EQ. KNIL)) THEN
           CALL GKBUG(-2016,'GKHPGR')
      ELSE IF (KHPXPR(KHPLXR(INDHP)) .NE. INDHP) THEN
           CALL GKBUG(-2016,'GKHPGR')
      ELSE
         IADR = KHPXR(INDHP) + IOFF
         DO 1 I = 1, ISIZE
            RARRAY(I) = QHP(IADR)
            IADR = IADR + 1
    1    CONTINUE
      END IF


      END
