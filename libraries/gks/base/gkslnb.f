C# IL>=a, OL>=0
      SUBROUTINE GKSLNB(IBOUND, ISIZE, IBLOCK)
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
*     Workstation Segment List Utility :
*
*  MAINTENANCE LOG
*  ---------------
*     04/08/83  CJW   Original version stabilized
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP   IBOUND Bounding Box Flag
*     INP   ISIZE  Size of Integer allocation to make from Heap
*                  (only values allowed are KMBKSZ or KIBKSZ)
*     OUT   IBLOCK Heap index of created block
*
      INTEGER IBOUND, ISIZE, IBLOCK
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKERR/    KERROR
*     Modify /GKHP/     Heap
*
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     I      Loop index
*     IREAL  Heap Index of real allocation
*     ILOW   Pointer to current entry
*     IHIGH  Pointer to next entry
*     IADDR  Address within the head
*
      INTEGER I, IREAL, ILOW, IHIGH, IADDR
*
*  HEAP USAGE
*  ----------
*     Allocate ISIZE integers, KRBKSZ reals
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

*     Obtain Storage from Integer Heap

      CALL GKHPAL ( ISIZE, KINTGS, IBLOCK)
      IF (KERROR .NE. 0) GO TO 999

*     Set up Bounding Boxes

      IF (IBOUND .EQ. KBOX) THEN
         CALL GKHPAL ( KRBKSZ, KREALS, IREAL)
         IF (KERROR .NE. 0) GO TO 999
         KHP(KHPXI(IBLOCK)+KBBXPT) = IREAL
      ELSE
         KHP(KHPXI(IBLOCK)+KBBXPT) = KNIL
      END IF

*     Link Entries together

      ILOW = IBLOCK * KMXSL
      IADDR = KHPXI(IBLOCK)
      KHP(IADDR + KLOWP) = KNIL
      DO 1 I = 1, KMXSL - 1
         IHIGH = ILOW + 1
         KHP(IADDR + KHIGHP) = IHIGH
         IADDR = IADDR + KIENSZ
         KHP(IADDR + KLOWP) = ILOW
         ILOW = IHIGH
    1 CONTINUE
      KHP(IADDR + KHIGHP) = KNIL

  999 CONTINUE
      END
