C# IL>=a, OL>=0
      SUBROUTINE GKSLSP(ISGLST, IPRI, ILOER, IHIER)
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
*     Workstation Segment List Internal Utility : Search on Priority
*
*  MAINTENANCE LOG
*  ---------------
*     04/08/83  CJW   Original version stabilized
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP   ISGLST Segment list pointer (Heap index)
*     INP   ILOER  Pointer to an entry of lower or equal priority
*     OUT   IHIER  Pointer to an entry of higher or equal priority
*     OUT   IPRI   Internal form of segment priority
*
      INTEGER ISGLST, ILOER, IHIER, IPRI
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKHP/     Heap
*
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
*
*  LOCALS
*  ------
*     ITYPE  Specific field required in entry
*     IHEAP  Heap pointer
*     IOFF   Entry number in block (0:KMXSL-1)
*     IADDRI Statement function to return the address of an integer item in the
*            current entry (current entry defined by IHEAP and IOFF)
*
      INTEGER ITYPE, IOFF, IHEAP, IADDRI
*
*  HEAP USAGE
*  ----------
*     Accessed
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      IADDRI(ITYPE) = KHPXI(IHEAP) + IOFF*KIENSZ+ITYPE

*     search on priority  (IPRI, ILOER, IHIER)

      IHIER = KHP(KHPXI(ISGLST)+KLOPTR)
      IHEAP = IHIER / KMXSL
      IOFF  = MOD(IHIER, KMXSL)

*     While IHIER <> KNIL and IHIER(pri) <= IPRI do
    1 CONTINUE
      IF (IHIER .EQ. KNIL) GO TO 2
      IF (IPRI .LT. KHP(IADDRI(KPRI))) GO TO 2

         IHIER = KHP(IADDRI(KHIGHP))
         IHEAP = IHIER / KMXSL
         IOFF  = MOD(IHIER, KMXSL)

      GO TO 1
    2 CONTINUE
*     End While

      IF (IHIER .NE. KNIL) THEN
         ILOER = KHP(IADDRI(KLOWP))
      ELSE
         ILOER = KHP(KHPXI(ISGLST)+KHIPTR)
      END IF

      END
