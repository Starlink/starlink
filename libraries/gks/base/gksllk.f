C# IL>=a, OL>=0
      SUBROUTINE GKSLLK(ISGLST, ILOER, IHIER)
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
*     Workstation Segment List Internal Utility : Link Segments
*
*  MAINTENANCE LOG
*  ---------------
*     04/08/83  CJW   Original version stabilized
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP   ISGLST Segment list (heap index)
*     INP   ILOER  Pointer to an entry of lower or equal priority
*     INP   IHIER  Pointer to an entry of higher or equal priority
*
      INTEGER ISGLST, ILOER, IHIER
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKHP/     Heap
*
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkdt.par'
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

*     Link up

      IF (ILOER .EQ. KNIL) THEN
         KHP(KHPXI(ISGLST)+KLOPTR) = IHIER
      ELSE
         IHEAP = ILOER / KMXSL
         IOFF  = MOD(ILOER, KMXSL)
         KHP(IADDRI(KHIGHP)) = IHIER
      END IF


*     Link down

      IF (IHIER .EQ. KNIL) THEN
         KHP(KHPXI(ISGLST)+KHIPTR) = ILOER
      ELSE
         IHEAP = IHIER / KMXSL
         IOFF  = MOD(IHIER, KMXSL)
         KHP(IADDRI(KLOWP)) = ILOER
      END IF

      END
