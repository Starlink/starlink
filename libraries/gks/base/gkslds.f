C# IL>=a, OL>=0
      SUBROUTINE GKSLDS(ISGLST, ISPEC)
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
*     Workstation Segment List Utility : Delete Segment from List
*
*  MAINTENANCE LOG
*  ---------------
*     04/08/83  CJW   Original version stabilized.
*     24/04/84  CJW   GKSLLK cannot be used to link free chain (I188).
*     20/01/87  ARG   IS conversion. Error number changed.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP   ISGLST Segment List ( Heap Index )
*     INP   ISPEC  Segment Specification
*
      INTEGER ISGLST, ISPEC
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
*     IPOINT Pointer to an entry
*     IBBOX  Bounding box flag
*
      INTEGER ITYPE, IHEAP, IOFF, IADDRI, IBBOX, IPOINT
*
*  HEAP USAGE
*  ----------
*     Accessed
*
*  ERRORS
*  ------
*     -2007  Entry not found in list
*
*---------------------------------------------------------------------

      IADDRI(ITYPE) = KHPXI(IHEAP) + IOFF*KIENSZ+ITYPE

*     Is there a bounding box

      IF (KHP(KHPXI(ISGLST)+KBBXPT).EQ.KNIL) THEN
         IBBOX = KNOBOX
      ELSE
         IBBOX = KBOX
      END IF

*     Find Entry - Must exist

      CALL GKSLFN(ISGLST, ISPEC, IPOINT)

      IF (IPOINT .EQ. KNIL) THEN
         CALL GKBUG (- 2007, 'GKSLDS')
      ELSE
         IHEAP = IPOINT / KMXSL
         IOFF  = MOD(IPOINT, KMXSL)

         IF (KHP(KHPXI(ISGLST)+KCUPTR) .EQ. IPOINT)
     :       KHP(KHPXI(ISGLST)+KCUPTR) = KNIL

*        Chain other entries around it

         CALL GKSLLK(ISGLST, KHP(IADDRI(KLOWP)),KHP(IADDRI(KHIGHP)))

*        Add segment to free chain

         KHP(IADDRI(KHIGHP)) = KHP(KHPXI(ISGLST)+KFRPTR)
         KHP(IADDRI(KLOWP)) = KNIL
         KHP(KHPXI(ISGLST)+KFRPTR) = IPOINT

      END IF
      END
