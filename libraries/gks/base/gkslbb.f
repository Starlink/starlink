C# IL>=a, OL>=0
      SUBROUTINE GKSLBB(ISGLST, ISPEC, BOUND)
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
*     Workstation Segment List Utility : Set Bounding box
*
*  MAINTENANCE LOG
*  ---------------
*     04/08/83  CJW   Original version stabilized.
*     21/01/87  ARG   IS conversion. Error number changed.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*     06/08/90  KEVP  Removed unused local variable (S342).
*
*  ARGUMENTS
*  ---------
*     INP   ISGLST Segment List ( Heap Index )
*     INP   ISPEC  Segment Specification
*     INP   BOUND  Bounding Box
*
      INTEGER ISGLST, ISPEC
      REAL BOUND(1:4)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKERR/    KERROR
*     Modify /GKHP/     Heap
*
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
*
*  LOCALS
*  ------
*     IPOINT Pointer to an entry
*     ITYPE  Specific field required in entry
*     IHEAP  Heap pointer
*     IOFF   Entry number in block (0:KMXSL-1)
*     IADDRR Statement function to return the address of an real item in the
*            current entry (current entry defined by IHEAP and IOFF)
*
      INTEGER IPOINT, ITYPE, IHEAP, IOFF, IADDRR
*
*  HEAP USAGE
*  ----------
*     Accessed
*
*  ERRORS
*  ------
*     -2007  Entry not found in list
*
*  COMMENTS
*  --------
*     If Bounding Boxes are not being stored this call is  ignored
*
*---------------------------------------------------------------------

      IADDRR(ITYPE) = KHPXR(IHEAP) + IOFF*KRENSZ+ITYPE

*     Is there a bounding box - if not then ignore the request

      IF (KHP(KHPXI(ISGLST)+KBBXPT).NE.KNIL) THEN

*        Find Entry - Must exist

         CALL GKSLFN(ISGLST, ISPEC, IPOINT)

         IF (IPOINT .EQ. KNIL) THEN
            CALL GKBUG (-2007, 'GKSLBB')
         ELSE
            IHEAP = IPOINT / KMXSL
            IOFF  = MOD(IPOINT, KMXSL)
            IHEAP = KHP(KHPXI(IHEAP) + KBBXPT)
            QHP(IADDRR(KBOXXL)) = BOUND(1)
            QHP(IADDRR(KBOXXR)) = BOUND(2)
            QHP(IADDRR(KBOXYB)) = BOUND(3)
            QHP(IADDRR(KBOXYT)) = BOUND(4)
         END IF

      END IF

      END
