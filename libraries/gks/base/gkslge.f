C# IL>=a, OL>=0
      SUBROUTINE GKSLGE(ISGLST, ISPEC, INAME, PRI, BOUND)
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
*     Workstation Segment List Utility : Get Segment into List
*
*  MAINTENANCE LOG
*  ---------------
*     04/08/83  CJW   Original version stabilized
*     17/01/84  JRG   Routine behaves itself if segment list empty
*     20/08/85  RMK   Definition of dummy argument BOUND changed from
*                     length 4 to allow actual array argument of size 1 when
*                     called from GKSGWK (S139 - Salford).
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP   ISGLST Segment List ( Heap Index )
*     INP   ISPEC  Segment specification
*     OUT   INAME  Segment Name
*     OUT   PRI    Segment Priority
*     OUT   BOUND  Bounding Box
*
      INTEGER ISGLST, ISPEC, INAME
      REAL PRI, BOUND(*)
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
*     IPOINT Pointer to free entry
*     IBBOX  Boundary box flag
*     ITYPE  Specific field required in entry
*     IHEAP  Heap pointer
*     IOFF   Entry number in block (0:KMXSL-1)
*     IADDRI Statement function to return the address of an integer item in the
*            current entry (current entry defined by IHEAP and IOFF)
*     IADDRR Statement function to return the address of an real item in the
*            current entry (current entry defined by IHEAP and IOFF)
*
      INTEGER IPOINT, IBBOX, IHEAP, IOFF, IADDRI, IADDRR, ITYPE
*
*  HEAP USAGE
*  ----------
*     Accessed directly
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      IADDRI(ITYPE) = KHPXI(IHEAP) + IOFF*KIENSZ+ITYPE
      IADDRR(ITYPE) = KHPXR(IHEAP) + IOFF*KRENSZ+ITYPE

      IF( ISGLST.EQ.KNIL ) THEN
      INAME=KNIL
      ELSE
*     Is there a bounding box

      IF (KHP(KHPXI(ISGLST)+KBBXPT).EQ.KNIL) THEN
         IBBOX = KNOBOX
      ELSE
         IBBOX = KBOX
      END IF

*     Find Entry

      CALL GKSLFN(ISGLST, ISPEC, IPOINT)

      IF (IPOINT .EQ. KNIL) THEN
         INAME = KNIL
      ELSE

*        Extract segment information

         IHEAP = IPOINT / KMXSL
         IOFF  = MOD(IPOINT, KMXSL)
         INAME = KHP(IADDRI(KNAME))
         PRI = FLOAT(KHP(IADDRI(KPRI ))) / QPRISC

         IF (IBBOX .EQ. KBOX) THEN
            IHEAP = KHP(KHPXI(IHEAP) + KBBXPT)
            BOUND(1) = QHP(IADDRR(KBOXXL))
            BOUND(2) = QHP(IADDRR(KBOXXR))
            BOUND(3) = QHP(IADDRR(KBOXYB))
            BOUND(4) = QHP(IADDRR(KBOXYT))
         END IF

      END IF
      END IF

      END
