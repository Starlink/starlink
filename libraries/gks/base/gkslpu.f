C# IL>=a, OL>=0
      SUBROUTINE GKSLPU(ISGLST, INAME, PRI, BOUND)
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
*     Workstation Segment List Utility : Put Segment into List
*
*  MAINTENANCE LOG
*  ---------------
*     04/08/83  CJW   Original version stabilized
*     24/04/84  CJW   Incorrect sized blocks created (I187)
*     20/08/85  RMK   Definition of dummy argument BOUND changed from
*                     length 4 to allow actual array argument of size 1 when
*                     called from GKSGWK (S140 - Salford).
*     20/01/87  ARG   IS conversion. Error number changed.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP   ISGLST Segment List ( Heap Index )
*     INP   INAME  Segment Name
*     INP   PRI    Segment Priority
*     INP   BOUND  Bounding Box
*
      INTEGER ISGLST, INAME
      REAL PRI, BOUND(*)
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
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     ILOER  Pointer to an entry of lower or equal priority
*     IHIER  Pointer to an entry of higher or equal priority
*     IBLOCK Heap index to new block
*     IPRI   Internal form for priority
*     IPOINT "Pointer" to entry
*     IBBOX  Boundary box flag
*     ITYPE  Specific field required in entry
*     IHEAP  Heap pointer
*     IOFF   Entry number in block (0:KMXSL-1)
*     IADBLK Heap address of current block
*     IADSGL Heap address of master block
*     IADDRI Statement function to return the address of an integer item in the
*            current entry (current entry defined by IHEAP and IOFF)
*     IADDRR Statement function to return the address of an real item in the
*            current entry (current entry defined by IHEAP and IOFF)
*
      INTEGER IPRI, IPOINT, IBBOX, IHEAP, IOFF, IADDRI, IADDRR
      INTEGER IADBLK, IADSGL, ITYPE, ILOER, IHIER, IBLOCK
*
*  HEAP USAGE
*  ----------
*     Accessed directly
*
*  ERRORS
*  ------
*     -2011  Entry already exists
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      IADDRI(ITYPE) = KHPXI(IHEAP) + IOFF*KIENSZ+ITYPE
      IADDRR(ITYPE) = KHPXR(IHEAP) + IOFF*KRENSZ+ITYPE

*     Convert Priority

      IPRI = INT(PRI * QPRISC)

*     Is there a bounding box

      IF (KHP(KHPXI(ISGLST)+KBBXPT).EQ.KNIL) THEN
         IBBOX = KNOBOX
      ELSE
         IBBOX = KBOX
      END IF

*     Find Entry - Must not exist

      CALL GKSLFN(ISGLST, INAME, IPOINT)

      IF (IPOINT .NE. KNIL) THEN
         CALL GKBUG (-2011, 'GKSLPU')
      ELSE

*        Get free entry

         IPOINT = KHP(KHPXI(ISGLST)+KFRPTR)
         IF (IPOINT .EQ. KNIL) THEN

*           Create a Block

            CALL GKSLNB(IBBOX, KMBKSZ, IBLOCK)
            IF (KERROR .NE. 0) GO TO 999
            IADSGL = KHPXI(ISGLST)
            IADBLK = KHPXI(IBLOCK)

*           Free Chain Pointer

            KHP(IADSGL+KFRPTR) = IBLOCK * KMXSL

*           Chain Blocks together- goes in as second block

            KHP(IADBLK+KNXBLK) = KHP(IADSGL+KNXBLK)
            KHP(IADSGL+KNXBLK) = IBLOCK

            IPOINT = KHP(IADSGL+KFRPTR)

         ELSE
            IADSGL = KHPXI(ISGLST)
         END IF

            IHEAP = IPOINT / KMXSL
            IOFF  = MOD(IPOINT, KMXSL)
            KHP(IADSGL+KFRPTR) = KHP(IADDRI(KHIGHP))
            CALL GKSLSP(ISGLST, IPRI, ILOER, IHIER)
            CALL GKSLLK(ISGLST, ILOER,IPOINT)
            CALL GKSLLK(ISGLST, IPOINT, IHIER)
            KHP(IADDRI(KNAME)) = INAME
            KHP(IADDRI(KPRI )) = IPRI

            IF (IBBOX .EQ. KBOX) THEN
               IHEAP = KHP(KHPXI(IHEAP) + KBBXPT)
               QHP(IADDRR(KBOXXL)) = BOUND(1)
               QHP(IADDRR(KBOXXR)) = BOUND(2)
               QHP(IADDRR(KBOXYB)) = BOUND(3)
               QHP(IADDRR(KBOXYT)) = BOUND(4)
            END IF

*        Store as current

         KHP(IADSGL+KCUPTR) = IPOINT

      END IF
  999 CONTINUE
      END
