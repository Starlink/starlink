C# IL>=a, OL>=0
      SUBROUTINE GKSLPR(ISGLST, ISPEC, PRI)
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
*     Workstation Segment List Utility : Change Segment Priority
*
*  MAINTENANCE LOG
*  ---------------
*     04/08/83  CJW   Original version stabilized.
*     20/01/87  ARG   IS conversion. Error number changed.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*     06/08/90  KEVP  Removed unused local variable (S342).
*
*  ARGUMENTS
*  ---------
*     INP   ISGLST Segment List ( Heap Index )
*     INP   ISPEC  Segment Specification
*     INP   PRI    Segment Priority
*
      INTEGER ISGLST,  ISPEC
      REAL PRI
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKHP/     Heap
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkhp.cmn'
*
*  LOCALS
*  ------
*     IPRI   Internal form of priority
*     IPOINT Pointer to an entry
*     ITYPE  Specific field required in entry
*     IHEAP  Heap pointer
*     IOFF   Entry number in block (0:KMXSL-1)
*     IADDRI Statement function to return the address of an integer item in the
*            current entry (current entry defined by IHEAP and IOFF)
*     IADDRR Statement function to return the address of an real item in the
*            current entry (current entry defined by IHEAP and IOFF)
*     ILOER  Pointer to an entry of lower or equal priority
*     IHIER  Pointer to an entry of higher or equal priority
*
      INTEGER IPRI, IPOINT, ITYPE, IHEAP, IOFF, IADDRI,
     :        ILOER, IHIER
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

      IADDRI(ITYPE) = KHPXI(IHEAP) + IOFF*KIENSZ + ITYPE

*     Convert Priority

      IPRI = INT(PRI * QPRISC)

*     Find Entry - Must exist

      CALL GKSLFN(ISGLST, ISPEC, IPOINT)

      IF (IPOINT .EQ. KNIL) THEN
         CALL GKBUG (-2007, 'GKSLPR')
      ELSE
         IHEAP = IPOINT / KMXSL
         IOFF  = MOD(IPOINT, KMXSL)

*        Chain other entries around it

         CALL GKSLLK(ISGLST, KHP(IADDRI(KLOWP)),KHP(IADDRI(KHIGHP)))

*        Store the priority

         KHP(IADDRI(KPRI )) = IPRI

*        Chain it in

         CALL GKSLSP(ISGLST, IPRI, ILOER, IHIER)
         CALL GKSLLK(ISGLST, ILOER,IPOINT)
         CALL GKSLLK(ISGLST, IPOINT, IHIER)

      END IF

      END
