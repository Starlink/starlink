C# IL>=a, OL>=0
      SUBROUTINE GKSLNM(ISGLST, ISPEC, INAME)
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
*     Workstation Segment List Utility : Change name of Segment
*
*  MAINTENANCE LOG
*  ---------------
*     04/08/83  CJW   Original version stabilized.
*     20/01/87  ARG   IS conversion. Error number changed.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP   ISGLST Segment List ( Heap Index )
*     INP   ISPEC  Segment Specification
*     INP   INAME  Segment Name
*
      INTEGER ISGLST, ISPEC, INAME
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
*     IPOINT Pointer to an entry
*     ITYPE  Specific field required in entry
*     IHEAP  Heap pointer
*     IOFF   Entry number in block (0:KMXSL-1)
*     IADDRI Statement function to return the address of an integer item in the
*            current entry (current entry defined by IHEAP and IOFF)
*
      INTEGER IPOINT, ITYPE, IHEAP, IOFF, IADDRI
*
*  HEAP USAGE
*  ----------
*      Accessed
*
*  ERRORS
*  ------
*     -2007  Entry not found in list
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      IADDRI(ITYPE) = KHPXI(IHEAP) + IOFF*KIENSZ+ITYPE

*     Find Entry - Must exist

      CALL GKSLFN(ISGLST, ISPEC, IPOINT)

      IF (IPOINT .EQ. KNIL) THEN
         CALL GKBUG (-2007, 'GKSLNM')
      ELSE
            IHEAP = IPOINT / KMXSL
            IOFF  = MOD(IPOINT, KMXSL)
            KHP(IADDRI(KNAME)) = INAME
      END IF

      END
