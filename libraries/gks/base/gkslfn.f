C# IL>=a, OL>=0
      SUBROUTINE GKSLFN(ISGLST, ISPEC, IPOINT)
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
*     Workstation Segment List Internal Utility : Find Segment
*
*  MAINTENANCE LOG
*  ---------------
*     04/08/83  CJW   Original version stabilized
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*     06/08/90  KEVP  Removed unused local variable (S342).
*
*  ARGUMENTS
*  ---------
*     INP   ISGLST Segment List ( Heap Index )
*     INP   ISPEC  Segment Specification
*     OUT   IPOINT Pointer to an entry
*
      INTEGER ISGLST, ISPEC, IPOINT
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

      IF (ISPEC .EQ. KHIEST) THEN
         IPOINT = KHP(KHPXI(ISGLST)+KHIPTR)
      ELSE IF (ISPEC .EQ. KLOEST ) THEN
         IPOINT = KHP(KHPXI(ISGLST)+KLOPTR)
      ELSE IF (ISPEC .LT. 0) THEN

*        Must be KCURR, KHIER or KLOER

         IPOINT = KHP(KHPXI(ISGLST)+KCUPTR)
         IF (IPOINT .NE. KNIL) THEN
            IHEAP = IPOINT / KMXSL
            IOFF  = MOD(IPOINT, KMXSL)
            IF (ISPEC .EQ. KHIER) THEN
               IPOINT = KHP(IADDRI(KHIGHP))
            ELSE IF (ISPEC .EQ. KLOER) THEN
               IPOINT = KHP(IADDRI(KLOWP))
            END IF
         END IF
      ELSE

*        A name - look for it!

         IPOINT = KHP(KHPXI(ISGLST)+KLOPTR)
         IHEAP = IPOINT / KMXSL
         IOFF  = MOD( IPOINT, KMXSL)

*        While IPOINT <> KNIL and IPOINT(name) <> ISPEC do
    1    CONTINUE
         IF (IPOINT .EQ. KNIL) GO TO 2
         IF (KHP(IADDRI(KNAME)) .EQ. ISPEC) GO TO 2

            IPOINT = KHP(IADDRI(KHIGHP))
            IHEAP = IPOINT / KMXSL
            IOFF  = MOD(IPOINT, KMXSL)

         GO TO 1
    2    CONTINUE
*        End While

      END IF

*     Store as current

      KHP(KHPXI(ISGLST)+KCUPTR) = IPOINT

      END
