C# IL>=a, OL>=0
      SUBROUTINE GKSLQN ( ISGLST, N, INAME )
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
*     Inquire Nth Entry
*
*  MAINTENANCE LOG
*  ---------------
*     10/08/83  CJW   Original version stabilized
*     17/01/84  CJW/JRG  Correction
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP ISGLST   Heap Index  ( = Directory Index)
*     INP N        Nth entry required
*     OUT INAME    Key of Nth entry
*
      INTEGER ISGLST, N, INAME
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
*     K      Counter
*     IPOINT Pointer to free entry
*     ITYPE  Specific field required in entry
*     IHEAP  Heap pointer
*     IOFF   Entry number in block (0:KMXSL-1)
*     IADDRI Statement function to return the address of an integer item in the
*            current entry (current entry defined by IHEAP and IOFF)
*     IADDRR Statement function to return the address of an real item in the
*            current entry (current entry defined by IHEAP and IOFF)
*
      INTEGER K, IPOINT, IHEAP, IOFF, IADDRI, IADDRR, ITYPE
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
*     Find Entry
      IPOINT = KHP(KHPXI(ISGLST)+KHIPTR)
      K = 1

*     while K < N & (have not hit the end) do
    1 CONTINUE
      IF ( (K.EQ.N) .OR. (IPOINT.EQ.KNIL)) GO TO 2

            IHEAP = IPOINT / KMXSL
            IOFF  = MOD(IPOINT, KMXSL)
            IPOINT = KHP(IADDRI(KLOWP))
            K = K+1

      GO TO 1
    2 CONTINUE
*     end while

      IF (IPOINT .EQ. KNIL) THEN
         INAME = KNIL
      ELSE
         IHEAP = IPOINT / KMXSL
         IOFF  = MOD(IPOINT, KMXSL)
         INAME = KHP(IADDRI(KNAME))
      END IF
      END IF

      END
