C# IL>=a, OL>=0
      SUBROUTINE GKSLDL(ISGLST)
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
*     Workstation Segment List Utility : Delete Segment List
*
*  MAINTENANCE LOG
*  ---------------
*     04/08/83  CJW   Original version stabilized
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP   ISGLST Segment List ( Heap Index )
*
      INTEGER ISGLST
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKERR/    KERROR
*     Modify /GKHP/     Heap
*
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     ISG    Heap index to current block
*
      INTEGER ISG
*
*  HEAP USAGE
*  ----------
*      Allocation Deleted
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

*     While ISGLST <> KNIL do
    1 CONTINUE
      IF (ISGLST .EQ. KNIL) GO TO 2

*        Delete Bounding Box if it exists

         IF (KHP(KHPXI(ISGLST)+KBBXPT) .NE. KNIL) THEN
            CALL GKHPDA(KHP(KHPXI(ISGLST)+KBBXPT), KREALS)
            IF (KERROR .NE. 0) GO TO 999
         END IF

*        Delete Integer Part

         ISG = ISGLST
         ISGLST = KHP(KHPXI(ISGLST)+KNXBLK)
         CALL GKHPDA(ISG, KINTGS)
         IF (KERROR .NE. 0) GO TO 999

      GO TO 1
    2 CONTINUE
*     End While

  999 CONTINUE
      END
