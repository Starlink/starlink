C# IL>=a, OL>=0
      SUBROUTINE GKSLBF(ISGLST, IBOUND)
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Workstation Segment List Utility : Get Bounding box flag
*
*  MAINTENANCE LOG
*  ---------------
*
*  ARGUMENTS
*  ---------
*     INP   ISGLST Segment List ( Heap Index )
*     OUT   IBOUND Bounding Box Flag
*
      INTEGER ISGLST, IBOUND
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read  /GKHP/     Heap
*
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
*
*  LOCALS
*  ------
*     None
*
*  HEAP USAGE
*  ----------
*     Accessed
*
*---------------------------------------------------------------------
*
      IF (KHP(KHPXI(ISGLST)+KBBXPT).NE.KNIL) THEN
         IBOUND = KBOX
      ELSE
         IBOUND = KNOBOX
      ENDIF
      END
