C# IL>=a, OL>=0
      SUBROUTINE GKSLCR(IBOUND, ISGLST)
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
*     Workstation Segment List Utility : Create Segment List
*
*  MAINTENANCE LOG
*  ---------------
*     04/08/83  CJW   Original version stabilized
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP   IBOUND Bounding Box Flag
*     OUT   ISGLST Segment List ( Heap Index )
*
      INTEGER IBOUND, ISGLST
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
*     IADR   Address of Heap item
*
      INTEGER IADR
*
*  HEAP USAGE
*  ----------
*      Accessed directly
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

*     Create First Block

      CALL GKSLNB(IBOUND, KMBKSZ, ISGLST)
      IF (KERROR .NE. 0) GO TO 999
      IADR = KHPXI(ISGLST)

*     Free Chain Pointer

      KHP(IADR+KFRPTR) = ISGLST * KMXSL

*     Set Default values for Internal Pointers

      KHP(IADR+KNXBLK) = KNIL
      KHP(IADR+KHIPTR) = KNIL
      KHP(IADR+KLOPTR) = KNIL
      KHP(IADR+KCUPTR) = KNIL

  999 CONTINUE
      END
