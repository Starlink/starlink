C# IL>=a, OL>=0
      SUBROUTINE GKIOBG
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             CJW / AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Initialize IO system.
*
*  MAINTENANCE LOG
*  ---------------
*     13/06/83  CJW  Original version stabilized
*     27/06/83  CJW  Implement revised error handling precedure
*                    (No change required)
*
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKFLS/    KEMFLS, KWDFLS
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkfls.cmn'
*
*---------------------------------------------------------------------

      KEMFLS = KFLCL
      KWDFLS = KFLCL
      KDBFLS = KFLCL
      KCSFLS = KFLCL

      END
