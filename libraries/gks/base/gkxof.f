C# IL>= a, OL>= 0
      SUBROUTINE GKXOF
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             PB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To close the font file
*
*  MAINTENANCE LOG
*  ---------------
*     14/11/83  PB    Original version stabilized
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkfls.cmn'
*
*---------------------------------------------------------------------

      CALL GKIOCL(KFDATA, KNIL, KDBFLU)

      END
