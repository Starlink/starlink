C# IL>=a, OL>=0
      SUBROUTINE GQOPS ( IOP )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Inquire Operating State Value
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the GKS Operating State Value
*
*  MAINTENANCE LOG
*  ---------------
*     01/11/82  CJW  Original version stabilized
*     10/02/83  CJW  Insert call to Prologue
*     27/06/83  CJW  Implement revised error handling precedure
*
*  ARGUMENTS
*  ---------
*     OUT   IOP    Operating State Value
*
      INTEGER IOP
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkops.cmn'
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL, GGKCL, GSGOP)

      IOP = KOPS

      END
