C# IL>=a, OL>=0
      SUBROUTINE GKTOLD
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front End
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To indicate that the transformation last received by the
*     active workstations is now out of date and by implication
*     the text and fill area attributes (at least those which are
*     set by the user in World Coordinates) are also out of date.
*
*  MAINTENANCE LOG
*  ---------------
*    16/3/83   First version
*   30/3/83     CHECK.INC included
*    8/4/83     GKSL.PAR included
*
*  ARGUMENTS
*  ---------
*     None
*
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/ Flags in GKS State List
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gksl.cmn'
*
*---------------------------------------------------------------------


*   Change the entries
      KSTRWK=KHANGE
      KSTXWK=KHANGE
      KSFAWK=KHANGE

      END
