C# IL>=a, OL>=0
      SUBROUTINE GKCPLG
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
*     To copy polyline attributes from GKS State List to Workstation
*     Communication Area.
*
*  MAINTENANCE LOG
*  ---------------
*     16/03/83   JRG  First version
*     30/3/83    JRG  CHECK.INC included
*     20/4/83    AS   Set up KIPLCI
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYSL/  Polyline attributes
*     Modify /GKYWCA/ Attributes to W.C.A.
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*     J     Loop counter
*
      INTEGER J
*
*---------------------------------------------------------------------


      KIPLI  = KCPLI
      KILNTY = KCLNTY
      QILNWD = QCLNWD
      KIPLCI = KCPLCI
      DO 300 J=1,3
 300    KIPLAF(J) = KCPLAF(J)
      END
