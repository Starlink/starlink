C# IL>=a, OL>=0
      SUBROUTINE GKCPMG
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
*     To copy polymarker attributes from GKS State List to Workstation
*     Communication Area.
*
*  MAINTENANCE LOG
*  ---------------
*    16 March 1983  1st version
*   30/3/83     CHECK.INC included
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYSL/  Polymarker attributes
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


      KIPMI=KCPMI
      KIMKTY=KCMKTY
      QIMKSZ=QCMKSZ
      KIPMCI=KCPMCI
      DO 300 J=1,3
 300    KIPMAF(J)=KCPMAF(J)
      END
