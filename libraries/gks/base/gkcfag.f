C# IL>=a, OL>=0
      SUBROUTINE GKCFAG
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front End
*  Author:             PGLS
*
      INCLUDE '../include/check.inc'
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To copy fill area attributes from GKS State List to Workstation
*     Communication Area.
*
*  MAINTENANCE LOG
*  ---------------
*     20/04/83  PGLS  Initial version
*     30/11/83  AS    Add pattern stuff
*     19/01/87  DCS   IS conversion. Set pattern vectors from new GKS
*                     State List entries.
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYSL/  Fill Area attributes
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


      KIFAI=KCFAI
      KIFAIS=KCFAIS
      KIFASI=KCFASI
      KIFACI=KCFACI
      QIPAHX=QCPAHX
      QIPAHY=QCPAHY
      QIPAWX=QCPAWX
      QIPAWY=QCPAWY
      QIPAX=QCPAX
      QIPAY=QCPAY
      DO 300 J=1,3
        KIFAAF(J)=KCFAAF(J)
  300 CONTINUE

      END
