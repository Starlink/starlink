C# IL>=a, OL=0
      SUBROUTINE GKCTXG
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    FRONTEND
*  Author:             FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To copy text attributes from GKS State List to Workstation
*     Communication Area.
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  FY    Original version stabilized
*     05/05/83  FY    renamed from GKSTXG
*     19/02/86  DCS   Update 'last source' flag in GTX rather than here
*                     for consistency with other primitives.
*     19/01/87  DCS   IS conversion. Set height and width vectors
*                     independently from GKS State List entries.
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /KYSL/   Text attributes
*     Modify /KYWCA/  Attributes to W.C.A.
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*     RL       Length of vector
*     I       Loop counter
*
      REAL    RL
      INTEGER I
*
*  ALGORITHM
*  ----------
*     height and width vectors are calculated as
*     described in Gks Implementation Note 28.
*
*---------------------------------------------------------------------



* height vector
      RL = SQRT(QCCHUX*QCCHUX + QCCHUY*QCCHUY)
      QICHHX = (QCCHUX/RL)*QCCHH
      QICHHY = (QCCHUY/RL)*QCCHH

* width vector
      RL = SQRT(QCCHBX*QCCHBX + QCCHBY*QCCHBY)
      QICHWX = (QCCHBX/RL)*QCCHW
      QICHWY = (QCCHBY/RL)*QCCHW

* aspect flags
      DO 10 I = 1,4
        KITXAF(I)=KCTXAF(I)
   10 CONTINUE

* text index
      KITXI = KCTXI

* text font and precisions
      KITXFN = KCTXFN
      KITXPR = KCTXPR

* expansion factor
      QICHXP = QCCHXP

* spacing
      QICHSP = QCCHSP

* text colour index
      KITXCI = KCTXCI

* text path
      KITXP = KCCHP

* vertical & horizontal text alignment
      KIVTXA = KCVTCH
      KIHTXA = KCHZCH

      END
