C# IL>=a, OL>=0
      SUBROUTINE GKDPLB
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation Utility
*  Author:             PGLS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Derive polyline data according to ASF settings
*
*  MAINTENANCE LOG
*  ---------------
*     20/01/83  PGLS  Change to use fixed size bundles
*     16/03/83  PGLS  Update for new workstation aspect scheme.
*                     Data now comes from WCA, not arguments.
*     24/03/83  PGLS  Change to new WKD defns (DMN 98).
*     20/02/84  JRG   Set QWOLDX/Y so that linetype pattern does not
*                     continue (correcting bug I41)
*     19/01/87  RMK   IS conversion. Changed use of "-1" to KNIL.
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    Aspect values and ASFs
*     Read   /PLB/    Get data from bundle
*     Modify /WKD/    Set up polyline derived data
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkplb.cmn'
*
*  LOCALS
*  ------
*     I   Bundle table index
*
      INTEGER I
*
*  ALGORITHM
*  ---------
*     Always looks for the bundle (could be optimised so that if all
*     ASFs are individual, don't). If a bundle for this index isn't
*     found, use bundle 1.
*
*---------------------------------------------------------------------


* Find polyline bundle for this index & workstation
      DO 10 I=1, KMXPLB
        IF (KPLI(I,KWKIX) .EQ. KIPLI) GOTO 20
        IF (KPLI(I,KWKIX) .EQ. KNIL) GOTO 15
   10 CONTINUE
*     Not found: use bundle 1
   15 CONTINUE
      I = 1
   20 CONTINUE
      IF (KIPLAF(1) .EQ. GBUNDL) THEN
         KWLNTY(KWKIX) = KLNTY(I,KWKIX)
      ELSE
         KWLNTY(KWKIX) = KILNTY
      ENDIF
      IF (KIPLAF(2) .EQ. GBUNDL) THEN
         QWLNWD(KWKIX) = QLNWD(I,KWKIX)
      ELSE
         QWLNWD(KWKIX) = QILNWD
      ENDIF
      IF (KIPLAF(3) .EQ. GBUNDL) THEN
         KWPLCI(KWKIX) = KPLCI(I,KWKIX)
      ELSE
         KWPLCI(KWKIX) = KIPLCI
      ENDIF

*     Change any related data needed by utilities (pattern phase)
      QWOLDX(KWKIX)=-999.0
      QWOLDY(KWKIX)=-999.0
      END
