C# IL>=a, OL>=0
      SUBROUTINE GKDPMB
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
*     Derive polymarker data according to ASF settings
*
*  MAINTENANCE LOG
*  ---------------
*     20/01/83  PGLS  Change to use fixed size bundles
*     16/03/83  PGLS  Update for new workstation aspect scheme.
*                     Data now comes from WCA, not arguments.
*     24/03/83  PGLS  Change to new WKD defns (DMN 98).
*     19/01/87  RMK   IS conversion. Changed use of "-1" to KNIL.
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    Aspect values and ASFs
*     Read   /PMB/    Get data from bundle
*     Modify /WKD/    Set up polymarker derived data
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkpmb.cmn'
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
      DO 10 I=1, KMXPMB
        IF (KPMI(I,KWKIX) .EQ. KIPMI) GOTO 20
        IF (KPMI(I,KWKIX) .EQ. KNIL) GOTO 15
   10 CONTINUE
*     Not found: use bundle 1
   15 CONTINUE
      I = 1
   20 CONTINUE
      IF (KIPMAF(1) .EQ. GBUNDL) THEN
         KWMKTY(KWKIX) = KMKTY(I,KWKIX)
      ELSE
         KWMKTY(KWKIX) = KIMKTY
      ENDIF
      IF (KIPMAF(2) .EQ. GBUNDL) THEN
         QWMKSZ(KWKIX) = QMKSZ(I,KWKIX)
      ELSE
         QWMKSZ(KWKIX) = QIMKSZ
      ENDIF
      IF (KIPMAF(3) .EQ. GBUNDL) THEN
         KWPMCI(KWKIX) = KPMCI(I,KWKIX)
      ELSE
         KWPMCI(KWKIX) = KIPMCI
      ENDIF
      END
