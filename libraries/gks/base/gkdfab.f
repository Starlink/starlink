C# IL>=a, OL>=0
      SUBROUTINE GKDFAB
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
*     Derive fill area data according to ASF settings
*
*  MAINTENANCE LOG
*  ---------------
*     20/01/83  PGLS  Change to use fixed size bundles
*     16/03/83  PGLS  Update for new workstation aspect scheme.
*                     Data now comes from WCA, not arguments.
*     24/03/83  PGLS  Change to new WKD defns (DMN 98).
*     18/04/83  AS    Change IFAIX to KIFAI
*     30/11/83  AS    Add pattern stuff
*     19/01/87  RMK   IS conversion. Changed use of "-1" to KNIL.
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    Aspect values and ASFs
*     Read   /FAB/    Get data from bundle
*     Modify /WKD/    Set up fill area derived data
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkfab.cmn'
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


* Find fill area bundle for this index & workstation
      DO 10 I=1, KMXFAB
        IF (KFAI(I,KWKIX) .EQ. KIFAI) GOTO 20
        IF (KFAI(I,KWKIX) .EQ. KNIL) GOTO 15
   10 CONTINUE
*     Not found: use bundle 1
   15 CONTINUE
      I = 1
   20 CONTINUE
      IF (KIFAAF(1) .EQ. GBUNDL) THEN
         KWFAIS(KWKIX) = KIS(I,KWKIX)
      ELSE
         KWFAIS(KWKIX) = KIFAIS
      ENDIF
      IF (KIFAAF(2) .EQ. GBUNDL) THEN
         KWFASI(KWKIX) = KSI(I,KWKIX)
      ELSE
         KWFASI(KWKIX) = KIFASI
      ENDIF
      IF (KIFAAF(3) .EQ. GBUNDL) THEN
         KWFACI(KWKIX) = KFACI(I,KWKIX)
      ELSE
         KWFACI(KWKIX) = KIFACI
      ENDIF

      CALL GKTWDV (QIPAHX,QIPAHY,QWPAHX(KWKIX),QWPAHY(KWKIX))
      CALL GKTWDV (QIPAWX,QIPAWY,QWPAWX(KWKIX),QWPAWY(KWKIX))
      CALL GKTWD (1,QIPAX,QIPAY,QWPAX(KWKIX),QWPAY(KWKIX))

      END
