C# IL>=a, OL>=0
      SUBROUTINE GKXFD
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
*     get the font details, if necessary makes font index the current
*     font
*
*  MAINTENANCE LOG
*  ---------------
*     08/11/83  PB    Original version stabilized
*     30/11/83  AS    Initialise database if necessary
*     07/12/83  AS    Change read to CHARBF(1:196)
*     12/12/83  AS    Put level of indirection for gks font index to
*                     database font index
*     13/12/83  CJW   Change Statement numbers so that it will compile!
*     23/12/83  AS    Fix initialisation of database
*     12/01/84  PMB   Fix font height to correspond to corrected
*                     database.
*     01/02/84  PMB   initialise QFHT to 1.0 for SW chars
*     16/04/86  RMK   Changed to use GKNA1 instead of ICHAR (S103).
*     22/01/87  JCS   IS conversion. Error number changed.
*     13/02/87  PKY   Changed to accomodate the fact that CLWDTH has
*                     changed from CHARACTER*(KMXICN) CLWDTH to
*                     CHARACTER*1 CLWDTH(KMXICN). CRWDTH changed in
*                     the same way (see GKXFD.CMN).
*     11/06/87  RMK   Merged GKS-UK and RAL versions of this routine.
*     13/11/87  RMK   Corrected error reporting to set -1009 (S277).
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gkxfd.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  EXTERNAL FUNCTION DEFINITIONS
*  -----------------------------
*
      INTEGER GKNA1
*
*  LOCALS
*  ------
*     I      Loop index
*     ILEFT
*     IRIGHT
*
      INTEGER I,ILEFT,IRIGHT,ITOPM,IBASEM,IRITM,ILEFTM,IFHT,IBOTG,ISTAT,
     :        INDEXF
*
*-----------------------------------------------------------------------


*     if font index not in the Cache
*     then copy into the Cache
*     else read from the Cache

* Initialise database if not already open
      IF (KDBFLS.EQ.KFLNA) THEN
        KERROR = -1009
      ELSE
        IF (KDBFLS.EQ.KFLCL) CALL GKXON
        IF (KERROR.EQ.0) THEN

* Find compact font index
          DO 1 I=1,KFNTMX
            IF (KWTXFN(KWKIX).EQ.KHFONT(I)) THEN
              INDEXF = I
              GOTO 2
            ENDIF
    1     CONTINUE
          INDEXF = 1
    2     CONTINUE

          IF (KFNAMS(INDEXF).EQ.0) THEN
            READ(UNIT = KDBFLU,REC = KFRECS(INDEXF)) CHARBF(1:196)
            CALL GKXCPU(CHARBF(1:196),KFNAMS(INDEXF),ISTAT)
            IF (ISTAT.NE.0)
     :      CALL GKXCPU(CHARBF(1:196),KFNAMS(INDEXF),ISTAT)
          ELSE
            CALL GKXCGE(KFNAMS(INDEXF),CHARBF)
          ENDIF

*       unpack font index into common area

          KURFON = KWTXFN(KWKIX)
          IFHT   = GKNA1(CHARBF(1:1)) + KFMARK
          ITOPM  = GKNA1(CHARBF(2:2)) + KFMARK
          IBASEM = GKNA1(CHARBF(3:3)) + KFMARK
          ILEFTM = GKNA1(CHARBF(4:4)) + KFMARK
          IRITM  = GKNA1(CHARBF(5:5)) + KFMARK
          IBOTG  = GKNA1(CHARBF(6:6)) + KFMARK

          DO 3 I = 1,KCHTOT
            KFONT(I) = GKNA1(CHARBF(I*2+5:I*2+5))*128
     :                 +GKNA1(CHARBF(I*2+6:I*2+6))
    3     CONTINUE

* calculate details from current font

          KFHGT = ITOPM-IBASEM
          QFHT = 1.0
          QFWDMX = REAL(IRITM-ILEFTM)/REAL(KFHGT)
          QFYADJ = -(ITOPM+IBASEM)/2.0
          QFCAP = REAL(IFHT+IBOTG-ITOPM)/REAL(KFHGT)
          QFBASE = REAL(IBASEM-IBOTG)/REAL(KFHGT)
          DO 4 I = 1,KCHTOT
             ILEFT = GKNA1(CLWDTH(KFONT(I))) + KFMARK
             IRIGHT = GKNA1(CRWDTH(KFONT(I))) + KFMARK
             QFWIDS(I) = REAL(IRIGHT-ILEFT)/REAL(KFHGT)
   4      CONTINUE
        ENDIF
      ENDIF

      END
