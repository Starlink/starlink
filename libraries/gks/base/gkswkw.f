C# IL>=a, OL>=0
      SUBROUTINE GKSWKW
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set workstation window.
*
*  MAINTENANCE LOG
*  ---------------
*     07/11/83  AS    Original version stabilized
*      27/3/84  JRG   If display surface empty, make change immediately
*                     (bug S36)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
*
*---------------------------------------------------------------------


* Set up requested window
      QRWWXL(KWKIX) = QWR1
      QRWWXR(KWKIX) = QWR2
      QRWWYB(KWKIX) = QWR3
      QRWWYT(KWKIX) = QWR4
*   If display surface is empty, move 'requested' to 'current' immediately,
*   else we need implicit regeneration (now or later)
      IF( KDSMT(KWKIX).EQ.GEMPTY ) THEN
        QCWWXL(KWKIX)=QWR1
        QCWWXR(KWKIX)=QWR2
        QCWWYB(KWKIX)=QWR3
        QCWWYT(KWKIX)=QWR4
      ELSE
* If implicit regeneration allowed, set regeneration needed, else set
* new frame action needed at update
        IF (KIMRGM(KWKIX) .EQ. GALLOW) THEN
           KRGN = .TRUE.
           KWRGN(KWKIX) = .TRUE.
        ELSE
           KNFAUP(KWKIX) = GYES
        ENDIF
        KWKTUP(KWKIX) = GPEND
* Transformation gets reset on regenerate
      ENDIF

      END
