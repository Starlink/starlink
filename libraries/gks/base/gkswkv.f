C# IL>=a, OL>=0
      SUBROUTINE GKSWKV
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
*     Set workstation viewport.
*
*  MAINTENANCE LOG
*  ---------------
*     07/11/83  AS    Original version stabilized
*      27/3/84  JRG   If display surface empty, make change immediately
*                     (bug S36)
*     03/04/86  DRJF  Check workstation viewport limits, report error 54
*                     if invalid (bug S76)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gkwsl.cmn'
*
*  ERRORS
*  ------
*     54  Workstation viewport is not within the display space
*
*---------------------------------------------------------------------


* Check if the viewport is within the display space
      IF (QWR1.LT.0.0 .OR. QWR2.GT.QDSDX(KWKIX) .OR.
     :    QWR3.LT.0.0 .OR. QWR4.GT.QDSDY(KWKIX)) THEN
            KERROR=54
            GOTO 9999
      END IF

* Set up requested viewport
      QRWVXL(KWKIX) = QWR1
      QRWVXR(KWKIX) = QWR2
      QRWVYB(KWKIX) = QWR3
      QRWVYT(KWKIX) = QWR4
*   If display surface is empty, move 'requested' to 'current' immediately,
*   else we need implicit regeneration (now or later)
      IF( KDSMT(KWKIX).EQ.GEMPTY ) THEN
        QCWVXL(KWKIX)=QWR1
        QCWVXR(KWKIX)=QWR2
        QCWVYB(KWKIX)=QWR3
        QCWVYT(KWKIX)=QWR4
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
 9999 CONTINUE
      END
