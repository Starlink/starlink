C# IL>=b, OL>=0
      SUBROUTINE GKILC(XNDC,YNDC, INTA,REALA, XDC,YDC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation utility
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To obtain the state of the specified locator device
*
*  MAINTENANCE LOG
*  ---------------
*     29/06/84  JRG   First version
*     20/08/85  RMK   XNDC,YNDC changed from integer to real (S149 - Salford).
*
*  ARGUMENTS
*  ---------
*     INP  XNDC,YNDC  Arrays of length 1 holding initial locator
*                  position in NDC
*     OUT  INTA    Array to hold INTEGER part of device state (length 5)
*     OUT  REALA   Array to hold REAL part of device state (length 6)
*     OUT  XDC,YDC Arrays of length 1 holding initial position in DC
*
      INTEGER INTA(5)
      REAL REALA(6),XDC(1),YDC(1),XNDC(1),YNDC(1)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    For KWKIX to identify workstation and device number
*     Read   /WSL/    For workstation window
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     None
*
*-----------------------------------------------------------------------


* Get locator device information
      CALL GKRQIP(GLOCAT,KWI1,5,6,INTA,REALA)
      IF( KERROR.NE.0 ) GOTO 999
* Check initial locator position in NDC is within workstation window
      IF (XNDC(1).LT.QCWWXL(KWKIX) .OR. XNDC(1).GT.QCWWXR(KWKIX) .OR.
     :      YNDC(1).LT.QCWWYB(KWKIX) .OR. YNDC(1).GT.QCWWYT(KWKIX)) THEN
* Set suitable defaults
        XNDC(1) = QCWWXL(KWKIX)
        YNDC(1) = QCWWYB(KWKIX)
      ENDIF
* Transform initial locator position from NDC to device coordinates
      CALL GKTND(1,XNDC,YNDC,XDC,YDC)

  999 CONTINUE
      END
