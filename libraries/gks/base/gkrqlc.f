C# IL>=a, OL>=0
      SUBROUTINE GKRQLC(ICURS,XNDC,YNDC)
*
* (C) COPYRIGHT SERC  1988
*

*---------------------------------------------------------------------
*
*  RAL GKS SYSTEM
*
*  Type of routine:     Workstation utility
*  Author:              RMK
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To handle Request Locator.
*
*  MAINTENANCE LOG
*  ---------------
*     23/06/88  RMK   Original version stabilized.
*     21/11/89  RMK   Changed to pass INTA and REALA down to driver
*                     cursor routine. Reduced the size of these arrays.
*
*  ARGUMENTS
*  ---------
*     INP     ICURS  Workstation driver cursor input routine
*     IN/OUT  XNDC   X coord of cursor position
*     IN/OUT  YNDC   Y coord of cursor position
*
      REAL XNDC, YNDC
*
*  EXTERNALS
*  ---------
      EXTERNAL ICURS
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*     ICTRLZ    ASCII code for CTRL-Z
*     ICODE     ASCII code of key hit
*     INTA      Array to hold integer part of locator device state
*     REALA     Array to hold real part of locator device state
*     XDC, YDC  Cursor position in device coordinates
*
      INTEGER ICTRLZ, ICODE, INTA(6)
      PARAMETER (ICTRLZ=26)
      REAL REALA(6), XDC, YDC
*
*  ERRORS
*  ------
*
*
*---------------------------------------------------------------------

*   Get locator device information
      CALL GKILC(XNDC,YNDC, INTA,REALA, XDC,YDC)
      IF( KERROR.NE.0 ) GOTO 9999

   50 CONTINUE
*     Read key hit and cursor
      CALL ICURS(ICODE, XDC,YDC, INTA, REALA)
      IF (ICODE.EQ.ICTRLZ) THEN
*        Break is CTRL-Z
         KWI1 = GNONE
         XNDC = QNIL
         YNDC = QNIL
      ELSE
*        Valid character, transform coordinates
         CALL GKTDN(1, XDC, YDC, XNDC, YNDC)
*        If point is not within wkstn window, try again
         IF (KERROR.NE.0) THEN
            KERROR=0
            GOTO 50
         ELSE
            KWI1=GOK
         ENDIF
      ENDIF

 9999 CONTINUE
      END
