*---------------------------------------------------------------------

      SUBROUTINE GK0XCS(ICODE,X,Y)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             DLT
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Read cursor position and return key pressed
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     OUT X,Y    - coordinates of cursor
*     OUT ICODE  - ASCII code of key pressed
*
      REAL    X(1), Y(1)
      INTEGER ICODE
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  Offsets in QWKDAT workspace
*
      INTEGER  XSCALE, YSCALE
      PARAMETER (XSCALE = 1, YSCALE = 2)
*
*  LOCALS
*  ------
      INTEGER ISP, ICR, IEOF
      PARAMETER (ISP = 32, ICR = 13, IEOF = 26)
      REAL XDC(1), YDC(1)

*  Transform initial position to pixels
      CALL GKTND(1,QWKDAT(3,KWKIX),QWKDAT(4,KWKIX),XDC,YDC)
      XDC(1) = XDC(1) * QWKDAT(XSCALE,KWKIX)
      YDC(1) = YDC(1) * QWKDAT(YSCALE,KWKIX)

*  Call input routine specifying that the cursor should be displayed
      CALL GK0XRL(KWKIX, GYES, XDC(1), YDC(1), X(1), Y(1), ICODE)

*  Convert position back to DC (metres)
      X(1) = X(1) / QWKDAT(XSCALE,KWKIX)
      Y(1) = Y(1) / QWKDAT(YSCALE,KWKIX)

*  Convert mouse button presses to keyboard codes
      IF (ICODE.LT.0) THEN
         IF (ICODE.EQ.-3) THEN
            ICODE = IEOF
         ELSE IF (ICODE.EQ.-2) THEN
            ICODE = ICR
         ELSE
            ICODE = ISP
         END IF
      END IF
      END
