      SUBROUTINE GK9SCS(ICODE, XDC,YDC, INTA, REALA)

*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (part of) Workstation Driver
*  Author:             RMK, based on GK0TCS by JRG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To read key hit and cursor position from Sun
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     OUT  ICODE   Ascii value of key hit
*     I/O  XDC(1)  } Cursor position in
*     I/O  YDC(1)  } device coordinates (real)
*     INP  INTA     } Arrays holding integer and real
*     INP  REALA    } parts of locator device state.
*
      INTEGER ICODE, INTA(6)
      REAL XDC(1),YDC(1), REALA(6)
*
*  LOCALS
*  ------
*
      INTEGER ISTAT
*
*  EXTERNAL FUNCTIONS
*  ------------------

      INTEGER GK9SCI

*  ERRORS
*  ------
*
*-----------------------------------------------------------------------

      ISTAT = GK9SCI(ICODE, XDC(1), YDC(1), INTA, REALA)

      END
