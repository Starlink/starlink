*---------------------------------------------------------------------


      SUBROUTINE GK0XDC( ICOL, NCOL, I, R, G, B)

*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the default colour for the given colour index
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP ICOL   - Whether device is colour
*     INP NCOL   - Number of colour table entries on the device
*     INP I      - Colour index
*     OUTP R,G,B - Selected colour
*
      INTEGER ICOL, NCOL, I
      REAL R, G, B
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/GKS_PAR'
*
*  LOCALS
*  ------
      REAL RDEF(2:7), GDEF(2:7), BDEF(2:7)
      DATA RDEF /1.0, 0.0, 0.0, 1.0, 1.0, 0.0 /
      DATA GDEF /0.0, 1.0, 0.0, 1.0, 0.0, 1.0 /
      DATA BDEF /0.0, 0.0, 1.0, 0.0, 1.0, 1.0 /
*
*  COMMENTS
*  --------
*
*     This driver is intended to support a range of X server devices so
*     it is not possible to store the default colour table in the WDT.
*     This routine calculates the default colour table on the basis of
*     the characteristics of the device.
*---------------------------------------------------------------------

*   Background is always black
      IF (I.EQ.0) THEN
         R = 0.0
         G = 0.0
         B = 0.0

*   Foreground is aways white
      ELSE IF (I.EQ.1) THEN
         R = 1.0
         G = 1.0
         B = 1.0

      ELSE

*   Monochrome device - grey scale ramp from index 2 upwards
         IF (ICOL.EQ.GNO) THEN
            R = REAL(I)/REAL(NCOL-1)
            G = R
            B = R

*   Colour - next 7 entries are primary and secondary colours followed
*   by ramp.
         ELSE
            IF (I.LT.8) THEN
               R = RDEF(I)
               G = GDEF(I)
               B = BDEF(I)
            ELSE
               R = REAL(I)/REAL(NCOL-1)
               G = R
               B = R
            END IF
         END IF
      END IF
      END
