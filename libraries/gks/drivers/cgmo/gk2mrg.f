      SUBROUTINE GK2MRG(REDIN,GRENIN,BLUEIN,KCOLPR)
*---------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Routine to convert RGB to hex & write the result
*  to the CGM, as ASCII characters.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*        REDIN  : Input red value in real form (actual parameter)
*        GRENIN : Input green value in real form (actual parameter)
*        BLUEIN : Input red value in real form (actual parameter)
*        KCOLPR : Colour precision (passed as a parameter)
*
      REAL REDIN, GRENIN, BLUEIN
      INTEGER KCOLPR
*
*  LOCALS
*  ------
*        RED    : Input red value in integer form
*        GREEN  : Input green value in integer form
*        BLUE   : Input blue value in integer form
*        TEMP   : Temporary variable
*        N      : Loop counter
*        I      : Loop counter
*        INTOUT : Contains the integer RGB value for bitstream
*                 conversion
*        FACTOR : Multiplication factor for conversion to integer
*        BYTES  : No of bytes necessary to hold bitstream value
*        CHAOUT : Output string (HEX characters)
*  Parameter
*        COLSAT : Maximum Colour saturation value

      INTEGER RED, GREEN, BLUE, TEMP, N, I, INTOUT, FACTOR, BYTES
      CHARACTER CHAOUT*16

      INTEGER COLSAT
      PARAMETER (COLSAT = 255)

*---------------------------------------------------------------

*   Change input values to integers (from 0 to COLSAT)
      RED=REDIN*COLSAT
      GREEN=GRENIN*COLSAT
      BLUE=BLUEIN*COLSAT

*   Find out how many bytes are necessary
      BYTES=KCOLPR/2
      TEMP=MOD(KCOLPR,2)
      IF (TEMP.EQ.1) THEN
         N=2
         BYTES=BYTES+1
      ELSE
         N=1
      ENDIF
      INTOUT=0

*   Put the 3 RGB values together as an integer & turn it into
*   Bitstream format
      DO 10 I=N, (KCOLPR+N-1)
         FACTOR = (I-1)*3

         TEMP = MOD(BLUE,2)
         IF (TEMP.EQ.1) THEN
            INTOUT=INTOUT + (2**FACTOR)
         ENDIF
         BLUE=BLUE/2

         FACTOR = FACTOR+1
         TEMP = MOD(GREEN,2)
         IF (TEMP.EQ.1) THEN
            INTOUT=INTOUT + (2**FACTOR)
         ENDIF
         GREEN=GREEN/2

         FACTOR = FACTOR+1
         TEMP = MOD(RED,2)
         IF (TEMP.EQ.1) THEN
            INTOUT=INTOUT + (2**FACTOR)
         ENDIF
         RED = RED/2

  10  CONTINUE
      CALL GK2MBS(INTOUT,CHAOUT,BYTES )

*   Write it to CGM
      CALL GK2MBU(CHAOUT)
      RETURN
      END
