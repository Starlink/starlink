      SUBROUTINE GK2MBS(INTIN,CHAOUT,BYTES)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
      INCLUDE '../../include/check.inc'
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Converts Integers to Hex in Bitstream format.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*        INTIN  : Input integer to be converted
*        CHAOUT : Output string (HEX characters)
*        BYTES  : No of bytes necessary to hold bitstream value

*
*  LOCALS
*  ------
*
*        TEMP   : Temporary variable
*        N      : Loop counter
*        I      : Loop counter
*        HEXOUT : Output integer (in HEX)
*        BIT    : Array containing powers of 2

      INTEGER INTIN, TEMP, N, I, BYTES
      INTEGER HEXOUT(8), BIT(8)
      CHARACTER CHAOUT*9
      DATA BIT/1,2,4,8,16,32,64,128/

*---------------------------------------------------------------------

*   Set output array to 0
      DO 5 N=1,8
         HEXOUT(N)=0
 5    CONTINUE

*   Break the integer into 6 bit chunks
      DO 12 N = BYTES,1,-1
         TEMP = INTIN/BIT(7)
         TEMP = INTIN - (TEMP * BIT(7))
         INTIN = INTIN/BIT(7)
         TEMP = TEMP +BIT(7)
         HEXOUT(N) = TEMP
  12  CONTINUE

*   Convert each hex digit to HEX character form
      N=1
      DO 30 I = 1,8
         IF(HEXOUT(I).NE.0) THEN
            CHAOUT(N:N)=CHAR(HEXOUT(I))
            N=N+1
         ENDIF
  30  CONTINUE
      CHAOUT(N:N)=CHAR(0)
      RETURN
      END
