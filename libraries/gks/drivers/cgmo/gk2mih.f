      SUBROUTINE GK2MIH(INPINT,CHAOUT,EXPAL,EXPTRU)
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
*  This routine converts integers to hex.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*        INPINT : Input integer to be converted (actual parameter)
*        CHAOUT : Output string (HEX characters)
*        EXPAL  : Flag showing whether Exponent is allowed
*                 (Allowed=0 | Forbidden=1)
*        EXPTRU : Flag showing whether Exponent is present
*
      INTEGER INPINT, EXPAL
      CHARACTER CHAOUT*9
      LOGICAL EXPTRU
*
*  LOCALS
*  ------
*        INTIN  : Input integer (modified locally)
*        TEMP   : Temporary variable
*        N      : Loop counter
*        I      : Loop counter

*        HEXOUT : Output integer (in HEX)
*        BIT    : Array containing powers of 2
*        NUMBIT : Bit number

*        NEGNUM : Flag showing whether a number is negative
*
      INTEGER INTIN, TEMP, N, I
      INTEGER HEXOUT(8), BIT(8), NUMBIT
      LOGICAL NEGNUM
      DATA BIT/1,2,4,8,16,32,64,128/
*
*---------------------------------------------------------------

*   Set output array to 0
      INTIN=INPINT
      DO 5 N=1,8
         HEXOUT(N)=0
 5    CONTINUE

*        If input negative, then make it positive & set negative
*        input flag to true
      IF (INTIN.LT.0) THEN
         INTIN = -INTIN
         NEGNUM = .TRUE.
      ELSE
         NEGNUM = .FALSE.
      ENDIF
      N=8

*   Decide on how many bits are to be used in last byte
      IF(EXPAL.EQ.0) THEN
         NUMBIT = BIT(4)
      ELSE
         NUMBIT = BIT(5)
      ENDIF
 12   CONTINUE

*   Break the integer into 5 bit chunks
      IF(INTIN.GE.NUMBIT)THEN
         TEMP = MOD(INTIN,BIT(6))
         INTIN = INTIN/BIT(6)
         TEMP = TEMP + BIT(6)+BIT(7)
         HEXOUT(N) = TEMP
         N = N-1
         GOTO 12
      ENDIF

*   Sort out last byte
      TEMP = INTIN+BIT(6)+BIT(7)
      IF (NEGNUM) TEMP = TEMP + BIT(5)
      IF ((EXPTRU).AND.(EXPAL.EQ.0)) TEMP = TEMP + BIT(4)
         HEXOUT(N) = TEMP
         HEXOUT(8) = HEXOUT(8) - BIT(6)
  15  CONTINUE

*   Write out the number into a character string
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
