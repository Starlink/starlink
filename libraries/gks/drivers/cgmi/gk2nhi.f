      SUBROUTINE GK2NHI(INTVAL,RBUFF)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) Workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  This routine gets an integer from the CGM.
*
*  MAINTENANCE LOG
*  ---------------
*     03/05/90  RTP   Add argument RBUFF to read directly from input
*
*  ARGUMENTS
*  ---------
*
*   OUT  INTVAL : Output integer
*   INP  RBUFF  : TRUE - Read from internal BUFFER
*                 FALSE - Read direct from file
*
      INTEGER INTVAL
      LOGICAL RBUFF
*
*  LOCALS
*  ------
*        TEMP   : Temporary variable
*        INPINT : Input integer to be converted (actual parameter)

*        NEGNUM : Flag showing whether a number is negative

      INTEGER TEMP, INPINT
      LOGICAL NEGNUM

*---------------------------------------------------------------------

      NEGNUM = .FALSE.

*   Get first byte of integer
      IF (RBUFF) THEN
         CALL GK2NNC(INPINT,.TRUE.)
      ELSE
         CALL GK2NBU(INPINT)
      ENDIF
      TEMP=MOD(INPINT,16)
      INPINT=INPINT/16

*   If sign bit is set, then set NEGNUM to true
      IF(MOD(INPINT,2).EQ.1) THEN
         NEGNUM = .TRUE.
      ENDIF
      INPINT=INPINT/2

  10  CONTINUE

*   If extension flag is set, get next byte
      IF(INPINT.EQ.3) THEN
         TEMP=TEMP*32
         IF (RBUFF) THEN
            CALL GK2NNC(INPINT,.TRUE.)
         ELSE
            CALL GK2NBU(INPINT)
         ENDIF

*   Add the last 5 bits to the running total
         TEMP=TEMP+MOD(INPINT,32)
         INPINT=INPINT/32
         GOTO 10
      ELSE

*   When extension flag not set (ie last byte), make number
*   positive or negative, according to the sign bit
         IF(NEGNUM) THEN
            INTVAL=-TEMP
         ELSE
            INTVAL=TEMP
         ENDIF
      ENDIF
      RETURN
      END
