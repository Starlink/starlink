      SUBROUTINE GK2MEA(NUMCHA,STRIN)
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
*  Routine to convert a character to its ASCII hex value.

*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*        NUMCHA : Number of characters to be converted
*        STRIN  : Input string
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.par'

      INCLUDE '../../include/gkwca.cmn'
*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgm.cmn'

*
*  LOCALS
*  ------
*        I      : Loop counter
*        J      : Loop variable
*        TEMP   : Temporary variable

*        CHAOUT : Output string (HEX characters)

*   Refer to function which gives the ASCII code for a character
      EXTERNAL GK2MNA
      INTRINSIC CHAR

      INTEGER NUMCHA, I, TEMP, GK2MNA, J, NUMBUF
      CHARACTER STRIN*(*),CHAOUT*81
*
*---------------------------------------------------------------

      J=0
  10  CONTINUE
      IF((NUMCHA-J).LT.80) THEN
         NUMBUF=NUMCHA-J
         IF(NUMBUF.EQ.0) GOTO 30
      ELSE
         NUMBUF=80
      ENDIF

*   Convert each character to its equivalent HEX bytes
      DO 20 I=1,NUMBUF
         J=J+1
         TEMP= GK2MNA (STRIN(J:J))
         CHAOUT(I:I)=CHAR(TEMP)
  20  CONTINUE
      CHAOUT(NUMBUF+1:NUMBUF+1)=CHAR(0)
      CALL GK2MBU(CHAOUT)
      GOTO 10

  30  CONTINUE
      RETURN
      END
