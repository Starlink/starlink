*+  STR_CTOB - Converts 8-bit binary number from CHAR to BYTE
      SUBROUTINE STR_CTOB(ISTRING,NUM,STATUS)
*    Description :
*     Converts an 8-bit binary number stored in character form to the
*     equivalent byte quantity. Invalid characters in the input string
*     cause bad status to be returned.  Numbers of less than 8-bits are
*     right adjusted
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     (BHVAD::RJV)
*    History :
*
*     24 Feb 94 : Updated to use BIT_ functions (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      CHARACTER*8 ISTRING
*
*    Export :
*
      BYTE NUM
*
*    Status :
*
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
      BYTE    BIT_ORUB
*    Local constants :
      CHARACTER*8 STRING
      INTEGER L
      INTEGER IBIT,JBIT
      BYTE BIT
*    Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN
        NUM=0
        STRING='00000000'

* right adjust number if necessary
        L=CHR_LEN(ISTRING)
        STRING(9-L:)=ISTRING(:L)

* scan string bit by bit
        IBIT=0
        DO WHILE (IBIT.LE.7.AND.STATUS.EQ.SAI__OK)
          IF (IBIT.LT.7) THEN
            BIT=2**IBIT
          ELSE
            BIT=-128
          ENDIF
          JBIT=8-IBIT
          IF (STRING(JBIT:JBIT).EQ.'1') THEN
            NUM=BIT_ORUB(NUM,BIT)
          ELSEIF (STRING(JBIT:JBIT).NE.'0') THEN
            STATUS=SAI__ERROR
            CALL ERR_REP('BADCHAR','Invalid character in binary string',
     :                                                           STATUS)
          ENDIF
          IBIT=IBIT+1
        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP('STR_CTOB','from STR_CTOB',STATUS)
        ENDIF
      ENDIF
      END
