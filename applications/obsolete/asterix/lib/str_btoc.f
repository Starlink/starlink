*+  STR_BTOC - Converts 8-bit binary number from  BYTE to CHAR
      SUBROUTINE STR_BTOC(NUM,STRING,STATUS)
*    Description :
*     Returns the binary representation of an 8-bit number in character form
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      BYTE NUM
*    Import-Export :
*    Export :
      CHARACTER*(*) STRING
*    Status :
      INTEGER STATUS
*    External references :
      BYTE BIT_ANDUB
*    Local constants :
      INTEGER IBIT,JBIT
      BYTE BIT
*    Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN
        IF (LEN(STRING).LT.8) THEN
          STATUS=SAI__ERROR
          CALL ERR_REP('TOOSHORT','Character string too short',STATUS)
        ELSE
          STRING='00000000'

* scan number bit by bit
          IBIT=0
          DO WHILE (IBIT.LE.7.AND.STATUS.EQ.SAI__OK)
            IF (IBIT.LT.7) THEN
              BIT=2**IBIT
            ELSE
              BIT=-128
            ENDIF
            JBIT=8-IBIT
            IF (BIT_ANDUB(NUM,BIT)/BIT) THEN
              STRING(JBIT:JBIT)='1'
            ENDIF
            IBIT=IBIT+1
          ENDDO

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP('STR_BTOC','from STR_BTOC',STATUS)
        ENDIF
      ENDIF
      END
