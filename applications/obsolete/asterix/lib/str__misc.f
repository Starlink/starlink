*+  STR_BSTRINGTOI - Converts 8-bit binary number from CHAR to INTEGER
      SUBROUTINE STR_BSTRINGTOI( STRING, NUM, STATUS )
*    Description :
*     Converts an 8-bit binary number stored in character form to the
*     equivalent integer quantity. Invalid characters in the input string
*     cause bad status to be returned.  Numbers of less than 8-bits are
*     right adjusted
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     (BHVAD::RJV)
*    Type Definitions :
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      CHARACTER*(*) STRING
*
*    Export :
*
      INTEGER       NUM

*    Status :
      INTEGER       STATUS
*
*    Function declarations :
*
      INTEGER       CHR_LEN
*
*    Local variables :
*
      INTEGER       I, LEN, START

      LOGICAL       LOOP
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      NUM = 0

*    Trap blank string
      LEN = CHR_LEN( STRING )
      IF ( LEN .LT. 1 ) THEN
        CALL MSG_PRNT( 'FATAL ERROR: Zero length string' )
        STATUS = SAI__ERROR
        GOTO 99
      END IF

*    Remove leading junk
      START = 1
      LOOP  = .TRUE.
      DO WHILE ( LOOP )
        IF ( STRING(START:START) .EQ. '1' .OR.
     :                               STRING(START:START) .EQ. '0') THEN
          LOOP = .FALSE.
        ELSE
          START = START + 1
        END IF
      END DO

*    Loop over string
      DO I = START, LEN
        IF ( STRING(I:I) .EQ. '1' ) THEN
          NUM = NUM + 2**(LEN - I)

        ELSE IF ( STRING(I:I) .NE. '0' ) THEN
          CALL MSG_PRNT( 'FATAL ERROR: String is not a binary number' )
          STATUS = SAI__ERROR
          GOTO 99

        END IF
      END DO

 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from STR_BSTRINGTOI', STATUS )
      END IF

      END
