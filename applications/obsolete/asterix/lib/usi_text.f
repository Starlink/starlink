*+  USI_TEXT - Scans a text array expanding parameter refs into extra lines
      SUBROUTINE USI_TEXT( INLINES, TEXT, OUTLINES, STATUS )
*    Description :
*    Method :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     13 Dec 88 : Original (DJA)
*     13 Jan 90 : Recoded using SUBPAR_ calls (DJA)
*     22 Nov 94 : Generic parameter system - removed SUBPAR (DJA)
*     18 Jan 96 : No longer needs USI common block (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Import :
*
      INTEGER             INLINES             ! Number of input lines
*
*    Import-Export :
*
      INTEGER             OUTLINES            ! Number of output lines & on
                                              ! entry, max no. of lines.
      CHARACTER*(*)       TEXT(OUTLINES)      ! Text to scan
*
*    Status :
*
      INTEGER STATUS
*   Functions:
      INTEGER			CHR_LEN
*
*    Local variables :
*
      CHARACTER*200       	FILE                	! File name of object
      CHARACTER*200       	PATH                	! Path name of object
      CHARACTER*40        	PARNAME             	! Name of parameter
      CHARACTER*140       	WORK(30)            	! Work space

      INTEGER             	I                   	! Counter
      INTEGER             	LBRACKPOS           	! Position of a "{"
      INTEGER             	LEVELS              	! Levels of object
      INTEGER             	MAXLINES            	! Max # of output lines
      INTEGER			PID			! ADI identifier
      INTEGER			PSID			! ADI identifier
      INTEGER             	RBRACKPOS           	! Position of a "}"
      INTEGER             	USE                 	! Current line
      INTEGER             	SLEN, WLEN          	! String lengths

      LOGICAL			ISTEMP			! Temporary?
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check number of input lines
      MAXLINES = OUTLINES
      IF (( MAXLINES .LT. 1 ) .OR. (MAXLINES.LT.INLINES) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Not enough space for output', STATUS )
        GOTO 99
      END IF

*  Skip over input text looking for parameter references
      USE = 0
      DO I = 1, INLINES
        LBRACKPOS = INDEX( TEXT(I), '{' )
        IF ( LBRACKPOS .EQ. 0 ) THEN
          USE = USE + 1
          WORK(USE) = TEXT(I)
        ELSE
          RBRACKPOS = INDEX( TEXT(I),'}' )
          IF (( RBRACKPOS .EQ. 0 ) .OR. ( RBRACKPOS .LT.
     :                                      LBRACKPOS )) THEN
            USE = USE + 1
            WORK(USE) = TEXT(I)
          ELSE

*        Extract parameter name
            PARNAME = TEXT(I)((LBRACKPOS+1):(RBRACKPOS-1))
            CALL CHR_UCASE(PARNAME)
            CALL USI0_FNDPSL( PARNAME, .FALSE., PSID, STATUS )

*        Parameter name is ok - get identifier
            IF ( PID .NE. ADI__NULLID ) THEN

*          Is it a temporary
              CALL ADI_CGET0L( PSID, 'TEMP', ISTEMP, STATUS )
              IF ( ISTEMP ) THEN
                CALL ADI_CGET0C( PSID, 'VALUE', PATH, STATUS )

                USE = USE + 1
                SLEN = CHR_LEN(PATH)
                WORK(USE) = TEXT(I)(1:(LBRACKPOS-1))//'Value : '
                WLEN = CHR_LEN(WORK(USE)) + 1
                IF ( (SLEN+WLEN) .GT. LEN(TEXT(1)) ) THEN
                  USE = USE + 1
                  WLEN = WLEN - 5
                  CALL CHR_FILL( ' ', WORK(USE) )
                END IF
                WORK(USE) = WORK(USE)(:WLEN)//PATH(:SLEN)

              ELSE
                CALL ADI_CGET0I( PSID, 'ID', PID, STATUS )

*          Do a trace on the object
                CALL ADI_FTRACE( PID, LEVELS, PATH, FILE, STATUS )
                IF ( STATUS .NE. SAI__OK ) GOTO 99
                USE = USE + 1
                WORK(USE) = TEXT(I)(1:(LBRACKPOS-1))//'File : '
                SLEN = CHR_LEN(FILE)
                WLEN = CHR_LEN(WORK(USE)) + 1
                IF ( (SLEN+WLEN) .GT. LEN(TEXT(1)) ) THEN
                  USE = USE + 1
                  WLEN = WLEN - 4
                  CALL CHR_FILL( ' ', WORK(USE) )
                END IF
                WORK(USE) = WORK(USE)(:WLEN)//FILE(:SLEN)

*              Write path if needed
                IF ( LEVELS .GT. 1 ) THEN
                  USE = USE + 1
                  SLEN = CHR_LEN(PATH)
                  CALL CHR_FILL( ' ', WORK(USE)(:LBRACKPOS-1) )
                  WORK(USE) = WORK(USE)(:LBRACKPOS-1)//'Object :'
                  WLEN = CHR_LEN(WORK(USE)) + 1
                  IF ( (SLEN+WLEN) .GT. LEN(TEXT(1)) ) THEN
                    USE = USE + 1
                    WLEN = WLEN - 6
                    CALL CHR_FILL( ' ', WORK(USE) )
                  END IF
                  WORK(USE) = WORK(USE)(:WLEN)//PATH(:SLEN)
                END IF

                END IF
                CALL ADI_ERASE( PSID, STATUS )

              ELSE
                USE = USE + 1
                WORK(USE) = TEXT(I)
              END IF
            END IF
         END IF

      END DO

*  Generate output text
      IF ( USE .GT. MAXLINES ) THEN
        CALL MSG_PRNT( 'WARNING : Text overfilled array in USI_TEXT' )
      END IF
      OUTLINES = MIN(USE,MAXLINES)

*  Copy to buffer
      DO I = 1, OUTLINES
        TEXT(I)=WORK(I)(:CHR_LEN(WORK(I)))
      END DO

*  Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'USI_TEXT', STATUS )
      END IF

      END
