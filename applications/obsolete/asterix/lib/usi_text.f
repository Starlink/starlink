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
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Global variables :
*
      INCLUDE 'USI_CMN'
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
*
*    Function declarations :
*
      INTEGER             CHR_LEN
      LOGICAL			CHR_SIMLR
*
*    Local variables :
*
      CHARACTER*200       FILE                ! File name of object
      CHARACTER*200       PATH                ! Path name of object
      CHARACTER*40        PARNAME             ! Name of parameter
      CHARACTER           PLOC*(DAT__SZLOC)   ! Parameter locator
      CHARACTER*140       WORK(30)            ! Work space

      INTEGER             CODE                ! Internal parameter code
      INTEGER             I                   ! Counter
      INTEGER             IP                  ! Parameter counter
      INTEGER             LBRACKPOS           ! Position of a "{"
      INTEGER             LEVELS              ! Levels of object
      INTEGER             MAXLINES            ! Max no. of output lines
      INTEGER             RBRACKPOS           ! Position of a "}"
      INTEGER             USE                 ! Current line
      INTEGER             SLEN, WLEN          ! String lengths
      INTEGER		L1, L2

      LOGICAL             PVALID              ! Valid parameter found?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check number of input lines
      MAXLINES = OUTLINES
      IF (( MAXLINES .LT. 1 ) .OR. (MAXLINES.LT.INLINES) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Not enough space for output', STATUS )
        GOTO 99
      END IF

*    Skip over input text looking for parameter references
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

*          Extract parameter name
            PARNAME = TEXT(I)((LBRACKPOS+1):(RBRACKPOS-1))
            CALL CHR_UCASE(PARNAME)

*          Is it valid?
            CODE = 0
            L1 = CHR_LEN(PARNAME)
            IP = 1
            DO WHILE ( (IP.LE.USI__NMAX) .AND. (CODE.EQ.0) )
              IF ( DS(IP).USED ) THEN
                L2 = CHR_LEN(DS(IP).PAR)
                IF ( CHR_SIMLR(PARNAME(:L1),DS(IP).PAR(:L2)) ) THEN
                  CODE = IP
                  PVALID = .TRUE.
                END IF
              END IF
              IP = IP + 1
            END DO

            IF ( CODE .NE. 0 ) THEN

*            Parameter name is ok - get locator
              PLOC = DS(CODE).LOC
              IF ( STATUS .NE. SAI__OK)  THEN
                CALL ERR_FLUSH( STATUS )
                PVALID=.FALSE.
              END IF
            ELSE
              PVALID = .FALSE.
              CALL ERR_ANNUL( STATUS )
            END IF

            IF ( PVALID ) THEN

*            Do an HDS trace on the object
              CALL HDS_TRACE( PLOC, LEVELS, PATH, FILE, STATUS )
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

              ELSE
                USE = USE + 1
                WORK(USE) = TEXT(I)
              END IF
            END IF
         END IF

      END DO

*    Generate output text
      IF ( USE .GT. MAXLINES ) THEN
        CALL MSG_PRNT( 'WARNING : Text overfilled array in USI_TEXT' )
      END IF
      OUTLINES = MIN(USE,MAXLINES)

*    Copy to buffer
      DO I = 1, OUTLINES
        TEXT(I)=WORK(I)(:CHR_LEN(WORK(I)))
      END DO

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from USI_TEXT', STATUS )
      END IF

      END
