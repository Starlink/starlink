*+  PRS_BITPAT - Decodes a string of 1 or more mask names into a bit pattern
      SUBROUTINE PRS_BITPAT( STR, NMASK, NAMES, CODES, VALUE, STATUS )
*
*    Description :
*
*        Decodes the string STR into a sequence of words. Each word must
*     correspond to one of the mask names supplied. The output VALUE is
*     constructed by ORing the CODES value for each mask name found together.
*
*        STR may consist of alphanumeric names in upper or lower case,
*     separated by commas and as many spaces as you like. The entire string
*     may be enclosed in doubles quotes, brackets, or both.
*
*    Authors :
*
*     ( DJA @ UK.AC.BHAM.SR.STAR )
*
*    History :
*
*     29 Aug 89 : Original ( DJA )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*
*    Import :
*
      CHARACTER*(*)              STR               ! String to analyse
      INTEGER                    NMASK             ! Number of masks supplied

      CHARACTER*(*)              NAMES(NMASK)      ! Names of masks
      INTEGER                    CODES(NMASK)      ! Code values
*
*    Export :
*
      INTEGER                    VALUE             ! The output bit pattern
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER                    CHR_LEN
      LOGICAL                    STR_ABBREV
*
*    Local constants :
*
      CHARACTER                  DQUOTE            !
         PARAMETER               (DQUOTE = '"' )
      CHARACTER                  LPAREN
         PARAMETER               (LPAREN = '(')
      CHARACTER                  RPAREN
         PARAMETER               (RPAREN = ')')
      CHARACTER                  COMMA
         PARAMETER               (COMMA = ',')
*
*    Local variables :
*
      CHARACTER*20               BIT

      INTEGER                    BEG, END, QPOS, I

      LOGICAL                    FOUND
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      VALUE = 0
      BEG = 1
      END = CHR_LEN(STR)

*    Check for quotes
      IF ( STR(BEG:BEG) .EQ. DQUOTE ) THEN
         QPOS = INDEX(STR((BEG+1):),DQUOTE)
         IF ( QPOS .EQ. 0 ) THEN
            CALL ERR_REP( 'UNBALQUOT', 'Unbalanced quote in mask '/
     :                                          /'string', STATUS )
            GOTO 99
         ELSE
            BEG = BEG + 1
            END = BEG + QPOS - 2
         END IF
      END IF

*    Check for parentheses
      IF ( STR(BEG:BEG) .EQ. LPAREN ) THEN
         QPOS = INDEX(STR((BEG+1):),RPAREN)
         IF ( QPOS .EQ. 0 ) THEN
            CALL ERR_REP( 'UNBALQUOT', 'Unbalanced parentheses in '/
     :                                      /'mask string', STATUS )
            GOTO 99
         ELSE
            BEG = BEG + 1
            END = BEG + QPOS - 2
         END IF
      END IF

*    Loop until no more mask names
      DO WHILE ( BEG .LT. END )
         DO WHILE ( ( STR(BEG:BEG) .EQ. ' ' ) .AND. ( BEG .LT. END ) )
            BEG = BEG + 1
         END DO
         IF ( BEG .EQ. END ) GOTO 99
         QPOS = INDEX( STR(BEG:), ',' )
         IF ( QPOS .EQ. 0 ) THEN
            BIT = STR(BEG:END)
            BEG = END
         ELSE
            BIT = STR(BEG:(BEG+QPOS-2))
            BEG = BEG + QPOS
         END IF
         FOUND = .FALSE.
         I = 1
         DO WHILE ( ( I .LE. NMASK ) .AND. ( .NOT. FOUND ) )
            IF ( STR_ABBREV(BIT,NAMES(I)) ) THEN
               FOUND = .TRUE.
            ELSE
               I = I + 1
            END IF
         END DO
         IF ( FOUND ) THEN
            VALUE = IOR( VALUE ,CODES(I) )
         ELSE
            CALL ERR_REP( 'INVMASK', 'Invalid mask name', STATUS )
            GOTO 99
         END IF
      END DO

 99   CONTINUE

      END
