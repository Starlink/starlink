      SUBROUTINE ECH_PARSE_ENV( STRING, LENGTH )

*  Global Variables:
      INCLUDE 'ECH_PSX.INC'

*  Arguments:
      CHARACTER*( * ) STRING
      INTEGER LENGTH

*  Local Variables:
      CHARACTER*255 XLATE
      INTEGER IMARK
      INTEGER IBEGIN
      INTEGER XLEN
      INTEGER STATUS

*  Functions Called:
      INTEGER CHR_LEN
*.

      STATUS = 0
      LENGTH = CHR_LEN( STRING )
      IF ( SYSNAME .EQ. 'VMS' ) CALL CHR_UCASE( STRING )
      IF ( STRING( :1 ) .EQ. '$' .AND.
     :     INDEX( STRING, '/' ) .GT. 0 ) THEN
         IMARK = INDEX( STRING, '/' )
         CALL PSX_GETENV( STRING( 2 : IMARK - 1 ), XLATE, STATUS )
         IF ( STATUS .EQ. 0 ) THEN
            XLEN = CHR_LEN( XLATE )
            IF ( SYSNAME .EQ. 'VMS' ) THEN
               STRING = XLATE( :XLEN ) // STRING( IMARK + 1: )

            ELSE
               STRING = XLATE( :XLEN ) // STRING( IMARK: )
            ENDIF
            LENGTH = CHR_LEN( STRING )

         ELSE
            CALL ERR_FLUSH( STATUS )
         END IF

      ELSE IF ( INDEX( STRING, ':' ) .GT. 0 ) THEN
         IMARK = INDEX( STRING, ':' )
         IBEGIN = IMARK - 1
         DO WHILE ( IBEGIN .GT. 1 .AND.
     :              STRING( IBEGIN : IBEGIN ) .NE. ' ' )
            IBEGIN = IBEGIN - 1
         END DO
         CALL PSX_GETENV( STRING( IBEGIN : IMARK - 1 ), XLATE, STATUS )
         IF ( STATUS .EQ. 0 ) THEN
            XLEN = CHR_LEN( XLATE )
            IF ( SYSNAME .EQ. 'VMS' ) THEN
               STRING = XLATE( :XLEN ) // STRING( IMARK + 1 : )
               LENGTH = LENGTH - IMARK + IBEGIN + XLEN

            ELSE
               IF ( STRING( IMARK + 1 : IMARK + 1 ) .EQ. ' ' ) THEN
                 STRING = XLATE( :XLEN )

               ELSE
                  STRING = XLATE( :XLEN ) // '/' // STRING( IMARK + 1: )
               END IF
            END IF
            LENGTH = CHR_LEN( STRING )

         ELSE
            CALL ERR_FLUSH( STATUS )
         END IF
      END IF

      END
