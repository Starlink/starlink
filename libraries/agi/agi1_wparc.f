************************************************************************
*+  AGI_1WPARC - Fill a parameter with a character string

      SUBROUTINE AGI_1WPARC ( PICLOC, PTYPE, PVAL, STATUS )

*    Description :
*     Fill a parameter with a parameter string
*
*    Invocation :
*     CALL AGI_1WPARC( PICLOC, PTYPE, PVAL, STATUS )
*
*    Method :
*     Check status on entry.
*     Check that the input string is less than AGI__CMAX long.
*     If the parameter structure is not found then
*        Create one.
*     Endif
*     Put the character string into the parameter entry.
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     July 1988
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*    Import :

*     Locator to picture
      CHARACTER * ( DAT__SZLOC ) PICLOC

*     Name of parameter
      CHARACTER * ( * ) PTYPE

*     Character string to write to parameter
      CHARACTER * ( * ) PVAL

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'agi_locs'

*    Local variables :
      LOGICAL FOUND

      INTEGER CLEN

      CHARACTER * ( DAT__SZLOC ) PARLOC
      CHARACTER * ( AGI__SZNAM ) PNAME
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check if character string is less than cmax long
         CLEN = LEN( PVAL )
         IF ( CLEN .GT. AGI__CMAX ) THEN
            CLEN = AGI__CMAX
         ENDIF

*   Make sure a PNAME parameter is no longer than AGI__SZNAM
         IF ( PTYPE .EQ. 'PNAME' ) THEN
            CLEN = MIN( CLEN, AGI__SZNAM )
         ENDIF

*   Check if the parameter is present
         PARLOC = ' '
         CALL AGI_1FPAR( PICLOC, PTYPE, PARLOC, FOUND, STATUS )

*   If the structure is not there then create it
         IF ( .NOT. FOUND ) THEN
            CALL DAT_NEWC( PICLOC, PTYPE, CLEN, 0, 0, STATUS )
            CALL DAT_FIND( PICLOC, PTYPE, PARLOC, STATUS )
         ENDIF

*   Put value into element. If the type is PNAME then ensure the
*   string is uppercase and less than AGI__SZNAM long.
         IF ( PTYPE .EQ. 'PNAME' ) THEN
            PNAME = PVAL( :CLEN )
            CALL CHR_UCASE( PNAME )
            CALL DAT_PUTC( PARLOC, 0, 0, PNAME( :CLEN ), STATUS )
         ELSE
            CALL DAT_PUTC( PARLOC, 0, 0, PVAL( :CLEN ), STATUS )
         ENDIF
         CALL DAT_ANNUL( PARLOC, STATUS )
         PARLOC = ' '

*   Indicate that the database has been updated
         FLUSH = .TRUE.

      ENDIF

*      print*, '+++++ AGI_1WPARC +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

