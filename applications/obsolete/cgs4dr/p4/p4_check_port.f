*+  P4_CHECK_PORT - Checks the port for an open device
      SUBROUTINE P4_CHECK_PORT( PORT, STATUS )
*    Invocation :
*     CALL P4_CHECK_PORT( PORT, STATUS )
*    Authors :
*     P N Daly (JACH.HAWAII.EDU::PND)
*    History :
*      7-Aug-1994: Original Unix version                     (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS           ! Global status
*    External references :
      INTEGER PGBEGIN          ! PGPLOT open device routine
      INTEGER CHR_LEN          ! Finds used length of string
      LOGICAL CHR_SIMLR        ! T if two strings are similar
*    Global variables :
      INCLUDE 'P4COM.INC'      ! P4 common block
*    Local variables :
      CHARACTER*( NBS_FLEN )
     :  TMPSTR,                ! A temporary string
     :  PG_STATE               ! Return from PGQINF call
      INTEGER
     :  PORT,                  ! Port number
     :  CLEN,                  ! Length of string
     :  IGNORE,                ! Ignorable return
     :  ERR_STAT               ! An error status
      LOGICAL PROCEED          ! T if we can open a device
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check if PGPLOT is already open
      CALL PGQINF( 'STATE', PG_STATE, IGNORE )
      IF ( PG_STATE .EQ. 'OPEN' ) THEN

*      Get the device specified on this port
        TMPSTR = DEVICE_NAME( PORT )
        CALL CHR_RMBLK( TMPSTR )
        CALL CHR_UCASE( TMPSTR )
        CLEN = CHR_LEN( TMPSTR )

*      If it is similar to the current device, we need go on further
        IF ( CHR_SIMLR( TMPSTR(1:CLEN)//'/OPEN', CURRENT_DEVICE ) ) THEN
          CALL PGQCOL( CI1( PORT ), CI2( PORT ) )
          PROCEED = .FALSE.
          PORT_OK( PORT ) = .TRUE.
          IF ( VERBOSE ) THEN
            CALL MSG_SETC( 'DEV', DEVICE_NAME( PORT ) )
            CALL MSG_SETI( 'PORT', PORT )
            CALL MSG_OUT( ' ',
     :        'Device ^DEV is already open on port ^PORT', STATUS )
          ENDIF
        ELSE

*        If it is not similar but it is specified, close it
          IF ( DEVICE_NAME( PORT ) .NE. ' ' ) THEN
            PROCEED = .TRUE.
            PORT_OK( PORT ) = .FALSE.
            PLOT_OK( PORT ) = .FALSE.
            IF ( VERBOSE ) THEN
              CALL MSG_SETC( 'DEV', DEVICE_NAME( PORT ) )
              CALL MSG_SETI( 'PORT', PORT )
              CALL MSG_OUT( ' ',
     :          'Closing device ^DEV on port ^PORT', STATUS )
            ENDIF
            CALL PGEND
          ELSE

*          It is not similar and has not been specified so signal an error
            PROCEED = .FALSE.
            PORT_OK( PORT ) = .FALSE.
            PLOT_OK( PORT ) = .FALSE.
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'PORT', PORT )
            CALL ERR_REP( ' ', 'P4_CHECK_PORT: '/
     :        /'Unspecified output device on port ^PORT', STATUS )
          ENDIF
        ENDIF
      ELSE

*      PGPLOT is not open so we can proceed to open a device
        PROCEED = .TRUE.
        PORT_OK( PORT ) = .FALSE.
      ENDIF

*    Proceed as necessary
      IF ( PROCEED ) THEN

*      No device open so go ahead
        CALL MSG_SETC( 'DEV', DEVICE_NAME( PORT ) )
        CALL MSG_SETI( 'PORT', PORT )
        CALL MSG_OUT( ' ', 'Opening device ^DEV on port ^PORT', STATUS )

        CLEN = CHR_LEN( DEVICE_NAME( PORT ) )
        STATUS = PGBEGIN( 0, DEVICE_NAME(PORT)(1:CLEN), 1, 1 )
        IF ( STATUS .EQ. PG__OK ) THEN
          CALL PGQCOL( CI1( PORT ), CI2( PORT ) )
          CURRENT_DEVICE = DEVICE_NAME( PORT )( 1:CLEN ) // '/OPEN'
          CALL CHR_UCASE( CURRENT_DEVICE )
          PORT_OK( PORT ) = .TRUE.
          PLOT_OK( PORT ) = .FALSE.
          CALL ERR_ANNUL( STATUS )
        ELSE
          PORT_OK( PORT ) = .FALSE.
          PLOT_OK( PORT ) = .FALSE.
          ERR_STAT = STATUS
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'DEV', DEVICE_NAME( PORT ) )
          CALL MSG_SETI( 'PORT', PORT )
          CALL MSG_SETI( 'ES', ERR_STAT )
          CALL ERR_REP( ' ', 'P4_CHECK_PORT: '/
     :      /'Error while opening opening '/
     :      /'^DEV on port ^PORT , PGPLOT Status = ^PGSTAT', STATUS )
        ENDIF
      ENDIF

*    Update the noticeboard for this port
      CALL P4_WRITE_NB( PORT, STATUS )
      END
