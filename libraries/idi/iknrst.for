*-----------------------------------------------------------------------
*+  IKNRST - Reset Ikon

      SUBROUTINE IKNRST ( DISPID, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIDRST.
*     The arguments are identical to those in IIDRST.
*
*    Invocation :
*     CALL IKNRST( DISPID, STATUS )
*
*    Method :
*     Purge the buffer for the given device.
*     Send the reset to the Ikon and wait until it is ready.
*     Reset the characteristics for the current configuration.
*
*    Deficiencies :
*     Very non-standard Fortran - INTEGER * 2
*     Uses VAX system calls - SYS$QIOW, LIB$WAIT
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  { DUVAD::NE )
*
*    History :
*     November 1988
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE '($IODEF)'
      INCLUDE '($SSDEF)'
      INCLUDE '($SYSSRVNAM)'
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Display identifier
      INTEGER DISPID

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMID)'
      INCLUDE 'IDIINC(IKN_COMBUF)'

*    Local variables :
      INTEGER * 2 CHAN, FUNC
      INTEGER ISTAT, NOTSET
*-

*   Use a local variable for the channel number
      CHAN = ACHAN( DISPID )

*   Check that the buffer is flushed on this device
      IF ( QCHAN .EQ. CHAN ) THEN
         CALL IKNOUT( STATUS )
      ENDIF

*   If there is a current QIO then check it has completed
      IF ( LQIO ) THEN
         ISTAT = SYS$SYNCH( %VAL( NEVF ), IOSB )
         IF ( ( ISTAT .NE. SS$_NORMAL ) .OR.
     :        ( IOSB( 1 ) .NE. SS$_NORMAL ) ) THEN
            STATUS = IDI__IOERR
            GOTO 99
         ENDIF
         LQIO = .FALSE.
      ENDIF

*   Clear the display
      FUNC = IO$_WRITEVBLK .OR. IO$M_RESET
      ISTAT = SYS$QIOW( ,%VAL( CHAN ), %VAL( FUNC ), IOSB,,,
     :                  %VAL( 0 ), %VAL( 0 ),,,, )
      IF ( ISTAT .NE. SS$_NORMAL ) THEN
         STATUS = IDI__IOERR
         GOTO 99
      ENDIF

*   Wait until the device is ready.
*   This occurs when status line A goes low
      NOTSET = 1
      FUNC = IO$_WRITEVBLK
      DO WHILE ( NOTSET .NE. 0 )

*   Wait 1/10th of a second before trying again
         CALL LIB$WAIT( 0.1 )

*   Send a QIO to test the status
         ISTAT = SYS$QIOW( ,%VAL( CHAN ), %VAL( FUNC ), IOSB,,,
     :                     %VAL( 0 ), %VAL( 0 ),,,, )
         IF ( ISTAT .NE. SS$_NORMAL ) THEN
            STATUS = IDI__IOERR
            GOTO 99
         ENDIF

*   Test status line A
         NOTSET = IOSB( 3 ) .AND. '800'X

      ENDDO

*   Reset the initial conditions for the current configuration
      CALL IKNINT( DISPID, CONFIG, STATUS )

  99  CONTINUE

      END

