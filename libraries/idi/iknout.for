*-----------------------------------------------------------------------
*+  IKNOUT - Output the buffer to the Ikon

      SUBROUTINE IKNOUT ( STATUS )

*    Description :
*     This sends the output buffer to the Ikon.
*
*    Invocation :
*     CALL IKNOUT( STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Check that the output buffer has something to send.
*     Ensure that the byte count is even.
*     Send the buffer to the Ikon using SYS$QIO.
*
*    Deficiencies :
*     Very non-standard Fortran - INTEGER * 2
*     Uses VAX system calls - SYS$QIO, SYS$SYNCH
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     November 1988
*     March 1990     Double buffering for synchronous QIOs
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

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMBUF)'

*    Local Constants :
      INTEGER FNCT, TIMOUT
      PARAMETER ( FNCT = 0 )
      PARAMETER ( TIMOUT = 10 )

*   IO$_WRITEVBLK modifiers for the DR11-W and DRV11-WA drivers
*   IO$M_SETFNCT sends parameter P4 specified by FNCT
*   IO$M_TIMED is a time-out for the QIO specified by TIMOUT
      INTEGER * 2 FUNC
      PARAMETER ( FUNC = IO$_WRITEVBLK .OR.
     :                   IO$M_SETFNCT .OR. IO$M_TIMED )

*    Local variables :
      INTEGER ISTAT
*-

*   Check the write flag
      IF ( WRITEF ) THEN

*   If there is a current QIO then check it has completed
         IF ( LQIO ) THEN
            ISTAT = SYS$SYNCH( %VAL( NEVF ), IOSB )
            IF ( ( ISTAT .NE. SS$_NORMAL ) .OR.
     :           ( IOSB( 1 ) .NE. SS$_NORMAL ) ) THEN
               STATUS = IDI__IOERR
               GOTO 99
            ENDIF
            LQIO = .FALSE.
         ENDIF

*   Select the current buffer
         IF ( BUFNUM .EQ. 1 ) THEN

*   The DR11-W driver transfers words so ensure that the buffer count
*   is even
            IF ( MOD( BUFLN1, 2 ) .NE. 0 ) THEN
               BUFLN1 = BUFLN1 + 1
               BUFF1( BUFLN1 ) = 0
            ENDIF

*   Send the buffer
            ISTAT = SYS$QIO( %VAL( NEVF ), %VAL( QCHAN ), %VAL( FUNC ),
     :                       IOSB,,, BUFF1, %VAL( BUFLN1 ),
     :                       %VAL( TIMOUT ), %VAL( FNCT ),, )
            LQIO = .TRUE.

*   Check the return status
            IF ( ISTAT .NE. SS$_NORMAL ) THEN
               STATUS = IDI__IOERR
            ENDIF

*   Reset the write flag and the buffer length
            WRITEF = .FALSE.
            BUFLN1 = 0
            REVRS1 = -1

*   Select the current buffer
         ELSEIF ( BUFNUM .EQ. 2 ) THEN

*   The DR11-W driver transfers words so ensure that the buffer count
*   is even
            IF ( MOD( BUFLN2, 2 ) .NE. 0 ) THEN
               BUFLN2 = BUFLN2 + 1
               BUFF2( BUFLN2 ) = 0
            ENDIF

*   Send the buffer
            ISTAT = SYS$QIO( %VAL( NEVF ), %VAL( QCHAN ), %VAL( FUNC ),
     :                       IOSB,,, BUFF2, %VAL( BUFLN2 ),
     :                       %VAL( TIMOUT ), %VAL( FNCT ),, )
            LQIO = .TRUE.

*   Check the return status
            IF ( ISTAT .NE. SS$_NORMAL ) THEN
               STATUS = IDI__IOERR
            ENDIF

*   Reset the write flag and the buffer length
            WRITEF = .FALSE.
            BUFLN2 = 0
            REVRS2 = -1
         ENDIF

*   Swap the buffer number
         IF ( BUFNUM .EQ. 1 ) THEN
            BUFNUM = 2
         ELSE
            BUFNUM = 1
         ENDIF

      ENDIF

  99  CONTINUE

      END

