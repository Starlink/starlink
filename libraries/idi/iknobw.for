*-----------------------------------------------------------------------
*+  IKNOBW - Output data packed in words to Ikon buffer

      SUBROUTINE IKNOBW ( DISPID, NUMBUF, WORBUF, STATUS )

*    Description :
*     This sends a number of words to the output buffer. The
*     double buffering is not used for sending instructions,
*     so this routine only uses buffer 0.
*
*    Invocation :
*     CALL IKNOBW( DISPID, NUMBUF, WORBUF, STATUS )
*
*    Method :
*     Purge the output buffer if the output does not belong to this
*     device.
*     Send the words to the output buffer byte by byte. Purge the
*     output buffer if it is full.
*
*    Deficiencies :
*     Very non-standard Fortran - BYTE, INTEGER * 2
*     VAX specific call - SYS$SYNCH. This used to be in IKNOUT only,
*     but the introduction of separate buffer arrays ( for speed )
*     means that it has to be included in this routine.
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
      INCLUDE '($SSDEF)'
      INCLUDE '($SYSSRVNAM)'
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Display identifier
      INTEGER DISPID

*     Number of words to send
      INTEGER NUMBUF

*     Buffer containing words to send
      INTEGER * 2 WORBUF( * )

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMID)'
      INCLUDE 'IDIINC(IKN_COMBUF)'

*    Local variables :
      INTEGER ISTAT, J

      BYTE BTEMP( 2 )
      INTEGER * 2 WTEMP
      EQUIVALENCE ( WTEMP, BTEMP( 1 ) )
*-

*   If the channel numbers are different then send any outstanding
*   bytes to the old channel.
      IF ( WRITEF .AND. ( QCHAN .NE. ACHAN( DISPID ) ) ) THEN
         CALL IKNOUT( STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            GOTO 99
         ENDIF
      ENDIF

*   Set up the buffer for writing on this channel
      IF ( NUMBUF .GT. 0 ) THEN
         WRITEF = .TRUE.
         QCHAN = ACHAN( DISPID )
      ENDIF

*   Select the current buffer
      IF ( BUFNUM .EQ. 1 ) THEN

*   Put the new data into the buffer
         DO J = 1, NUMBUF

*   If the buffer is full or nearly full then send it
*   Note: IKNOUT changes the buffer number
            IF ( BUFLN1 .GE. MAXBUF - 1 ) THEN
               CALL IKNOUT( STATUS )

*   Reset the write flag as there is more data to send
*   This do loop only applies to BUFNUM = 1 so therefore have to
*   reset the buffer number and synchronise the QIO
               IF ( STATUS .EQ. IDI__OK ) THEN
                  WRITEF = .TRUE.
                  BUFNUM = 1
                  IF ( LQIO ) THEN
                     ISTAT = SYS$SYNCH( %VAL( NEVF ), IOSB )
                     IF ( ( ISTAT .NE. SS$_NORMAL ) .OR.
     :                    ( IOSB( 1 ) .NE. SS$_NORMAL ) ) THEN
                        STATUS = IDI__IOERR
                        GOTO 99
                     ENDIF
                     LQIO = .FALSE.
                  ENDIF
               ELSE
                  GOTO 99
               ENDIF
            ENDIF

*   Add the next element to the buffer
            BUFLN1 = BUFLN1 + 2
            WTEMP = WORBUF( J )
            BUFF1( BUFLN1 - 1 ) = BTEMP( 1 )
            BUFF1( BUFLN1 ) = BTEMP( 2 )
         ENDDO

*   Select the current buffer
      ELSEIF ( BUFNUM .EQ. 2 ) THEN

*   Put the new data into the buffer
         DO J = 1, NUMBUF

*   If the buffer is full or nearly full then send it
*   Note: IKNOUT changes the buffer number
            IF ( BUFLN2 .GE. MAXBUF - 1 ) THEN
               CALL IKNOUT( STATUS )

*   Reset the write flag as there is more data to send
*   This do loop only applies to BUFNUM = 2 so therefore have to
*   reset the buffer number and synchronise the QIO
               IF ( STATUS .EQ. IDI__OK ) THEN
                  WRITEF = .TRUE.
                  BUFNUM = 2
                  IF ( LQIO ) THEN
                     ISTAT = SYS$SYNCH( %VAL( NEVF ), IOSB )
                     IF ( ( ISTAT .NE. SS$_NORMAL ) .OR.
     :                    ( IOSB( 1 ) .NE. SS$_NORMAL ) ) THEN
                        STATUS = IDI__IOERR
                        GOTO 99
                     ENDIF
                     LQIO = .FALSE.
                  ENDIF
               ELSE
                  GOTO 99
               ENDIF
            ENDIF

*   Add the next element to the buffer
            BUFLN2 =  BUFLN2 + 2
            WTEMP = WORBUF( J )
            BUFF2( BUFLN2 - 1 ) = BTEMP( 1 )
            BUFF2( BUFLN2 ) = BTEMP( 2 )
         ENDDO
      ENDIF

  99  CONTINUE

      END

