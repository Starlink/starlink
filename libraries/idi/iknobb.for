*-----------------------------------------------------------------------
*+  IKNOBB - Output data in bytes to Ikon buffer

      SUBROUTINE IKNOBB ( DISPID, NUMBUF, BYTBUF, STATUS )

*    Description :
*     This sends a number of bytes to the output buffer. The
*     buffers are used alternately, first buffer 0 and then
*     buffer 1. The buffers are flushed at the end of this
*     routine, so there should be no data in either of the
*     buffers after this routine has completed.
*
*    Invocation :
*     CALL IKNOBB( DISPID, NUMBUF, BYTBUF, STATUS )
*
*    Method :
*     Purge the output buffer if it does not contain output for this
*     device.
*     Send the bytes to the buffer. Purge the buffer if it is full.
*     Every other byte is reversed since the VAX stores bytes reversed
*     in an INTEGER word.
*
*    Deficiencies :
*     Very non-standard Fortran - BYTE
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

*     Number of bytes to send
      INTEGER NUMBUF

*     Buffer containing bytes to send
      BYTE BYTBUF( * )

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMID)'
      INCLUDE 'IDIINC(IKN_COMBUF)'

*    Local variables :
      INTEGER ISTAT, J
*-

*   If the channel numbers are different then send any outstanding
*   bytes to the old channel.
      IF ( ( WRITEF .AND. ( QCHAN .NE. ACHAN( DISPID ) ) ) ) THEN
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

*   If the buffer is full then send it
            IF ( BUFLN1 .GE. MAXBUF ) THEN
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
*   Reverse the order of the bytes in a word
            BUFLN1 = BUFLN1 + 1
            REVRS1 = -REVRS1
            BUFF1( BUFLN1 + REVRS1 ) = BYTBUF( J )
         ENDDO

*   Select the current buffer
      ELSEIF ( BUFNUM .EQ. 2 ) THEN

*   Put the new data into the buffer
         DO J = 1, NUMBUF

*   If the buffer is full then send it
            IF ( BUFLN2 .GE. MAXBUF ) THEN
               CALL IKNOUT( STATUS )

*   Reset the write flag as there is more data to send
*   This do loop only applies to BUFNUM = 1 so therefore have to
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
*   Reverse the order of the bytes in a word
            BUFLN2 = BUFLN2 + 1
            REVRS2 = -REVRS2
            BUFF2( BUFLN2 + REVRS2 ) = BYTBUF( J )
         ENDDO
      ENDIF

*   Send the buffer
      CALL IKNOUT( STATUS )

  99  CONTINUE

      END

