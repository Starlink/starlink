*-----------------------------------------------------------------------
*+  IKNIBB - Input data in bytes from Ikon buffer

      SUBROUTINE IKNIBB ( DISPID, NUMBUF, BYTBUF, STATUS )

*    Description :
*     This reads in an even number of bytes from the Ikon.
*
*    Invocation :
*     CALL IKNIBB( DISPID, NUMBUF, BYTBUF, STATUS )
*
*    Method :
*     Check the required number of bytes is even.
*     Read in the bytres from the Ikon using SYS$QIOW.
*     Reverse every other byte, since the VAX stores bytes reversed in
*     an INTEGER word.
*
*    Deficiencies :
*     Very non-standard Fortran - BYTE, INTEGER * 2
*     Uses VAX system calls - SYS$QIOW
*     The event flag number has to have the same value as in IKNOUT
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     December 1988
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

*    Import-Export :
*     Import : Number of bytes to read
*     Export : number of bytes read
      INTEGER NUMBUF

*    Export :
*     Output byte buffer
      BYTE BYTBUF ( * )

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMBUF)'
      INCLUDE 'IDIINC(IKN_COMID)'

*    Local Constants :
      INTEGER * 2 FUNC
      PARAMETER ( FUNC = IO$_READVBLK .OR. IO$M_SETFNCT .OR.
     :                   IO$M_TIMED )

      INTEGER FNCT, TIMOUT
      PARAMETER ( FNCT = 1 )
      PARAMETER ( TIMOUT = 10 )

*    Local variables :
      INTEGER ISTAT, J, PAD, TEMP
*-

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

*   The DR11-W driver transfers words so ensure the number of bytes
*   is even
      IF ( MOD( NUMBUF, 2 ) .EQ. 0 ) THEN
         PAD = 0
      ELSE
         PAD = 1
      ENDIF

*   Read the buffer from the given channel
      ISTAT = SYS$QIOW( ,%VAL( ACHAN( DISPID ) ), %VAL( FUNC ), IOSB,,,
     :                  BYTBUF, %VAL( NUMBUF + PAD ),
     :                  %VAL( TIMOUT ), %VAL( FNCT ),, )

      IF ( ISTAT .NE. SS$_NORMAL ) THEN
         STATUS = IDI__IOERR
      ENDIF

*   Return the number of bytes read not counting any padding
      IF ( IOSB( 2 ) .LT. NUMBUF + PAD ) THEN
         NUMBUF = IOSB( 2 )
      ENDIF

*   Reverse an even number of bytes
      DO J = 1, NUMBUF + MOD( NUMBUF, 2 ), 2
         TEMP = BYTBUF( J )
         BYTBUF( J ) = BYTBUF( J + 1 )
         BYTBUF( J + 1 ) = TEMP
      ENDDO

  99  CONTINUE

      END

