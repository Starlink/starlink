*-----------------------------------------------------------------------
*+  IKNOPN - Open Ikon

      SUBROUTINE IKNOPN ( DEVNAM, DISPID, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIDOPN.
*     The arguments are identical to those in IIDOPN.
*
*    Invocation :
*     CALL IKNOPN( DEVNAM, DISPID, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     See if there is any room in the common blocks for this device.
*     Assign an I/O channel to this device.
*     Wait until the Ikon is ready to receive.
*     Recover the context of the device, or if this fails reset to the
*     default state.
*     Enable the Ikon mouse ( GID ).
*
*    Deficiencies :
*     Very non-standard Fortran - INTEGER * 2
*     Uses VAX system calls - SYS$ASSIGN, SYS$QIOW, LIB$WAIT
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     November 1988
*     November 1989  Added clear flag
*     May      1990  Clear zone macros
*     January  1991  Remove DTYPE
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
*     Name of device to open
      CHARACTER * ( * ) DEVNAM

*    Export :
*     Identifier for display
      INTEGER DISPID

*    Status :
      INTEGER STATUS

*    External references :
      EXTERNAL IDI_DATID

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMID)'

*    Local Constants :
      INTEGER * 2 FUNC
      PARAMETER ( FUNC = IO$_WRITEVBLK )

*    Local variables :
      LOGICAL GOTONE

      INTEGER * 2 CHAN, IOSB( 4 ), WORDS( 29 )
      INTEGER J, ISTAT, NOTSET, NUMWOR
*-

*   Look for an empty slot in the common block storage
      J = 1
      GOTONE = .FALSE.
      DO WHILE ( ( .NOT. GOTONE ) .AND. ( J .LE. MAXID ) )
         IF ( ACHAN( J ) .EQ. 0 ) THEN
            GOTONE = .TRUE.
         ELSE
            J= J + 1
         ENDIF
      ENDDO

*   If there is no room in the common block then exit with an error
      IF ( .NOT. GOTONE ) THEN
         DISPID = -1
         STATUS = IDI__COOVF
         GOTO 99
      ELSE
         DISPID = J
      ENDIF

*   Assign a channel to the device
      ISTAT = SYS$ASSIGN( DEVNAM, CHAN,, )
      IF ( ISTAT .NE. SS$_NORMAL ) THEN
         STATUS = IDI__IOERR
         GOTO 99
      ENDIF

*   Store the channel number and type in the common block
      ACHAN( DISPID ) = CHAN
      ONOFF( DISPID ) = 1

*   Wait until the device is ready.
*   This occurs when status line A goes low
      NOTSET = 1
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

*   Get the context from the device if CLRFG = 1
      IF ( CLRFG .EQ. 1 ) THEN
         CALL IKNRCO( DISPID, 0, STATUS )

*   Otherwise recover the context of this device from the file
      ELSE
         CALL IDRECO( DISPID, STATUS )
      ENDIF

*   If recovery failed initialise the Ikon to its default setting
      IF ( STATUS .EQ. IDI__NOREC ) THEN
         STATUS = IDI__OK
         CONFIG = 0
         CALL IKNRST( DISPID, STATUS )
      ENDIF

*   Set up the Ikon the way IDI likes it :-
*   Set non-buffered mode
*   Ikon command 96 = '60'X = Set register 8 bit
*   Ikon register 63 = '3F'X = Parameter mode
*   Bit 0 - 1 = 0, 1, 2, 3 = Bits per pixel. 0 = 8 bits per pixel
*   Bit 2 - 3 = 0, 4, 8, 12 = Coordinate system. 0 = 16 bit abs coords
*   Bit 4 = 16 = Word order. 0 = HILO, 16 = LOHI
*   Bit 5 = 32 = Non buffered-mode
*   Bit 7 = 128 = Direction. 0 = up screen, 128 = down screen
      WORDS( 1 ) = 96
      WORDS( 2 ) = 63
      WORDS( 3 ) = 32

*   Set up the graphics input device
*   Ikon command 99 = '63'X = Set register 32 bit
*   Ikon address 26 = '1A'X = Graphics input device maximum xy
      WORDS( 4 ) = 99
      WORDS( 5 ) = 26
      WORDS( 6 ) = CTWDIY( 0 )
      WORDS( 7 ) = CTWDIX( 0 )

*   Clamp the GID by putting the size to zero
*   Ikon address 28 = '1C'X = Graphics input device size xy
      WORDS( 8 ) = 99
      WORDS( 9 ) = 28
      WORDS( 10 ) = 0
      WORDS( 11 ) = 0

*   Ikon address 44 = '2C'X = Zone size xy
      WORDS( 12 ) = 99
      WORDS( 13 ) = 44
      WORDS( 14 ) = CTWDIY( 0 )
      WORDS( 15 ) = CTWDIX( 0 )

*   Make sure the zone macros are zero
*   Ikon register 22 = '16'X = Zone macros 0, 1, 2 and 3
*   Ikon register 23 = '17'X = Zone macros 4, 5, 6 and 7
      WORDS( 16 ) = 99
      WORDS( 17 ) = 22
      WORDS( 18 ) = 0
      WORDS( 19 ) = 0
      WORDS( 20 ) = 99
      WORDS( 21 ) = 23
      WORDS( 22 ) = 0
      WORDS( 23 ) = 0

*   Ikon address 74 = '4A'X = Aux port setup. 227 = 11100011
      WORDS( 24 ) = 96
      WORDS( 25 ) = 74
      WORDS( 26 ) = 227

*   Enable the GID
      WORDS( 27 ) = 96
      WORDS( 28 ) = 24
      WORDS( 29 ) = 128

*   Send commands to Ikon
      NUMWOR = 29
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )

  99  CONTINUE

      END

