*-----------------------------------------------------------------------
*+  IKNRCO - Read the context from the device

      SUBROUTINE IKNRCO ( DISPID, NCONF, STATUS )

*    Description :
*     This initilaises the Ikon, reading as much as it can from the
*     device.
*
*    Invocation :
*     CALL IKNRCO( DISPID, NCONF, STATUS )
*
*    Method :
*     Read in the characteristics and configurations from the
*     workstation description file.
*     Initialise all other characteristics not in the description file.
*     Set up the Ikon and the mouse ( GID ).
*     Read in the context from the Ikon.
*
*    Deficiencies :
*     Very non-standard Fortran - INTEGER * 2
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     November 1989
*     December 1989  Disable clipping
*     July 1990  Set memory visibilities
*     January 1991  Remove DTYPE
*     14 February 1992  Remove display of both planes and initialise the
*                       visibilities to zero.
*     October 1992  Set memory visibilities as undefined
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Display identifier
      INTEGER DISPID

*     Configuration number
      INTEGER NCONF

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMID)'
      INCLUDE 'IDIINC(IKN_COMINT)'
      INCLUDE 'IDIINC(IKN_COMLUT)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local variables :
      LOGICAL GOTCON

      BYTE B8( 4 ), BLUT( 6 ), IDBAND

      INTEGER * 2 WLUT( 3 ), WORDS( 50 )

      INTEGER B32, ICNMEM, INUM, ISTAT, J, K, LUTLEN, NDEF, NUMWOR

      REAL RNUM

      CHARACTER FNAME * 72, STRING * 64

*     Equivalence 4 bytes with one integer long word
      EQUIVALENCE ( B32, B8( 1 ) )

*     Equivalence the word and byte LUT arrays
      EQUIVALENCE ( WLUT( 1 ), BLUT( 1 ) )
*-

*   Save the characteristics if the devices are different
      IF ( ( CURRID .NE. DISPID ) .AND. ( CURRID .GT. 0 ) ) THEN
         CALL IKNOUT( STATUS )
         CALL IDSACO( STATUS )
      ENDIF

*   Obtain the correct description file for the device
      FNAME = 'IDI_DIR:IKNWDT.DAT'
      CURRID = DISPID

*   Do some initialisations
      GOTCON = .FALSE.

*   Open the workstation description table
      OPEN( UNIT = 1, FILE = FNAME, IOSTAT = ISTAT, STATUS = 'OLD',
     :      READONLY )

*   Check the status
      IF ( ISTAT .NE. 0 ) THEN
         STATUS = IDI__WDTER
         GOTO 99
      ENDIF

 9001 FORMAT( A64, I8 )
 9002 FORMAT( A64, L8 )
 9003 FORMAT( A64, F8.3 )

*   Read the device type
      READ( 1, 9001, ERR = 98 ) STRING, INUM

*   Read the characterisitics
      READ( 1, 9001, ERR = 97 ) STRING, CONNUM
      READ( 1, 9001, ERR = 97 ) STRING, CNPIX( 0 )
      READ( 1, 9001, ERR = 97 ) STRING, CNPIX( 1 )
      READ( 1, 9001, ERR = 97 ) STRING, CDISDE
      READ( 1, 9001, ERR = 97 ) STRING, CLUTDE
      READ( 1, 9001, ERR = 97 ) STRING, CNLUT
      READ( 1, 9001, ERR = 97 ) STRING, CNITT
      READ( 1, 9001, ERR = 97 ) STRING, CZOOMR( 0 )
      READ( 1, 9001, ERR = 97 ) STRING, CZOOMR( 1 )
      READ( 1, 9001, ERR = 97 ) STRING, CURN
      DO J = 0, CURN - 1
         READ( 1, 9001, ERR = 97 ) STRING, CURASH( J )
      ENDDO
      READ( 1, 9001, ERR = 97 ) STRING, CURNSH
      READ( 1, 9001, ERR = 97 ) STRING, CNLOC
      READ( 1, 9001, ERR = 97 ) STRING, CNREVA
      READ( 1, 9001, ERR = 97 ) STRING, CNIEVA
      READ( 1, 9001, ERR = 97 ) STRING, CNLEVA
      READ( 1, 9001, ERR = 97 ) STRING, CNCEVA
      READ( 1, 9001, ERR = 97 ) STRING, CNTRIG
      READ( 1, 9002, ERR = 97 ) STRING, CANROI
      READ( 1, 9001, ERR = 97 ) STRING, CNROI
      READ( 1, 9002, ERR = 97 ) STRING, CANBLI
      READ( 1, 9002, ERR = 97 ) STRING, CANSPL
      READ( 1, 9002, ERR = 97 ) STRING, CANINT
      READ( 1, 9002, ERR = 97 ) STRING, CANSNA
      READ( 1, 9002, ERR = 97 ) STRING, CANESC
      READ( 1, 9002, ERR = 97 ) STRING, CANDIA
      READ( 1, 9002, ERR = 97 ) STRING, CANDYN

*   Verify the given configuration number
      IF ( ( NCONF .GE. 0 ) .AND. ( NCONF .LE. CONNUM - 1 ) ) THEN
         NDEF = NCONF
      ELSE
         NDEF = 0
      ENDIF

*   Define the default configuration
      DO K = 0, CONNUM - 1
         READ( 1, 9001, ERR = 97 ) STRING, INUM
         IF ( K .EQ. NDEF ) THEN
            CONFIG = K
            GOTCON = .TRUE.
         ENDIF

*   Read the configuration
         READ( 1, 9001, ERR = 97 ) STRING, ICNMEM
         IF ( GOTCON ) CNMEM = ICNMEM
         DO J = 0, ICNMEM - 1
            READ( 1, 9001, ERR = 97 ) STRING, INUM
            IF ( GOTCON ) CONMEM( J ) = INUM
         ENDDO
         DO J = 0, ICNMEM - 1
            READ( 1, 9001, ERR = 97 ) STRING, INUM
            IF ( GOTCON ) CMEMDE( J ) = INUM
         ENDDO
         DO J = 0, ICNMEM - 1
            READ( 1, 9001, ERR = 97 ) STRING, INUM
            IF ( GOTCON ) CTWDIX( J ) = INUM
         ENDDO
         DO J = 0, ICNMEM - 1
            READ( 1, 9001, ERR = 97 ) STRING, INUM
            IF ( GOTCON ) CTWDIY( J ) = INUM
         ENDDO
         DO J = 0, ICNMEM - 1
            READ( 1, 9001, ERR = 97 ) STRING, INUM
         ENDDO
         DO J = 0, ICNMEM - 1
            READ( 1, 9003, ERR = 97 ) STRING, RNUM
            IF ( GOTCON ) CBLINK( J ) = RNUM
         ENDDO
         DO J = 0, ICNMEM - 1
            READ( 1, 9001, ERR = 97 ) STRING, INUM
            IF ( GOTCON ) CMEMTY( J ) = INUM
         ENDDO

      ENDDO

*   Skip the error handling
      GOTO 98

*   Abort if error during file read
  97  CONTINUE
      STATUS = IDI__WDTER
      CLOSE( UNIT = 1, IOSTAT = ISTAT )
      GOTO 99

*   Close the workstation description table
  98  CONTINUE
      CLOSE( UNIT = 1, IOSTAT = ISTAT )

*   Initialise the characterisitics not device specific
      CIMPLE = 0
      CONMOD = 1
      DO J = 0, CNMEM - 1

*   The visibility of the memories cannot be established so intialise them
*   to undefined (-1) so that a call to Set Memory Visibility does not
*   display a memory that is not visible on the screen
         CMEMVI( J ) = -1

*   Initialise the other characterisitics
         CLUTBI( J ) = J
         CDISBI( J ) = 7
         CITTBI( J ) = 0
         CTWSIX( J ) = CTWDIX( J )
         CTWSIY( J ) = CTWDIY( J )
         CTWOFX( J ) = 0
         CTWOFY( J ) = 0
         CTWDE( J ) = 0
         CTWDIR( J ) = 0
      ENDDO
      DO J = 0, CURN - 1
         CURBI( J ) = 0
         CURSHA( J ) = 0
      ENDDO
      DO J = 0, CNROI - 1
         CROIBI( J ) = 0
         CROICO( J ) = 0
         CROIVI( J ) = 0
      ENDDO
      DO J = 0, CNMEM - 1
         CSPXOF( J ) = 0
         CSPYOF( J ) = 0
      ENDDO
      CSPLXY( 0 ) = 0
      CSPLXY( 1 ) = 0
      CSPLON = 0
      DO J = 0, CNMEM - 1
         CINTVI( J ) = 0
      ENDDO

*   Zero the positions and zooms
      CLOCXY( 1 ) = 0
      CLOCXY( 2 ) = 0

*   Reset all the interactions to zero
      CINTN = 0
      DO J = 0, MAXINT - 1
         CINTTY( J ) = 0
         CINTID( J ) = 0
         COBJTY( J ) = 0
         COBJID( J ) = 0
         CINTOP( J ) = 0
         CEXTRN( J ) = 0
      ENDDO

*   Set the size of the memories and the priority list
      DO J = 0, CNMEM - 1
         CMEMSX( J ) = CTWDIX( J )
         CMEMSY( J ) = CTWDIY( J )
         CMEMPR( J ) = CNMEM - J
      ENDDO

*   Define the button controls for blinking in IIMBLM
      CBLINS = 0
      CBLDES = 1
      CBLSTP = 2

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

*   Disable clipping
*   Ikon command 203 = 'CB'X = Disable clipping mode
      WORDS( 4 ) = 203

*   Read in the look-up table for memory 0 from the Ikon
*   This allows for pictures not drawn with IDI.
*   Set frame buffer to read and write
*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7D'X = Set frame buffer to write
      WORDS( 5 ) = 124
      WORDS( 6 ) = 0
      WORDS( 7 ) = 125
      WORDS( 8 ) = 0

*   Set up the command to read in the LUT
*   Ikon command 84 = '54'X = Read palette colours
      LUTLEN = 2 ** CLUTDE
      WORDS( 9 ) = 84
      WORDS( 10 ) = LUTLEN - 1
      WORDS( 11 ) = 0
      NUMWOR = 11
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )

*   Read in the number of colours and start position
      NUMWOR = 2
      CALL IKNIBW( DISPID, NUMWOR, WLUT, STATUS )

*   Deal with the LUT's one at a time
      DO K = 1, LUTLEN

*   Read in the palette colour
         NUMWOR = 3
         CALL IKNIBW( DISPID, NUMWOR, WLUT, STATUS )
         B8( 1 ) = BLUT( 1 )
         CLUT0( 1, K ) = REAL( B32 ) / 255.0
         B8( 1 ) = BLUT( 3 )
         CLUT0( 2, K ) = REAL( B32 ) / 255.0
         B8( 1 ) = BLUT( 5 )
         CLUT0( 3, K ) = REAL( B32 ) / 255.0
      ENDDO

*   Read in the look-up table for memory 1 from the Ikon
*   This allows for pictures not drawn with IDI.
*   Set frame buffer to read and write
*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7D'X = Set frame buffer to write
      WORDS( 1 ) = 124
      WORDS( 2 ) = 1
      WORDS( 3 ) = 125
      WORDS( 4 ) = 1

*   Set up the command to read in the LUT
*   Ikon command 84 = '54'X = Read palette colours
      WORDS( 5 ) = 84
      WORDS( 6 ) = LUTLEN - 1
      WORDS( 7 ) = 0
      NUMWOR = 7
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )

*   Read in the number of colours and start position
      NUMWOR = 2
      CALL IKNIBW( DISPID, NUMWOR, WLUT, STATUS )

*   Deal with the LUT's one at a time
      DO K = 1, LUTLEN

*   Read in the palette colour
         NUMWOR = 3
         CALL IKNIBW( DISPID, NUMWOR, WLUT, STATUS )
         B8( 1 ) = BLUT( 1 )
         CLUT1( 1, K ) = REAL( B32 ) / 255.0
         B8( 1 ) = BLUT( 3 )
         CLUT1( 2, K ) = REAL( B32 ) / 255.0
         B8( 1 ) = BLUT( 5 )
         CLUT1( 3, K ) = REAL( B32 ) / 255.0
      ENDDO

*   Read in the memory positions from the Ikon
*   Set frame buffer to read
*   Ikon command 124 = '7C'X = Set frame buffer to read
      WORDS( 1 ) = 124
      WORDS( 2 ) = 1

*   Set current screen to base
*   Ikon command 204 = 'CC'X = Set current screen to base
      WORDS( 3 ) = 204

*   Read the origin offsets
*   Ikon command 105 = '69'X = Read register - 32 bit
*   Ikon register 107 = '6B'X = Base screen origin offset x-y
      WORDS( 4 ) = 105
      WORDS( 5 ) = 107
      NUMWOR = 5
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )
      NUMWOR = 2
      CALL IKNIBW( DISPID, NUMWOR, WLUT, STATUS )

*   Store this in the common block
      CSCROF( 1 ) = 0
      CSCROF( 2 ) = WLUT( 1 )

*   Read the zoom factors
*   Ikon command 103 = '67'X = Read register - 16 bit low
*   Ikon register 111 = '6F'X = Base screen zoom factor
      WORDS( 1 ) = 103
      WORDS( 2 ) = 111
      NUMWOR = 2
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )
      NUMWOR = 2
      CALL IKNIBB( DISPID, NUMWOR, B8, STATUS )

*   Store the zooms in the common block
      DO J = 0, CNMEM - 1
         CMEMZ( J ) = B8( 1 )
      ENDDO

*   Read the scroll factors
*   Ikon command 105 = '69'X = Read register - 32 bit
*   Ikon register 106 = '6A'X = Base screen pan scroll offset x-y
      WORDS( 1 ) = 105
      WORDS( 2 ) = 106
      NUMWOR = 2
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )
      NUMWOR = 2
      CALL IKNIBW( DISPID, NUMWOR, WLUT, STATUS )

*   Store the scrolls in the common block
      DO J = 0, CNMEM - 1
         CSCROX( J ) = -WLUT( 2 )
         CSCROY( J ) = WLUT( 1 ) - CSCROF( 2 )

*   The CMEM* arrays store the scrolls that have been requested by the
*   user not forced upon them by scrolls following a zoom. This
*   information cannot be recovered so make some assumtions. If the zoom
*   factor is 0 then assume the scrolls have been requested and are
*   therefore the same as CSCRO*, otherwise assume any scroll is due to
*   a zoom and therefore CMEM* = 0.
         IF ( CMEMZ( J ) .EQ. 0 ) THEN
            CMEMX( J ) = -WLUT( 2 )
            CMEMY( J ) = WLUT( 1 ) - CSCROF( 2 )
         ELSE
            CMEMX( J ) = 0
            CMEMY( J ) = 0
         ENDIF
      ENDDO

*   Read the cursor information
*   Ikon command 103 = '67'X = Read register - 16 bit low
*   Ikon register 56 = '38'X = Cursor control register
      WORDS( 1 ) = 103
      WORDS( 2 ) = 56
      NUMWOR = 2
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )
      NUMWOR = 2
      CALL IKNIBB( DISPID, NUMWOR, B8, STATUS )

*   Store the cursor visibility in the common block
      DO J = 0, CURN - 1
         CURVIS( J ) = IDBAND( B8( 2 ), 128 )
      ENDDO

*   Read the current position
*   Ikon command 165 = 'A5'X = Return current position
      WORDS( 1 ) = 165
      NUMWOR = 1
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )
      NUMWOR = 2
      CALL IKNIBW( DISPID, NUMWOR, WLUT, STATUS )

*   Store the current position in the common block
      DO J = 0, CURN - 1
         CURX( J ) = WLUT( 1 )
         CURY( J ) = WLUT( 2 )
      ENDDO

  99  CONTINUE

      END

