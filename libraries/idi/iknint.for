*-----------------------------------------------------------------------
*+  IKNINT - Initialise the Ikon characteristics

      SUBROUTINE IKNINT ( DISPID, NCONF, STATUS )

*    Description :
*     This resets the Ikon to its default settings.
*
*    Invocation :
*     CALL IKNINT( DISPID, NCONF, STATUS )
*
*    Method :
*     Read in the characteristics and configurations from the
*     workstation description file.
*     Initialise all other characteristics not in the description file.
*     Set up the Ikon and the mouse ( GID ).
*     Set up the default linear ( grey ) look-up tables.
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
*     November 1988
*     July 1990  Set memory visibilities
*     January 1991  Remove DTYPE
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
      INCLUDE 'IDIINC(IKN_COMINT)'
      INCLUDE 'IDIINC(IKN_COMPOS)'
      INCLUDE 'IDIINC(IKN_COMID)'

*    Local variables :
      LOGICAL GOTCON

      INTEGER * 2 WORDS( 50 )

      INTEGER ICNMEM, INUM, ISTAT, J, K, LUTLEN, NDEF, NUMWOR

      REAL GMAX, GREY, RNUM, VLUT( 3, MAXCOL )

      CHARACTER FNAME * 72, STRING * 64
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

*   Initialise the other characterisitics
      CIMPLE = 0
      CONMOD = 1
      DO J = 0, CNMEM - 1
         CMEMVI( J ) = 1
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
         CURCOL( J ) = 0
         CURVIS( J ) = 0
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
      DO J = 0, MAXMEM - 1
         CMEMX( J ) = 0
         CMEMY( J ) = 0
         CMEMZ( J ) = 0
         CSCROX( J ) = 0
         CSCROY( J ) = 0
      ENDDO
      DO J = 0, MAXCUR - 1
         CURX( J ) = 0
         CURY( J ) = 0
      ENDDO
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

*   Set up the graphics input device
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

*   Ikon address 74 = '4A'X = Aux port setup. 227 = 11100011
      WORDS( 16 ) = 96
      WORDS( 17 ) = 74
      WORDS( 18 ) = 227

*   Enable the GID
      WORDS( 19 ) = 96
      WORDS( 20 ) = 24
      WORDS( 21 ) = 128

*   To use the full 1024 x 1024 frame buffer have to move the origin
*   of the frame buffer, scroll the screen and reset the clipping
*   Ikon command 203 = 'CB'X = Disable clipping mode
      WORDS( 22 ) = 203

*   Ikon command 217 = 'D9'X = Move origin position
      WORDS( 23 ) = 217
      WORDS( 24 ) = 0
      WORDS( 25 ) = IKON_SCROLL

*   Ikon command 219 = 'DB'X = Pan / scroll current screen
      WORDS( 26 ) = 219
      WORDS( 27 ) = 0
      WORDS( 28 ) = IKON_SCROLL

*   Display all memories
*   Ikon command 204 = 'CC'X = Set current screen to base
*   Ikon command 212 = 'D4'X = Unblank currently selected screen
*   Ikon command 92 = '5C'X = Set frame grab control latch
      WORDS( 29 ) = 204
      WORDS( 30 ) = 212
      WORDS( 31 ) = 92
      WORDS( 32 ) = 1

*   Send commands to Ikon
      NUMWOR = 32
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   Save the scroll offsets in the common block
      CSCROF( 1 ) = 0
      CSCROF( 2 ) = IKON_SCROLL

*   Set up the default linear look up table
      LUTLEN = 2 ** CLUTDE
      GMAX = REAL( LUTLEN - 1 )
      DO J = 1, LUTLEN
         GREY = REAL( J - 1 ) / GMAX
         VLUT( 1, J ) = GREY
         VLUT( 2, J ) = GREY
         VLUT( 3, J ) = GREY
      ENDDO
      CALL IKNUPD( DISPID, STATUS )
      CALL IKNWLT( DISPID, 0, 0, LUTLEN, VLUT, STATUS )
      CALL IKNWLT( DISPID, 1, 0, LUTLEN, VLUT, STATUS )

  99  CONTINUE

      END

