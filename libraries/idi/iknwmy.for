*-----------------------------------------------------------------------
*+  IKNWMY - Write Memory for Ikon

      SUBROUTINE IKNWMY ( DISPID, MEMID, IMAGE, NPIX, DEPTH, PACK,
     :                    XSTART, YSTART, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIMWMY.
*     The arguments are identical to those in IIMWMY.
*
*    Invocation :
*     CALL IKNWMY( DISPID, MEMID, IMAGE, NPIX, DEPTH, PACK,
*    :             XSTART, YSTART, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     This routine is complicated by the possibility of having partial
*     lines of pixels to send at the beginning and end of a block of
*     pixels.
*     Verify the input arguments.
*     See if there is a partial row starting the area.
*     Calculate the dimensions of the main rectangular block.
*     See if there is a partial row ending the area.
*     Prepare the Ikon to receive the data.
*     Send the first partial line if required.
*     Send the main rectangular block dealing with it one row at a time.
*     Send the last partial line if required.
*     Tidy up.
*
*    Deficiencies :
*     Very non-standard Fortran - BYTE, INTEGER * 2
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     December 1988
*     February 1990  Put cursor shapes into IKNCON
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

*     Memory identifier
      INTEGER MEMID

*     Image data
      INTEGER IMAGE( * )

*     Number of pixels
      INTEGER NPIX

*     Data depth. Bits per pixel
      INTEGER DEPTH

*     Packing factor. Number of pixels per longword
      INTEGER PACK

*     Start position in X
      INTEGER XSTART

*     Start position in Y
      INTEGER YSTART

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local variables :
      BYTE BSPACE( MAXBUF * 2 )

      INTEGER * 2 WORDS( 9 )

      INTEGER BROWS, EROWS, J, NROWS, NUMBYT, NUMWOR, OFFSET, PCOLS
      INTEGER PLEFT, PROWS, WCOLS, WROWS, XBEGIN, XEND, XMIN, YMIN
*-

*   Recover the characteristics if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNOUT( STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            STATUS = IDI__NOREC
            GOTO 99
         ENDIF
      ENDIF

*   Verify the memory identifier
      IF ( ( MEMID .LT. 0 ) .OR. ( MEMID .GT. CNMEM - 1 ) ) THEN
         STATUS = IDI__INMID
         GOTO 99
      ENDIF

*   Verify the number of pixels
      IF ( NPIX .LE. 0 ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   Use local variables for the transfer window dimensions
      WCOLS = CTWSIX( MEMID )
      WROWS = CTWSIY( MEMID )

*   Verify the starting position
      IF ( ( XSTART .LT. 0 ) .OR. ( XSTART .GT. WCOLS - 1 ) ) THEN
         STATUS = IDI__TWOVF
         GOTO 99
      ENDIF
      IF ( ( YSTART .LT. 0 ) .OR. ( YSTART .GT. WROWS - 1 ) ) THEN
         STATUS = IDI__TWOVF
         GOTO 99
      ENDIF

*   Depth and pack are verified in IDIPIB

*   See if there is an incomplete row starting the area
      IF ( XSTART .GT. 0 ) THEN
         XBEGIN = MIN( NPIX, WCOLS - XSTART )
         BROWS = 1
      ELSE
         XBEGIN = 0
         BROWS = 0
      ENDIF

*   Calculate how many complete rows have to be sent
      NROWS = ( NPIX - XBEGIN ) / WCOLS

*   The pixel area must fit inside the transfer window
      PROWS = MIN( WROWS - YSTART - BROWS, NROWS )
      PCOLS = WCOLS

*   See if there are any pixels left
      XEND = NPIX - XBEGIN - NROWS * WCOLS

*   See if there is enough room in the window for this extra line
      IF ( XEND .GT. 0 ) THEN
         IF ( PROWS + BROWS .LT. WROWS - YSTART ) THEN
            EROWS = 1
         ELSE
            EROWS = 0
         ENDIF
      ELSE
         EROWS = 0
      ENDIF

*   Remember the bottom left corner of this area
      XMIN = CTWOFX( MEMID )
      YMIN = CTWOFY( MEMID ) + YSTART

*   Load direction. Use the same set up as in IKNINT.
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
      IF ( CTWDIR( MEMID ) .EQ. 1 ) THEN
         WORDS( 3 ) = WORDS( 3 ) + 128
      ENDIF

*   Switch off any cursors
*   Ikon command 192 = 'C0'X = Crosshair cursor off
      WORDS( 4 ) = 192

*   Set frame buffer to write
*   Ikon command 125 = '7D'X = Set frame buffer to write
      WORDS( 5 ) = 125
      WORDS( 6 ) = MEMID
      NUMWOR = 6
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   If there is an incomplete line at the start then send it
*   allowing for the load direction in CTWDIR
*   Ikon command 164 = 'A4'X = Move to
      IF ( XBEGIN .GT. 0 ) THEN
         WORDS( 1 ) = 164
         WORDS( 2 ) = XMIN + XSTART
         IF ( CTWDIR( MEMID ) .EQ. 0 ) THEN
            WORDS( 3 ) = YMIN
         ELSE
            WORDS( 3 ) = YMIN + EROWS + PROWS + BROWS - 1
         ENDIF

*   Paint pixel line
*   Ikon command 172 = 'AC'X = Paint pixel line
         WORDS( 4 ) = 172
         WORDS( 5 ) = XBEGIN - 1

*   Send these commands
         NUMWOR = 5
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   Pack the data into contiguous bytes
         CALL IDIPIB( IMAGE, XBEGIN, PACK, DEPTH, PLEFT, BSPACE,
     :                NUMBYT, STATUS )

*   Send the data.
*   The Ikon requires an even number of bytes for each line, so send an
*   extra byte if the number is odd. This pixel is not displayed.
         IF ( MOD( NUMBYT, 2 ) .NE. 0 ) THEN
            NUMBYT = NUMBYT + 1
         ENDIF
         CALL IKNOBB( DISPID, NUMBYT, BSPACE, STATUS )
      ENDIF

*   Set up the command to copy the bulk of the image to the frame buffer
*   allowing for the load direction in CTWDIR
*   Ikon command 164 = 'A4'X = Move to
      IF ( PROWS .GT. 0 ) THEN
         WORDS( 1 ) = 164
         WORDS( 2 ) = XMIN
         IF ( CTWDIR( MEMID ) .EQ. 0 ) THEN
            WORDS( 3 ) = YMIN + BROWS
         ELSE
            WORDS( 3 ) = YMIN + EROWS + PROWS - 1
         ENDIF

*   Ikon command 173 = 'AD'X = Paint pixel area
         WORDS( 4 ) = 173

*   Pass the height and width of the pixel area
         WORDS( 5 ) = PCOLS - 1
         WORDS( 6 ) = PROWS - 1

*   Send these commands
         NUMWOR = 6
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   Send the image to the Ikon, one row at a time
         DO J = 0, PROWS - 1

*   Pack the data into contiguos bytes in the work space
            OFFSET = ( XBEGIN + J * PCOLS ) / PACK + 1
            CALL IDIPIB( IMAGE( OFFSET ), PCOLS, PACK, DEPTH, PLEFT,
     :                   BSPACE, NUMBYT, STATUS )

*   Send the data.
*   The Ikon requires an even number of bytes for each line, so send an
*   extra byte if the number is odd. This pixel is not displayed.
            IF ( MOD( NUMBYT, 2 ) .NE. 0 ) THEN
               NUMBYT = NUMBYT + 1
            ENDIF
            CALL IKNOBB( DISPID, NUMBYT, BSPACE, STATUS )

         ENDDO
      ENDIF

*   See if there is an incomplete line to send and room to put it
*   in the transfer window
      IF ( ( XEND .GT. 0 ) .AND.
     :     ( WROWS - YSTART - PROWS - BROWS .GT. 0 ) ) THEN

*   Allow for the load direction in CTWDIR
*   Ikon command 164 = 'A4'X = Move to
         WORDS( 1 ) = 164
         WORDS( 2 ) = XMIN
         IF ( CTWDIR( MEMID ) .EQ. 0 ) THEN
            WORDS( 3 ) = YMIN + PROWS + BROWS
         ELSE
            WORDS( 3 ) = YMIN
         ENDIF

*   Paint pixel line
*   Ikon command 172 = 'AC'X = Paint pixel line
         WORDS( 4 ) = 172
         WORDS( 5 ) = XEND - 1

*   Send these commands
         NUMWOR = 5
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   Pack data into contiguous bytes in the work space
         OFFSET = ( XBEGIN + PROWS * PCOLS ) / PACK + 1
         CALL IDIPIB( IMAGE( OFFSET ), XEND, PACK, DEPTH, PLEFT,
     :                BSPACE, NUMBYT, STATUS )

*   Send the data.
*   The Ikon requires an even number of bytes for each line, so send an
*   extra byte if the number is odd. This pixel is not displayed.
         IF ( MOD( NUMBYT, 2 ) .NE. 0 ) THEN
            NUMBYT = NUMBYT + 1
         ENDIF
         CALL IKNOBB( DISPID, NUMBYT, BSPACE, STATUS )

      ENDIF

*   Reinstate any cursors
      DO J = 0, CURN - 1
         IF ( CURVIS( J ) .NE. 0 ) THEN
            CALL IKNCON( DISPID, J, STATUS )
         ENDIF
      ENDDO

*   Flag an error if the number of pixels overflowed the window
      IF ( NPIX .GT. XBEGIN + PROWS * PCOLS + XEND ) THEN
         STATUS = IDI__TWOVF
      ENDIF

  99  CONTINUE

      END

