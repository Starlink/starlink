*-----------------------------------------------------------------------
*+  IKNRMY - Read Ikon Memory

      SUBROUTINE IKNRMY ( DISPID, MEMID, NPIX, XSTART, YSTART, DEPTH,
     :                    PACK, ITTON, IMAGE, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIMRMY.
*     The arguments are identical to those in IIMRMY.
*
*    Invocation :
*     CALL IKNRMY ( DISPID, MEMID, NPIX, XSTART, YSTART, DEPTH,
*    :              PACK, ITTON, IMAGE, STATUS )
*
*    Method :
*     This routine is complicated by the possibility of having partial
*     lines of pixels to read at the beginning and end of a block of
*     pixels.
*     Verify the input arguments.
*     See if there is a partial row starting the area.
*     Calculate the dimensions of the main rectangular block.
*     See if there is a partial row ending the area.
*     Prepare the Ikon to send the data.
*     Read the first partial line if required.
*     Read the main rectangular block dealing with it one row at a time.
*     Read the last partial line if required.
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
*     January  1993  Initialise PLEFT
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

*     Number of pixels
      INTEGER NPIX

*     Start position in x
      INTEGER XSTART

*     Start position in y
      INTEGER YSTART

*     Data depth. Bits per pixel
      INTEGER DEPTH

*     Packing factor
      INTEGER PACK

*     ITT flag
      LOGICAL ITTON

*    Export :
*     Image data
      INTEGER IMAGE( * )

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMID)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local variables :
      BYTE BSPACE( MAXBUF * 2 )

      INTEGER * 2 WORDS( 9 )

      INTEGER BROWS, EROWS, J, NROWS, NB, NUMINT, NUMWOR, OFFSET, PCOLS
      INTEGER PLEFT, PROWS, WCOLS, WROWS, XBEGIN, XEND, XMIN, YMIN
*-

*   Recover the characteristics if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNOUT( STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
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

*   Use local variables for the window sizes
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

*   Depth and pack are verified in IDIPIL

*   Initialise the flag to remember the number of pixels in incomplete words
      PLEFT = 0

*   See if there is an incomplete row starting the area
      IF ( XSTART .GT. 0 ) THEN
         XBEGIN = MIN( NPIX, WCOLS - XSTART )
         BROWS = 1
      ELSE
         XBEGIN = 0
         BROWS = 0
      ENDIF

*   Calculate how many complete rows have to be read
      NROWS = ( NPIX - XBEGIN ) / WCOLS

*   The pixel area must fit inside the transfer window
      PROWS = MIN( WROWS - YSTART - BROWS, NROWS )
      PCOLS = WCOLS

*   See if there are any pixels are left
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

*   Set the frame buffer to read
*   Ikon command 124 = '7C'X = Set frame buffer to read
      WORDS( 5 ) = 124
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

*   Return pixel line
*   Ikon command 174 = 'AE'X = Return pixel line
         WORDS( 4 ) = 174
         WORDS( 5 ) = XBEGIN - 1

*   Send these commands and flush the buffer
         NUMWOR = 5
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         CALL IKNOUT( STATUS )

*   Read the data
         NB = XBEGIN
         CALL IKNIBB( DISPID, NB, BSPACE, STATUS )

*   Transform this through the ITT
         IF ( ITTON ) THEN
         ENDIF

*   Pack the data into integers
         CALL IDIPIL( BSPACE, NB, PACK, DEPTH, PLEFT, IMAGE,
     :                NUMINT, STATUS )
      ENDIF

*   Set up the command to read the bulk of the image from the frame
*   buffer allowing for the load direction in CTWDIR
*   Ikon command 164 = 'A4'X = Move to
      IF ( PROWS .GT. 0 ) THEN
         WORDS( 1 ) = 164
         WORDS( 2 ) = XMIN
         IF ( CTWDIR( MEMID ) .EQ. 0 ) THEN
            WORDS( 3 ) = YMIN + BROWS
         ELSE
            WORDS( 3 ) = YMIN + EROWS + PROWS - 1
         ENDIF

*   Ikon command 175 = 'AF'X = Return pixel area
         WORDS( 4 ) = 175

*   Pass the height and width of the pixel area
         WORDS( 5 ) = PCOLS - 1
         WORDS( 6 ) = PROWS - 1

*   Send these commands and flush the buffer
         NUMWOR = 6
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         CALL IKNOUT( STATUS )

*   Read the image data from the Ikon, one row at a time
         DO J = 0, PROWS - 1

*   Read in one line of memory into the work space
            NB = PCOLS
            CALL IKNIBB( DISPID, NB, BSPACE, STATUS )

*   Transform this through the ITT
            IF ( ITTON ) THEN
            ENDIF

*   Pack the data into integers
            OFFSET = ( XBEGIN + J * NB ) / PACK + 1
            CALL IDIPIL( BSPACE, NB, PACK, DEPTH, PLEFT,
     :                   IMAGE( OFFSET ), NUMINT, STATUS )

         ENDDO
      ENDIF

*   See if there is an incomplete line to read and room to get it
*   from the transfer window
*   Ikon command 164 = 'A4'X = Move to
      IF ( ( XEND .GT. 0 ) .AND.
     :     ( WROWS - YSTART - PROWS - BROWS .GT. 0 ) ) THEN
         WORDS( 1 ) = 164
         WORDS( 2 ) = XMIN
         IF ( CTWDIR( MEMID ) .EQ. 0 ) THEN
            WORDS( 3 ) = YMIN + PROWS + BROWS
         ELSE
            WORDS( 3 ) = YMIN
         ENDIF

*   Return pixel line
*   Ikon command 174 = 'AE'X = Return pixel line
         WORDS( 4 ) = 174
         WORDS( 5 ) = XEND - 1

*   Send these commands and flush the buffer
         NUMWOR = 5
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         CALL IKNOUT( STATUS )

*   Read the data
         NB = XEND
         CALL IKNIBB( DISPID, NB, BSPACE, STATUS )

*   Transform this through the ITT
         IF ( ITTON ) THEN
         ENDIF

*   Pack the data into integers
         OFFSET = ( XBEGIN + PROWS * PCOLS ) / PACK + 1
         CALL IDIPIL( BSPACE, NB, PACK, DEPTH, PLEFT,
     :                IMAGE( OFFSET ), NUMINT, STATUS )
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

