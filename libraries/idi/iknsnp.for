*-----------------------------------------------------------------------
*+  IKNSNP - Create Snapshot

      SUBROUTINE IKNSNP ( DISPID, CMODE, NPIX, XSTART, YSTART, DEPTH,
     :                    PACK, IMAGE, STATUS )
      
*    Description :
*     This does the Ikon specific work for the IDI routine IIDSNP.
*     The arguments are identical to those in IIDSNP.
*
*    Invocation :
*     CALL IKNSNP( DISPID, CMODE, NPIX, XSTART, YSTART, DEPTH,
*    :             PACK, IMAGE, STATUS )
*
*    Method :
*     This routine is complicated by the possibility of having partial
*     lines of pixels to read at the beginning and end of a block of
*     pixels.
*     Verify the input arguments.
*     Read in the look-up tables from the Ikon.
*     See if there is a partial row starting the area.
*     Calculate the dimensions of the main rectangular block.
*     See if there is a partial row ending the area.
*     Prepare the Ikon to receive the data.
*     Read the first partial line if required from the base and overlay
*     memories. Check every pixel to see if the base or overlay memory
*     is showing.
*     Convert the memory colour index into a greyscale value using the
*     appropriate look-up table. If the mode is pseudo-colour then
*     use the coulour to greyscale conversion equation ( from PGPLOT )
*     intensity = 0.30 * R + 0.59 * G + 0.11 * R
*     Read the main rectangular block dealing with it one row at a time
*     checking every pixel for memory visibility.
*     Read the last partial line if required checking every pixel for
*     memory visbility.
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
*     May 1989
*     Sep 1989  Convert memory indices to colours with the LUT's
*     Feb 1990  Put cursor shapes into IKNCON
*     Apr 1991  Calculate LUT length from LUT depth
*    endhistory

*    Type Definitions :
      IMPLICIT NONE
      
*    Global constants :
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'
      
*    Import :
*     Display identifier
      INTEGER DISPID

*     Color mode
      INTEGER CMODE

*     Number of pixels
      INTEGER NPIX

*     Data position in x
      INTEGER XSTART

*     Data position in y
      INTEGER YSTART

*     Data depth ( bits / pixel )
      INTEGER DEPTH

*     Packing factor
      INTEGER PACK

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

*    Local Constants :
*     These are the conversion factors for 255 intensity levels
*     i.e. RED = 255 * 0.30, GREEN = 255 * 0.59, BLUE = 255 * 0.11
      INTEGER RED, GREEN, BLUE, WHITE
      PARAMETER ( RED = 77 )
      PARAMETER ( GREEN = 150 )
      PARAMETER ( BLUE = 28 )
      PARAMETER ( WHITE = 255 )

*    Local variables :
      BYTE BSPAC0( MAXBUF * 2 ), BSPAC1( MAXBUF * 2 ), B8( 4 ),
     :     BLUT( 6 )

      INTEGER * 2 WLUT( 3 ), WORDS( 15 )

      INTEGER BROWS, B32, EROWS, I, J, K, LUT( MAXBUF * 2 ), LUTLEN,
     :        NROWS, NB, NUMINT, NUMWOR, OFFSET, PCOLS, PLEFT, PROWS,
     :        WCOLS, WROWS, XBEGIN, XEND, XMIN, YMIN

      REAL VLUT0( 3, MAXCOL ), VLUT1( 3, MAXCOL )

*     Equivalence 4 bytes with one integer long word
      EQUIVALENCE ( B32, B8( 1 ) )

*     Equivalence the word and byte LUT arrays
      EQUIVALENCE ( WLUT( 1 ), BLUT( 1 ) )
*-

*   Recover the characteristics if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNOUT( STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            GOTO 99
         ENDIF
      ENDIF

*   Verify the color mode
      IF ( ( CMODE .LT. 0 ) .OR. ( CMODE .GT. 3 ) ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   Verify the number of pixels
      IF ( NPIX .LE. 0 ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   Use local variables for the base window sizes.
      WCOLS = CTWSIX( 0 )
      WROWS = CTWSIY( 0 )

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

*   Read in the look-up table for memory 0 from the Ikon
*   This allows for pictures not drawn with IDI.
*   Set frame buffer to read
*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7C'X = Set frame buffer to write
      WORDS( 1 ) = 124
      WORDS( 2 ) = 0
      WORDS( 3 ) = 125
      WORDS( 4 ) = 0

*   Set up the command to read in the LUT
*   Ikon command 84 = '54'X = Read palette colours
      LUTLEN = 2 ** CLUTDE
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
         VLUT0( 1, K ) = REAL( B32 ) / 255.0
         B8( 1 ) = BLUT( 3 )
         VLUT0( 2, K ) = REAL( B32 ) / 255.0
         B8( 1 ) = BLUT( 5 )
         VLUT0( 3, K ) = REAL( B32 ) / 255.0
      ENDDO

*   Read in the look-up table for memory 1 from the Ikon
*   This allows for pictures not drawn with IDI.
*   Set frame buffer to read
*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7C'X = Set frame buffer to write
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
         VLUT1( 1, K ) = REAL( B32 ) / 255.0
         B8( 1 ) = BLUT( 3 )
         VLUT1( 2, K ) = REAL( B32 ) / 255.0
         B8( 1 ) = BLUT( 5 )
         VLUT1( 3, K ) = REAL( B32 ) / 255.0
      ENDDO

*   Start reading the image
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
      XMIN = CTWOFX( 0 )
      YMIN = CTWOFY( 0 ) + YSTART

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
      IF ( CTWDIR( 0 ) .EQ. 1 ) THEN
         WORDS( 3 ) = WORDS( 3 ) + 128
      ENDIF

*   Switch off any cursors
*   Ikon command 192 = 'C0'X = Crosshair cursor off
      WORDS( 4 ) = 192
      NUMWOR = 4
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   If there is an incomplete line at the start then send it
*   Ikon command 164 = 'A4'X = Move to
      IF ( XBEGIN .GT. 0 ) THEN
         WORDS( 1 ) = 164
         WORDS( 2 ) = XMIN + XSTART
         WORDS( 3 ) = YMIN

*   Set the frame buffer to read the base memory
*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7C'X = Set frame buffer to write
         WORDS( 4 ) = 124
         WORDS( 5 ) = 0
         WORDS( 6 ) = 125
         WORDS( 7 ) = 0

*   Return pixel line
*   Ikon command 174 = 'AE'X = Return pixel line
         WORDS( 8 ) = 174
         WORDS( 9 ) = XBEGIN - 1

*   Send these commands and flush the buffer
         NUMWOR = 9
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         CALL IKNOUT( STATUS )

*   Read the data
         NB = XBEGIN
         CALL IKNIBB( DISPID, NB, BSPAC0, STATUS )

*   Set the frame buffer to read the overlay memory
*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7C'X = Set frame buffer to write
         WORDS( 1 ) = 124
         WORDS( 2 ) = 1
         WORDS( 3 ) = 125
         WORDS( 4 ) = 1

*   Return pixel line
*   Ikon command 174 = 'AE'X = Return pixel line
         WORDS( 5 ) = 174
         WORDS( 6 ) = XBEGIN - 1

*   Send these commands and flush the buffer
         NUMWOR = 6
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         CALL IKNOUT( STATUS )

*   Read the data
         NB = XBEGIN
         CALL IKNIBB( DISPID, NB, BSPAC1, STATUS )

*   If the base memory is not visible then use the overlay memory if
*   that is visible
*   Remember which look-up table is being used in the array LUT
         IF ( CMEMVI( 0 ) .EQ. 0 ) THEN
            DO K = 1, NB
               IF ( CMEMVI( 1 ) .EQ. 0 ) THEN
                  BSPAC0( K ) = 0
                  LUT( K ) = -1
               ELSE
                  BSPAC0( K ) = BSPAC1( K )
                  LUT( K ) = 1
               ENDIF
            ENDDO

*   If the base memory is visible then use the overlay memory if it
*   is visible and non-zero
*   Remember which look-up table is being used in the array LUT
         ELSE
            DO K = 1, NB
               LUT( K ) = 0
               IF ( ( CMEMVI( 1 ) .NE. 0 ) .AND.
     :              ( BSPAC1( K ) .NE. 0 ) ) THEN
                  BSPAC0( K ) = BSPAC1( K )
                  LUT( K ) = 1
               ENDIF
            ENDDO
         ENDIF

*   Convert every colour index into an actual coulour
         DO K = 1, NB

*   Put the memory colour index into an integer word
            B32 = 0
            B8( 1 ) = BSPAC0( K )

*   Add 1 to the colour index to make it scale between 1 and 256
            I = B32 + 1

*   Check which look-up table is being used
            IF ( LUT( K ) .EQ. 0 ) THEN

*   Use the appropriate mode
*   If it is pseudo-colour then use 0.30 * R + 0.59 * G + 0.11 * B
               IF ( CMODE .EQ. 0 ) THEN
                  B32 = NINT( VLUT0( 1, I ) * RED + VLUT0( 2, I ) *
     :                        GREEN + VLUT0( 3, I ) * BLUE )
                  BSPAC0( K ) = B8( 1 )
               ELSE
                  B32 = NINT( VLUT0( CMODE, I ) * WHITE )
                  BSPAC0( K ) = B8( 1 )
               ENDIF

            ELSEIF ( LUT( K ) .EQ. 1 ) THEN

*   Use the appropriate mode
*   If it is pseudo-colour then use 0.30 * R + 0.59 * G + 0.11 * B
               IF ( CMODE .EQ. 0 ) THEN
                  B32 = NINT( VLUT1( 1, I ) * RED + VLUT1( 2, I ) *
     :                        GREEN + VLUT1( 3, I ) * BLUE )
                  BSPAC0( K ) = B8( 1 )
               ELSE
                  B32 = NINT( VLUT1( CMODE, I ) * WHITE )
                  BSPAC0( K ) = B8( 1 )
               ENDIF

*   If the look up table is not recognised put the colour = 0
            ELSE
               BSPAC0( K ) = 0
            ENDIF
         ENDDO

*   Pack the data into integers
         CALL IDIPIL( BSPAC0, NB, PACK, DEPTH, PLEFT, IMAGE,
     :                NUMINT, STATUS )
      ENDIF

*   Set up the loop to read the bulk of the image from the frame
      IF ( PROWS .GT. 0 ) THEN

*   Read the image data from the Ikon, one row at a time
         DO J = 0, PROWS - 1

*   Ikon command 164 = 'A4'X = Move to
            WORDS( 1 ) = 164
            WORDS( 2 ) = XMIN
            WORDS( 3 ) = YMIN + BROWS + J

*   Set the frame buffer to read the base memory
*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7C'X = Set frame buffer to write
            WORDS( 4 ) = 124
            WORDS( 5 ) = 0
            WORDS( 6 ) = 125
            WORDS( 7 ) = 0

*   Return pixel line
*   Ikon command 174 = 'AE'X = Return pixel line
            WORDS( 8 ) = 174
            WORDS( 9 ) = PCOLS - 1

*   Send these commands and flush the buffer
            NUMWOR = 9
            CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
            CALL IKNOUT( STATUS )

*   Read the data
            NB = PCOLS
            CALL IKNIBB( DISPID, NB, BSPAC0, STATUS )

*   Set the frame buffer to read the overlay memory
*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7C'X = Set frame buffer to write
            WORDS( 1 ) = 124
            WORDS( 2 ) = 1
            WORDS( 3 ) = 125
            WORDS( 4 ) = 1

*   Return pixel line
*   Ikon command 174 = 'AE'X = Return pixel line
            WORDS( 5 ) = 174
            WORDS( 6 ) = PCOLS - 1

*   Send these commands and flush the buffer
            NUMWOR = 6
            CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
            CALL IKNOUT( STATUS )

*   Read the data
            NB = PCOLS
            CALL IKNIBB( DISPID, NB, BSPAC1, STATUS )

*   If the base memory is not visible then use the overlay memory if
*   that is visible
            IF ( CMEMVI( 0 ) .EQ. 0 ) THEN
               DO K = 1, NB
                  IF ( CMEMVI( 1 ) .EQ. 0 ) THEN
                     BSPAC0( K ) = 0
                     LUT( K ) = -1
                  ELSE
                     BSPAC0( K ) = BSPAC1( K )
                     LUT( K ) = 1
                  ENDIF
               ENDDO

*   If the base memory is visible then use the overlay memory if it
*   is visible and non-zero
            ELSE
               DO K = 1, NB
                  LUT( K ) = 0
                  IF ( ( CMEMVI( 1 ) .NE. 0 ) .AND.
     :                 ( BSPAC1( K ) .NE. 0 ) ) THEN
                     BSPAC0( K ) = BSPAC1( K )
                     LUT( K ) = 1
                  ENDIF
               ENDDO
            ENDIF

*   Convert every colour index into an actual coulour
            DO K = 1, NB

*   Put the memory colour index into an integer word
               B32 = 0
               B8( 1 ) = BSPAC0( K )

*   Add 1 to the colour index to make it scale between 1 and 256
               I = B32 + 1

*   Check which look-up table is being used
               IF ( LUT( K ) .EQ. 0 ) THEN

*   Use the appropriate mode
*   If it is pseudo-colour then use 0.30 * R + 0.59 * G + 0.11 * B
                  IF ( CMODE .EQ. 0 ) THEN
                     B32 = NINT( VLUT0( 1, I ) * RED + VLUT0( 2, I ) *
     :                           GREEN + VLUT0( 3, I ) * BLUE )
                     BSPAC0( K ) = B8( 1 )
                  ELSE
                     B32 = NINT( VLUT0( CMODE, I ) * WHITE )
                     BSPAC0( K ) = B8( 1 )
                  ENDIF

               ELSEIF ( LUT( K ) .EQ. 1 ) THEN

*   Use the appropriate mode
*   If it is pseudo-colour then use 0.30 * R + 0.59 * G + 0.11 * B
                  IF ( CMODE .EQ. 0 ) THEN
                     B32 = NINT( VLUT1( 1, I ) * RED + VLUT1( 2, I ) *
     :                           GREEN + VLUT1( 3, I ) * BLUE )
                     BSPAC0( K ) = B8( 1 )
                  ELSE
                     B32 = NINT( VLUT1( CMODE, I ) * WHITE )
                     BSPAC0( K ) = B8( 1 )
                  ENDIF

*   If the look up table is not recognised put the colour = 0
               ELSE
                  BSPAC0( K ) = 0
               ENDIF
            ENDDO

*   Pack the data into integers
            OFFSET = ( XBEGIN + J * NB ) / PACK + 1
            CALL IDIPIL( BSPAC0, NB, PACK, DEPTH, PLEFT,
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
         WORDS( 3 ) = YMIN + PROWS + BROWS

*   Set the frame buffer to read the base memory
*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7C'X = Set frame buffer to write
         WORDS( 4 ) = 124
         WORDS( 5 ) = 0
         WORDS( 6 ) = 125
         WORDS( 7 ) = 0

*   Return pixel line
*   Ikon command 174 = 'AE'X = Return pixel line
         WORDS( 8 ) = 174
         WORDS( 9 ) = XEND - 1

*   Send these commands and flush the buffer
         NUMWOR = 9
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         CALL IKNOUT( STATUS )

*   Read the data
         NB = XEND
         CALL IKNIBB( DISPID, NB, BSPAC0, STATUS )

*   Set the frame buffer to read the overlay memory
*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7C'X = Set frame buffer to write
         WORDS( 1 ) = 124
         WORDS( 2 ) = 1
         WORDS( 3 ) = 125
         WORDS( 4 ) = 1

*   Return pixel line
*   Ikon command 174 = 'AE'X = Return pixel line
         WORDS( 5 ) = 174
         WORDS( 6 ) = XEND - 1

*   Send these commands and flush the buffer
         NUMWOR = 6
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         CALL IKNOUT( STATUS )

*   Read the data
         NB = XEND
         CALL IKNIBB( DISPID, NB, BSPAC1, STATUS )

*   If the base memory is not visible then use the overlay memory if
*   that is visible
         IF ( CMEMVI( 0 ) .EQ. 0 ) THEN
            DO K = 1, NB
               IF ( CMEMVI( 1 ) .EQ. 0 ) THEN
                  BSPAC0( K ) = 0
                  LUT( K ) = -1
               ELSE
                  BSPAC0( K ) = BSPAC1( K )
                  LUT( K ) = 1
               ENDIF
            ENDDO

*   If the base memory is visible then use the overlay memory if it
*   is visible and non-zero
         ELSE
            DO K = 1, NB
               LUT( K ) = 0
               IF ( ( CMEMVI( 1 ) .NE. 0 ) .AND.
     :              ( BSPAC1( K ) .NE. 0 ) ) THEN
                  BSPAC0( K ) = BSPAC1( K )
                  LUT( K ) = 1
               ENDIF
            ENDDO
         ENDIF

*   Convert every colour index into an actual coulour
         DO K = 1, NB

*   Put the memory colour index into an integer word
            B32 = 0
            B8( 1 ) = BSPAC0( K )

*   Add 1 to the colour index to make it scale between 1 and 256
            I = B32 + 1

*   Check which look-up table is being used
            IF ( LUT( K ) .EQ. 0 ) THEN

*   Use the appropriate mode
*   If it is pseudo-colour then use 0.30 * R + 0.59 * G + 0.11 * B
               IF ( CMODE .EQ. 0 ) THEN
                  B32 = NINT( VLUT0( 1, I ) * RED + VLUT0( 2, I ) *
     :                        GREEN + VLUT0( 3, I ) * BLUE )
                  BSPAC0( K ) = B8( 1 )
               ELSE
                  B32 = NINT( VLUT0( CMODE, I ) * WHITE )
                  BSPAC0( K ) = B8( 1 )
               ENDIF

            ELSEIF ( LUT( K ) .EQ. 1 ) THEN

*   Use the appropriate mode
*   If it is pseudo-colour then use 0.30 * R + 0.59 * G + 0.11 * B
               IF ( CMODE .EQ. 0 ) THEN
                  B32 = NINT( VLUT1( 1, I ) * RED + VLUT1( 2, I ) *
     :                        GREEN + VLUT1( 3, I ) * BLUE )
                  BSPAC0( K ) = B8( 1 )
               ELSE
                  B32 = NINT( VLUT1( CMODE, I ) * WHITE )
                  BSPAC0( K ) = B8( 1 )
               ENDIF

*   If the look up table is not recognised put the colour = 0
            ELSE
               BSPAC0( K ) = 0
            ENDIF
         ENDDO

*   Pack the data into integers
         OFFSET = ( XBEGIN + PROWS * PCOLS ) / PACK + 1
         CALL IDIPIL( BSPAC0, NB, PACK, DEPTH, PLEFT,
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

