*-----------------------------------------------------------------------
*+  IKNTXT - Plot text

      SUBROUTINE IKNTXT ( DISPID, MEMID, TEXT, XPOS, YPOS, TPATH,
     :                    TANGLE, COLOR, TSIZE, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIGTXT.
*     The arguments are identical to those in IIGTXT.
*
*    Invocation :
*     CALL IKNTXT( DISPID, MEMID, TEXT, XPOS, YPOS, TPATH, TANGLE,
*    :             COLOR, TSIZE, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Verify input arguments.
*     Select the Ikon text colour, symbol set and symbol size.
*     Limit the angle to be one of eight directions, 45 degrees apart.
*     For each character in the input string, replace it with a space
*     if it is an ASCII control character, then put it in the local
*     buffer. Send the local buffer if it is full.
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
*     April 1989
*     February 1990  Put cursor shapes into IKNCON
*     July 1990  Set transparent colour equal to background colour
*     November 1992  Draw with XOR if COLOR < 0 and correct transparency
*                    Select symbol frame size x-y to squeeze text.
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

*     Text
      CHARACTER * ( * ) TEXT

*     X position
      INTEGER XPOS

*     Y position
      INTEGER YPOS

*     Text path
      INTEGER TPATH

*     Text orientation
      INTEGER TANGLE

*     Pixel color value
      INTEGER COLOR

*     Text size
      INTEGER TSIZE

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local Constants :
      INTEGER BUFLEN
      PARAMETER ( BUFLEN = 64 )

*    Local variables :
      INTEGER * 2 BACOL, WORDS( BUFLEN )

      INTEGER FRAME( 0: 3 ), ICODE, J, LOCOL, NUMWOR, SCALE( 0: 3 ),
     :        SET( 0: 3 ), TLEN

*    Local data :
*     Symbol sizes are a combination of the symbol set and scale factor
*     Symbol scale factor X-Y
      DATA SCALE / 257, 771, 1799, 0 /

*     Symbol set to use for each of the available sizes
      DATA SET / 0, 0, 0, 0 /

*     The symbol spacing can be reduced by eating into the frame size.
*     The default for symbol set 0 is 'B08'X, but use 'B07'X = 2823
*     Symbol frame size X-Y
      DATA FRAME / 2823, 2823, 2823, 2823 /

      SAVE FRAME, SCALE, SET
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

*   Verify the text size
      IF ( ( TSIZE .LT. 0 ) .OR. ( TSIZE .GT. 3 ) ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   Find out the length of the character string
      TLEN = LEN( TEXT )

*   Select the correct memory
*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7D'X = Set frame buffer to write
      WORDS( 1 ) = 124
      WORDS( 2 ) = MEMID
      WORDS( 3 ) = 125
      WORDS( 4 ) = MEMID

*   Read in the current symbol background colour
*   Ikon command 103 = '67'X = Read register - 16 bit low
*   Ikon register 2 = '02'X = Symbol background colour
      WORDS( 5 ) = 103
      WORDS( 6 ) = 2
      NUMWOR = 6
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )
      NUMWOR = 1
      CALL IKNIBW( DISPID, NUMWOR, BACOL, STATUS )

*   Switch off any cursors
*   Ikon command 192 = 'C0'X = Crosshair cursor off
      WORDS( 1 ) = 192

*   Set the symbol colour to the chosen one
*   Ikon command 67 = '43'X = Set symbol drawing colour register
      LOCOL = COLOR
      IF ( COLOR .LT. 0 ) LOCOL = -LOCOL
      WORDS( 2 ) = 67
      WORDS( 3 ) = LOCOL

*   Set the transparent colour to the same as the background colour
*   Ikon command 72 = '48'X = Set transparent colour register
      WORDS( 4 ) = 72
      WORDS( 5 ) = BACOL

*   Select symbol set according to the size required
*   Ikon command 35 = '23'X = Select symbol set
      WORDS( 6 ) = 35
      WORDS( 7 ) = SET( TSIZE )

*   Set the symbol drawing mode
*   Draw with XOR if COLOR < 0
*   Ikon command 40 = '28'X = Set symbol drawing mode
      IF ( COLOR .LT. 0 ) THEN
         WORDS( 8 ) = 40
         WORDS( 9 ) = 11
      ELSE
         WORDS( 8 ) = 40
         WORDS( 9 ) = 8
      ENDIF

*   Select the character size
*   Ikon command 97 = '61'X = Set register - 16 bit low
*   Ikon register 18 = '12'X = Symbol scale factor X-Y
      WORDS( 10 ) = 97
      WORDS( 11 ) = 18
      WORDS( 12 ) = SCALE( TSIZE )

*   Select the symbol frame size
*   Ikon command 97 = '61'X = Set register - 16 bit low
*   Ikon register 17 = '11'X = Symbol frame size X-Y
      WORDS( 13 ) = 97
      WORDS( 14 ) = 17
      WORDS( 15 ) = FRAME( TSIZE )

*   Set the text orientation, in 45 degree steps
*   Ikon command 97 = '61'X = Set register - 16 bit low
*   Ikon register 16 = '10'X = Symbol direction / symbol slant
      WORDS( 16 ) = 97
      WORDS( 17 ) = 16
      WORDS( 18 ) = MOD( MOD( TANGLE, 360 ) + 360 + 22, 360 ) / 45

*   Move to the starting position
*   Ikon command 164 = 'A4'X = Move to
      WORDS( 19 ) = 164
      WORDS( 20 ) = XPOS
      WORDS( 21 ) = YPOS

*   Set up the character transfer
*   Ikon command 33 = '21'X = Draw symbols from current symbol set
      WORDS( 22 ) = 33
      WORDS( 23 ) = TLEN - 1

*   Send these commands
      NUMWOR = 23
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      NUMWOR = 0

*   Loop through the character string
      DO J = 1, TLEN

*   Convert the character into its ASCII code
         ICODE = ICHAR( TEXT( J : J ) )

*   Replace all ASCII control characters with a space
         IF ( ( ICODE .LT. 32 ) .OR. ( ICODE .GT. 126 ) ) THEN
            ICODE = 32
         ENDIF

*   Put the character into the local buffer
         NUMWOR = NUMWOR + 1
         WORDS( NUMWOR ) = ICODE

*   If the local buffer is full then send the string
         IF ( NUMWOR .GE. BUFLEN ) THEN
            CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
            NUMWOR = 0
         ENDIF
      ENDDO

*   Send the string
      IF ( NUMWOR .GT. 0 ) THEN
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      ENDIF

*   Reinstate any cursors
      DO J = 0, CURN - 1
         IF ( CURVIS( J ) .NE. 0 ) THEN
            CALL IKNCON( DISPID, J, STATUS )
         ENDIF
      ENDDO

  99  CONTINUE

      END

