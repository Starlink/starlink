*-----------------------------------------------------------------------
*+  IKNINC - Initialize Cursor

      SUBROUTINE IKNINC ( DISPID, MEMID, NUMCUR, SHAPE, COLOR, XC,
     :                    YC, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IICINC.
*     The arguments are identical to those in IICINC.
*
*    Invocation :
*     CALL IKNINC( DISPID, MEMID, NUMCUR, SHAPE, COLOR, XC,
*    :             YC, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Verify the input arguments.
*     Check the cursor position.
*     Set up the symbol cursor shapes.
*     The Ikon has the 16x16 bit cursor centre in the centre of the
*     symbol. To get the cursor to point to the edge of a symbol have
*     to use the 64x64 bit cursor.
*
*    Deficiencies :
*     <description of any deficiencies>
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     November 1989
*     February 1990  Delete symbol set 16 before creating it
*                    and set up the cursor colour properly
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

*     Cursor number
      INTEGER NUMCUR

*     Cursor shape
      INTEGER SHAPE

*     Cursor color
      INTEGER COLOR

*     X cursor position
      INTEGER XC

*     Y cursor position
      INTEGER YC

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local variables :
      INTEGER *2 COLS( 3, 0:8 ), BLANK( 512 ), WLUT( 4 ), WORDS( 25 )
      INTEGER *2 ARROW1( 16 ), ARROW2( 16 ), CIRCL1( 16 ), CIRCL2( 16 ),
     :           CROSS1( 16 ), CROSS2( 16 ), DIAMO1( 16 ), DIAMO2( 16 ),
     :           HAND1( 16 ), HAND2( 16 ), SQUAR1( 16 ), SQUAR2( 16 )

      INTEGER LCOLOR, NUMWOR, XP, YP

*    Local data :
*     Define the cursor shapes on a 16x16 array
      DATA BLANK / 512 * 0 /
      DATA ARROW1 / 'C000'X, 'B000'X, '4C00'X, '4300'X,
     :              '20E0'X, '2010'X, '1020'X, '1040'X,
     :              '0820'X, '0910'X, '0A88'X, '0444'X,
     :              '0022'X, '0011'X, '000A'X, '0004'X /
      DATA ARROW2 / '0000'X, '4000'X, '3000'X, '3C00'X,
     :              '1F00'X, '1FE0'X, '0FC0'X, '0F80'X,
     :              '07C0'X, '06E0'X, '0470'X, '0038'X,
     :              '001C'X, '000E'X, '0004'X, '0000'X /
      DATA CIRCL1 / '03E0'X, '0C18'X, '1004'X, '2002'X,
     :              '2002'X, '4001'X, '4001'X, '4001'X,
     :              '4001'X, '4001'X, '2002'X, '2002'X,
     :              '1004'X, '0C18'X, '03E0'X, '0000'X /
      DATA CIRCL2 / '0000'X, '03E0'X, '0C18'X, '1004'X,
     :              '1004'X, '2002'X, '2002'X, '2002'X,
     :              '2002'X, '2002'X, '1004'X, '1004'X,
     :              '0C18'X, '03E0'X, '0000'X, '0000'X /
      DATA CROSS1 / '0080'X, '0080'X, '0080'X, '0080'X,
     :              '0080'X, '0000'X, '0000'X, '7C1F'X,
     :              '0000'X, '0000'X, '0080'X, '0080'X,
     :              '0080'X, '0080'X, '0080'X, '0000'X /
      DATA CROSS2 / '0100'X, '0100'X, '0100'X, '0100'X,
     :              '0100'X, '0000'X, '001F'X, '0000'X,
     :              '7C00'X, '0000'X, '0040'X, '0040'X,
     :              '0040'X, '0040'X, '0040'X, '0000'X /
      DATA DIAMO1 / '0080'X, '0140'X, '0220'X, '0410'X,
     :              '0808'X, '1004'X, '2002'X, '4001'X,
     :              '2002'X, '1004'X, '0808'X, '0410'X,
     :              '0220'X, '0140'X, '0080'X, '0000'X /
      DATA DIAMO2 / '0000'X, '0080'X, '0140'X, '0220'X,
     :              '0410'X, '0808'X, '1004'X, '2002'X,
     :              '1004'X, '0808'X, '0410'X, '0220'X,
     :              '0140'X, '0080'X, '0000'X, '0000'X /
      DATA HAND1  / 'E00C'X, '9014'X, '4824'X, '2428'X,
     :              '1228'X, '29C8'X, '2408'X, '5208'X,
     :              '4908'X, 'A608'X, '9C04'X, '4804'X,
     :              '3802'X, '1004'X, '0808'X, '07F0'X /
      DATA HAND2  / '0000'X, '6008'X, '3018'X, '1810'X,
     :              '0C10'X, '1630'X, '1BF0'X, '2DF0'X,
     :              '36F0'X, '59F0'X, '63F8'X, '37F8'X,
     :              '07FC'X, '0FF8'X, '07F0'X, '0000'X /
      DATA SQUAR1 / '7FFF'X, '4001'X, '4001'X, '4001'X,
     :              '4001'X, '4001'X, '4001'X, '4001'X,
     :              '4001'X, '4001'X, '4001'X, '4001'X,
     :              '4001'X, '4001'X, '7FFF'X, '0000'X /
      DATA SQUAR2 / '0000'X, '3FFE'X, '2002'X, '2002'X,
     :              '2002'X, '2002'X, '2002'X, '2002'X,
     :              '2002'X, '2002'X, '2002'X, '2002'X,
     :              '2002'X, '3FFE'X, '0000'X, '0000'X /

*     Define the cursor colours : white, black, white,
*     red, green, blue, yellow, magenta, cyan
      DATA COLS / 255,255,255, 0,0,0, 255,255,255, 255,0,0, 0,255,0,
     :            0,0,255, 255,255,0, 255,0,255, 0,255,255 /

      SAVE ARROW1, ARROW2, CIRCL1, CIRCL2, CROSS1, CROSS2,
     :     DIAMO1, DIAMO2, HAND1, HAND2, SQUAR1, SQUAR2, COLS
*-

*   Recover the common blocks if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNOUT( STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            STATUS = IDI__NOREC
            GOTO 99
         ENDIF
      ENDIF

*   Check the cursor number
      IF ( ( NUMCUR .LT. 0 ) .OR. ( NUMCUR .GE. CURN ) ) THEN
         STATUS = IDI__INCID
         GOTO 99
      ENDIF

*   Check the cursor shape
      IF ( ( SHAPE .LT. 0 ) .OR. ( SHAPE .GT. CURNSH ) ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   Check the cursor colour
*   Use a local variable for COLOR as it is an input argument only
      LCOLOR = COLOR
      IF ( LCOLOR .LT. 0 ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ELSEIF ( LCOLOR .GT. 8 ) THEN
         LCOLOR = 2
      ENDIF

*   Set the symbol cursor colours
*   Use colour number 0 in the overlay plane to set the colour
*   First read in the overlay background into temporary storage
*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 83 = '53'X = Read palette colour
      WORDS( 1 ) = 124
      WORDS( 2 ) = 1
      WORDS( 3 ) = 83
      WORDS( 4 ) = 0
      NUMWOR = 4
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )

*   Read in the colour
      NUMWOR = 4
      CALL IKNIBW( DISPID, NUMWOR, WLUT, STATUS )

*   Set up the cursor background colour to be black
*   Ikon command 125 = '7D'X = Set frame buffer to write
*   Ikon command 81 = '51'X = Set palette colour
*   Ikon command 71 = '47'X = Set symbol cursor colour 1 register
      WORDS( 1 ) = 125
      WORDS( 2 ) = 1
      WORDS( 3 ) = 81
      WORDS( 4 ) = 0
      WORDS( 5 ) = 0
      WORDS( 6 ) = 0
      WORDS( 7 ) = 0
      WORDS( 8 ) = 71
      WORDS( 9 ) = 0
      
*   Set up the cursor foreground colour to be as defined
*   Ikon command 81 = '51'X = Set palette colour
*   Ikon command 70 = '46'X = Set symbol cursor colour 0 register
      WORDS( 10 ) = 81
      WORDS( 11 ) = 0
      WORDS( 12 ) = COLS( 1, LCOLOR )
      WORDS( 13 ) = COLS( 2, LCOLOR )
      WORDS( 14 ) = COLS( 3, LCOLOR )
      WORDS( 15 ) = 70
      WORDS( 16 ) = 0
      
*   Reinstate the original colour to the overlay background
*   Ikon command 81 = '51'X = Set palette colour
      WORDS( 17 ) = 81
      WORDS( 18 ) = 0
      WORDS( 19 ) = WLUT( 2 )
      WORDS( 20 ) = WLUT( 3 )
      WORDS( 21 ) = WLUT( 4 )
      NUMWOR = 21
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   If MEMID = -1 then the position is relative to the screen origin
*   Use memory 0 to work out the screen coordinates allowing for zoom
      IF ( MEMID .EQ. -1 ) THEN
         XP = XC - CSCROX( 0 )
         YP = YC - CSCROY( 0 ) + CNPIX( 1 ) *
     :        CMEMZ( 0 ) / ( CMEMZ( 0 ) + 1 )

*   Check the position is on the screen
         IF ( ( XP .GE. 0 ) .AND.
     :        ( XP .LE. CNPIX( 0 ) / ( CMEMZ( 0 ) + 1 ) ) .AND.
     :        ( YP .GE. 0 ) .AND.
     :        ( YP .LE. CNPIX( 1 ) / ( CMEMZ( 0 ) + 1 ) ) ) THEN
            CURX( NUMCUR ) = XP + CMEMX( 0 )
            CURY( NUMCUR ) = YP + CMEMY( 0 )
         ENDIF

*   Ikon command 123 = '7B'X = Set frame buffer to write
         WORDS( 1 ) = 123
         WORDS( 2 ) = 0

*   Otherwise it is relative to the memory origin
      ELSEIF ( ( MEMID .GE. 0 ) .AND. ( MEMID .LT. CNMEM ) ) THEN
         XP = XC
         YP = YC

*   Check the position is on the memory
         IF ( ( XP .GE. 0 ) .AND.
     :        ( XP .LE. CMEMSX( MEMID ) ) .AND.
     :        ( YP .GE. 0 ) .AND.
     :        ( YP .LE. CMEMSY( MEMID ) ) ) THEN
            CURX( NUMCUR ) = XC + CSCROX( MEMID )
            CURY( NUMCUR ) = YC + CSCROY( MEMID )
         ENDIF

*   Ikon command 123 = '7B'X = Set frame buffer to write
         WORDS( 1 ) = 123
         WORDS( 2 ) = MEMID

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INMID
         GOTO 99
      ENDIF

*   Delete symbol set 16 before creating it.
*   Previously the Ikon crashed if an attempt to create an
*   existing symbol set was made.
*   Ikon command 35 = '23'X = Select symbol set
*   Ikon command 36 = '24'X = Delete current symbol set
      WORDS( 3 ) = 35
      WORDS( 4 ) = 16
      WORDS( 5 ) = 36

*   Select symbol set 1
*   Ikon command 35 = '23'X = Select symbol set
      WORDS( 6 ) = 35
      WORDS( 7 ) = 1

*   Create symbol set number 16, containing 128 symbols
*   Ikon command 39 = '27'X = Create symbol set
      WORDS( 8 ) = 39
      WORDS( 9 ) = 16
      WORDS( 10 ) = 0

*   Select cursor symbol set number 16
*   Ikon command 195 = 'C3'X = Select cursor symbol set
      WORDS( 11 ) = 195
      WORDS( 12 ) = 16
      NUMWOR = 12
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   Load cross symbol into current symbol set
*   Indicate use of 16x16 bit cursor
*   Ikon command 97 = '61'X = Set register - 16 bit low
*   Ikon address 56 = '38'X = Cursor control register
      IF ( SHAPE .EQ. 3 ) THEN
         WORDS( 1 ) = 97
         WORDS( 2 ) = 56
         WORDS( 3 ) = 0

*   Ikon command 38 = '26'X = Load symbols into current symbol set
         WORDS( 4 ) = 38
         WORDS( 5 ) = 1
         WORDS( 6 ) = 6

*   Sends these commands and the cross symbol
         NUMWOR = 6
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         NUMWOR = 16
         CALL IKNOBW( DISPID, NUMWOR, CROSS1, STATUS )
         CALL IKNOBW( DISPID, NUMWOR, CROSS2, STATUS )

*   Load square symbol into current symbol set
*   Indicate use of 16x16 bit cursor
*   Ikon command 97 = '61'X = Set register - 16 bit low
*   Ikon address 56 = '38'X = Cursor control register
      ELSEIF ( SHAPE .EQ. 4 ) THEN
         WORDS( 1 ) = 97
         WORDS( 2 ) = 56
         WORDS( 3 ) = 0

*   Ikon command 38 = '26'X = Load symbols into current symbol set
         WORDS( 4 ) = 38
         WORDS( 5 ) = 1
         WORDS( 6 ) = 8

*   Sends these commands and the square symbol
         NUMWOR = 6
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         NUMWOR = 16
         CALL IKNOBW( DISPID, NUMWOR, SQUAR1, STATUS )
         CALL IKNOBW( DISPID, NUMWOR, SQUAR2, STATUS )

*   Load diamond symbol into current symbol set
*   Indicate use of 16x16 bit cursor
*   Ikon command 97 = '61'X = Set register - 16 bit low
*   Ikon address 56 = '38'X = Cursor control register
      ELSEIF ( SHAPE .EQ. 5 ) THEN
         WORDS( 1 ) = 97
         WORDS( 2 ) = 56
         WORDS( 3 ) = 0

*   Ikon command 38 = '26'X = Load symbols into current symbol set
         WORDS( 4 ) = 38
         WORDS( 5 ) = 1
         WORDS( 6 ) = 10

*   Sends these commands and the diamond symbol
         NUMWOR = 6
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         NUMWOR = 16
         CALL IKNOBW( DISPID, NUMWOR, DIAMO1, STATUS )
         CALL IKNOBW( DISPID, NUMWOR, DIAMO2, STATUS )

*   Load circle symbol into current symbol set
*   Indicate use of 16x16 bit cursor
*   Ikon command 97 = '61'X = Set register - 16 bit low
*   Ikon address 56 = '38'X = Cursor control register
      ELSEIF ( SHAPE .EQ. 6 ) THEN
         WORDS( 1 ) = 97
         WORDS( 2 ) = 56
         WORDS( 3 ) = 0

*   Ikon command 38 = '26'X = Load symbols into current symbol set
         WORDS( 4 ) = 38
         WORDS( 5 ) = 1
         WORDS( 6 ) = 12

*   Sends these commands and the diamond symbol
         NUMWOR = 6
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         NUMWOR = 16
         CALL IKNOBW( DISPID, NUMWOR, CIRCL1, STATUS )
         CALL IKNOBW( DISPID, NUMWOR, CIRCL2, STATUS )

*   Load arrow symbol into current symbol set
*   Indicate use of 64x64 bit cursor
*   Ikon command 97 = '61'X = Set register - 16 bit low
*   Ikon address 56 = '38'X = Cursor control register
      ELSEIF ( SHAPE .EQ. 7 ) THEN
         WORDS( 1 ) = 97
         WORDS( 2 ) = 56
         WORDS( 3 ) = '0400'X

*   Blank out the next 32 symbols for the 64x64 cursor
*   Ikon command 38 = '26'X = Load symbols into current symbol set
         WORDS( 4 ) = 38
         WORDS( 5 ) = 31
         WORDS( 6 ) = 14
         NUMWOR = 6
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         NUMWOR = 512
         CALL IKNOBW( DISPID, NUMWOR, BLANK, STATUS )

*   Load the arrow symbol into symbols 24 and 40
*   Ikon command 38 = '26'X = Load symbols into current symbol set
         WORDS( 1 ) = 38
         WORDS( 2 ) = 0
         WORDS( 3 ) = 24
         NUMWOR = 3
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         NUMWOR = 16
         CALL IKNOBW( DISPID, NUMWOR, ARROW1, STATUS )
         WORDS( 1 ) = 38
         WORDS( 2 ) = 0
         WORDS( 3 ) = 40
         NUMWOR = 3
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         NUMWOR = 16
         CALL IKNOBW( DISPID, NUMWOR, ARROW2, STATUS )

*   Load hand symbol into current symbol set
*   Indicate use of 64x64 bit cursor
*   Ikon command 97 = '61'X = Set register - 16 bit low
*   Ikon address 56 = '38'X = Cursor control register
      ELSEIF ( SHAPE .EQ. 8 ) THEN
         WORDS( 1 ) = 97
         WORDS( 2 ) = 56
         WORDS( 3 ) = '0400'X

*   Blank out the next 32 symbols for the 64x64 cursor
*   Ikon command 38 = '26'X = Load symbols into current symbol set
         WORDS( 4 ) = 38
         WORDS( 5 ) = 31
         WORDS( 6 ) = 46
         NUMWOR = 6
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         NUMWOR = 512
         CALL IKNOBW( DISPID, NUMWOR, BLANK, STATUS )

*   Load the arrow symbol into symbols 56 and 72
*   Ikon command 38 = '26'X = Load symbols into current symbol set
         WORDS( 1 ) = 38
         WORDS( 2 ) = 0
         WORDS( 3 ) = 56
         NUMWOR = 3
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         NUMWOR = 16
         CALL IKNOBW( DISPID, NUMWOR, HAND1, STATUS )
         WORDS( 1 ) = 38
         WORDS( 2 ) = 0
         WORDS( 3 ) = 72
         NUMWOR = 3
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         NUMWOR = 16
         CALL IKNOBW( DISPID, NUMWOR, HAND2, STATUS )
      ENDIF

*   Remember the set ups in the common blocks
      CURCOL( NUMCUR ) = LCOLOR
      CURSHA( NUMCUR ) = SHAPE

*   Reset the symbol set to 1
*   Ikon command 35 = '23'X = Select symbol set
      WORDS( 1 ) = 35
      WORDS( 2 ) = 1
      NUMWOR = 2
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   Display the cursor if required
      IF ( CURVIS( NUMCUR ) .NE. 0 ) THEN
         CALL IKNCON( DISPID, NUMCUR, STATUS )
      ENDIF

  99  CONTINUE

      END

