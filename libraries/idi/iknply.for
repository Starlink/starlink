*-----------------------------------------------------------------------
*+  IKNPLY - Polyline

      SUBROUTINE IKNPLY ( DISPID, MEMID, X, Y, NXY, COLOR, LSTYLE,
     :                    STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIGPLY.
*     The arguments are identical to those in IIGPLY.
*
*    Invocation :
*     CALL IKNPLY( DISPID, MEMID, X, Y, NXY, COLOR, LSTYLE, STATUS )
*
*    Method :
*     Verify the input arguments.
*     Set up the polyline characteristics.
*     Send the polyline points, utilising a local buffer.
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
*     November 1992  Draw with XOR if COLOR < 0
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

*     List of x positions
      INTEGER X( * )

*     List of y positions
      INTEGER Y( * )

*     Number of ( x,y ) positions
      INTEGER NXY

*     Color
      INTEGER COLOR

*     Line style
      INTEGER LSTYLE

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
      INTEGER * 2 WORDS( BUFLEN )

      INTEGER J, LOCOL, NUMWOR
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

*   Verify the number of points
      IF ( NXY .LT. 1 ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   Verify the line style
      IF ( ( LSTYLE .LT. 1 ) .OR. ( LSTYLE .GT. 4 ) ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   Select the correct memory
*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7D'X = Set frame buffer to write
      WORDS( 1 ) = 124
      WORDS( 2 ) = MEMID
      WORDS( 3 ) = 125
      WORDS( 4 ) = MEMID

*   Switch off any cursors
*   Ikon command 192 = 'C0'X = Crosshair cursor off
      WORDS( 5 ) = 192

*   Set the line drawing mode
*   If the colour index is less than zero draw with XOR
      LOCOL = COLOR
      IF ( LOCOL .LT. 0 ) THEN
         LOCOL = -LOCOL

*   Set the line drawing mode to XOR
*   Ikon command 160 = 'A0'X = Set line drawing mode
         WORDS( 6 ) = 160
         WORDS( 7 ) = 3

*   Set the line drawing mode to normal
*   Ikon command 160 = 'A0'X = Set line drawing mode
      ELSE
         WORDS( 6 ) = 160
         WORDS( 7 ) = 0
      ENDIF

*   Set the line colour to the chosen one
*   For 8 bit memories the top 8 bits of the word have to be set to 1's
*   Ikon command 65 = '41'X = Set drawing colour register 1
      WORDS( 8 ) = 65
      WORDS( 9 ) = LOCOL - 256

*   Set the line style to the one chosen
*   Ikon command 97 = '61'X = Set register 16 bit low
*   Ikon register 50 = '32'X = Line style
      WORDS( 10 ) = 97
      WORDS( 11 ) = 50
      IF ( LSTYLE .EQ. 1 ) THEN
         WORDS( 12 ) = 0
      ELSEIF ( LSTYLE .EQ. 2 ) THEN
         WORDS( 12 ) = 3
      ELSEIF ( LSTYLE .EQ. 3 ) THEN
         WORDS( 12 ) = 7
      ELSEIF ( LSTYLE .EQ. 4 ) THEN
         WORDS( 12 ) = 4
      ENDIF

*   Draw the polyline starting from the first point
*   Ikon command 164 = 'A4'X = Move to
      WORDS( 13 ) = 164
      WORDS( 14 ) = X( 1 )
      WORDS( 15 ) = Y( 1 )

*   Set up the polyline command
*   Ikon command 178 = 'B2'X = Draw polyline
      WORDS( 16 ) = 178
      WORDS( 17 ) = NXY - 2

*   Send these commands
      NUMWOR = 17
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   Send the polyline positions to the Ikon
      NUMWOR = 0
      DO J = 2, NXY
         WORDS( NUMWOR + 1 ) = X( J )
         WORDS( NUMWOR + 2 ) = Y( J )
         NUMWOR = NUMWOR + 2

*   If the local buffer is full then send the points
         IF ( NUMWOR .GE. BUFLEN ) THEN
            CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
            NUMWOR = 0
         ENDIF
      ENDDO

*   Send these points
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

