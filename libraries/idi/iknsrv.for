*-----------------------------------------------------------------------
*+  IKNSRV - Set Visibility Rectangular Region of Interest

      SUBROUTINE IKNSRV ( DISPID, ROIID, LVIS, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIRSRV.
*     The arguments are identical to those in IIRSRV.
*
*    Invocation :
*     CALL IKNSRV( DISPID, ROIID, LVIS, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Verify the input arguments.
*     Switch the ROI on or off according to the flag.
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
*     March 1990
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

*     ROI identifier
      INTEGER ROIID

*     Visibility
      LOGICAL LVIS

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMID)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local variables :
      INTEGER * 2 WORDS( 30 )

      INTEGER J, NUMWOR
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

*   Check the ROI identifier
      IF ( ( ROIID .LT. 0 ) .OR. ( ROIID .GT. CNROI - 1 ) ) THEN
         STATUS = IDI__INRID
         GOTO 99
      ENDIF
      IF ( CROIID( ROIID ) .LT. 0 ) THEN
         STATUS = IDI__INRID
         GOTO 99
      ENDIF

*   Store the visibility
      CROIVI( ROIID ) = LVIS

*   Display the ROI
      IF ( LVIS ) THEN

*   Make sure the frame buffers are correct
*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7D'X = Set frame buffer to write
         WORDS( 1 ) = 124
         WORDS( 2 ) = CROIBI( ROIID )
         WORDS( 3 ) = 125
         WORDS( 4 ) = CROIBI( ROIID )

*   Ikon command 88 = '58'X = Set rubber outline
         WORDS( 5 ) = 88

*   Set up fixed sized rectangle specified by the copy source area
*   Ikon command 164 = 'A4'X = Move to
         WORDS( 6 ) = 164
         WORDS( 7 ) = CROIXL( ROIID ) - CMEMX( 0 )
         WORDS( 8 ) = CROIYL( ROIID ) - CMEMY( 0 )
*   Ikon command 48 = '30'X = Mark copy source area
         WORDS( 9 ) = 48
         WORDS( 10 ) = CROIXH( ROIID ) - CMEMX( 0 )
         WORDS( 11 ) = CROIYH( ROIID ) - CMEMY( 0 )

*   Use a small crosshair cursor at the bottom left of the box
*   to force rapid update of the screen
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 57 = '39'X = Crosshair cursor size X-Y
*   Ikon command 193 = 'C1'X = Crosshair cursor on
         WORDS( 12 ) = 99
         WORDS( 13 ) = 57
         WORDS( 14 ) = 16
         WORDS( 15 ) = 16
         WORDS( 16 ) = 193

*   Move the GID position to the bottom left corner of the box
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 29 = '1D'X = Graphic input device position XY
         WORDS( 17 ) = 99
         WORDS( 18 ) = 29
         WORDS( 19 ) = CROIYL( ROIID ) - CMEMY( 0 )
         WORDS( 20 ) = CROIXL( ROIID ) - CMEMX( 0 )

*   Switch on the rubber band box in GID mode
*   Ikon command 96 = '60'X = Set register - 8 bit
*   Ikon address 24 = '18'X = Graphic input device mode
         WORDS( 21 ) = 96
         WORDS( 22 ) = 24
         WORDS( 23 ) = 128 + 32

*   Set the GID position again
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 29 = '1D'X = Graphic input device position XY
         WORDS( 24 ) = 99
         WORDS( 25 ) = 29
         WORDS( 26 ) = CROIYL( ROIID ) - CMEMY( 0 )
         WORDS( 27 ) = CROIXL( ROIID ) - CMEMX( 0 )

*   Send the commands and flush the buffer
         NUMWOR = 27
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         CALL IKNOUT( STATUS )

*   Undisplay the ROI
      ELSE

*   Switch off the rubber band box in GID mode
*   Ikon command 96 = '60'X = Set register - 8 bit
*   Ikon address 24 = '18'X = Graphic input device mode
         WORDS( 1 ) = 96
         WORDS( 2 ) = 24
         WORDS( 3 ) = 128

*   Switch off the small crosshair cursor
*   Ikon command 192 = 'C0'X = Cursor off
         WORDS( 4 ) = 192

*   Ikon command 89 = '59'X = Reset rubber outline
         WORDS( 5 ) = 89

*   Send the commands and flush the buffer
         NUMWOR = 5
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         CALL IKNOUT( STATUS )

*   Reinstate any cursors
         DO J = 0, CURN - 1
            IF ( CURVIS( J ) .NE. 0 ) THEN
               CALL IKNCON( DISPID, J, STATUS )
            ENDIF
         ENDDO

      ENDIF

  99  CONTINUE

      END

