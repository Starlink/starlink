*-----------------------------------------------------------------------
*+  IKNWRI - Write Rectangular Region of Interest

      SUBROUTINE IKNWRI ( DISPID, MEMID, ROIID, XMIN, YMIN, XMAX, YMAX,
     :                    STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIRWRI.
*     The arguments are identical to those in IIRWRI.
*
*    Invocation :
*     CALL IKNWRI( DISPID, MEMID, ROIID, XMIN, YMIN, XMAX, YMAX,
*    :             STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works>
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

*     Memory identifier
      INTEGER MEMID

*     ROI identifier
      INTEGER ROIID

*     Minimum x position
      INTEGER XMIN

*     Minimum y position
      INTEGER YMIN

*     Maximum x position
      INTEGER XMAX

*     Maximum y position
      INTEGER YMAX

*    Status :
      INTEGER STATUS

*    External references :
*     <declarations for external function references>

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMID)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local Constants :
*     <local constants defined by PARAMETER>

*    Local variables :
      INTEGER * 2 WORDS( 30 )

      INTEGER NUMWOR, X1, X2, XPH, XPL, Y1, Y2, YPH, YPL

*    Internal References :
*     <declarations for internal functions>
*    Local data :
*     <any DATA initialisations for local variables>
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

*   Check the ROI identifier
      IF ( ( ROIID .LT. 0 ) .OR. ( ROIID .GT. CNROI - 1 ) ) THEN
         STATUS = IDI__INRID
         GOTO 99
      ENDIF
      IF ( CROIID( ROIID ) .LT. 0 ) THEN
         STATUS = IDI__INRID
         GOTO 99
      ENDIF

*   Check the positions are the correct way round
      IF ( XMIN .LE. XMAX ) THEN
         X1 = XMIN
         X2 = XMAX
      ELSE
         X1 = XMAX
         X2 = XMIN
      ENDIF
      IF ( YMIN .LE. YMAX ) THEN
         Y1 = YMIN
         Y2 = YMAX
      ELSE
         Y1 = YMAX
         Y2 = YMIN
      ENDIF

*   If MEMID = -1 then the position is relative to the screen origin
*   Use memory 0 to work out the screen coordinates allowing for zoom
      IF ( MEMID .EQ. -1 ) THEN
         XPL = X1 - CSCROX( 0 )
         YPL = Y1 - CSCROY( 0 ) + CNPIX( 1 ) *
     :        CMEMZ( 0 ) / ( CMEMZ( 0 ) + 1 )
         XPH = X2 - CSCROX( 0 )
         YPH = Y2 - CSCROY( 0 ) + CNPIX( 1 ) *
     :        CMEMZ( 0 ) / ( CMEMZ( 0 ) + 1 )

*   Check the position is on the screen
         IF ( ( XPL .GE. 0 ) .AND.
     :        ( XPL .LE. CNPIX( 0 ) / ( CMEMZ( 0 ) + 1 ) ) .AND.
     :        ( YPL .GE. 0 ) .AND.
     :        ( YPL .LE. CNPIX( 1 ) / ( CMEMZ( 0 ) + 1 ) ) ) THEN
            CROIXL( ROIID ) = XPL + CMEMX( 0 )
            CROIYL( ROIID ) = YPL + CMEMY( 0 )
         ENDIF
         IF ( ( XPH .GE. 0 ) .AND.
     :        ( XPH .LE. CNPIX( 0 ) / ( CMEMZ( 0 ) + 1 ) ) .AND.
     :        ( YPH .GE. 0 ) .AND.
     :        ( YPH .LE. CNPIX( 1 ) / ( CMEMZ( 0 ) + 1 ) ) ) THEN
            CROIXH( ROIID ) = XPH + CMEMX( 0 )
            CROIYH( ROIID ) = YPH + CMEMY( 0 )
         ENDIF

*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7D'X = Set frame buffer to write
         WORDS( 1 ) = 124
         WORDS( 2 ) = 0
         WORDS( 3 ) = 125
         WORDS( 4 ) = 0

*   Store the memory in the bindings
         CROIBI( ROIID ) = 0

*   Otherwise it is relative to the memory origin
      ELSEIF ( ( MEMID .GE. 0 ) .AND. ( MEMID .LT. CNMEM ) ) THEN
         XPL = X1
         YPL = Y1
         XPH = X2
         YPH = Y2

*   Check the position is on the memory
         IF ( ( XPL .GE. 0 ) .AND.
     :        ( XPL .LE. CMEMSX( MEMID ) ) .AND.
     :        ( YPL .GE. 0 ) .AND.
     :        ( YPL .LE. CMEMSY( MEMID ) ) ) THEN
            CROIXL( ROIID ) = XPL + CSCROX( MEMID )
            CROIYL( ROIID ) = YPL + CSCROY( MEMID )
         ENDIF
         IF ( ( XPH .GE. 0 ) .AND.
     :        ( XPH .LE. CMEMSX( MEMID ) ) .AND.
     :        ( YPH .GE. 0 ) .AND.
     :        ( YPH .LE. CMEMSY( MEMID ) ) ) THEN
            CROIXH( ROIID ) = XPH + CSCROX( MEMID )
            CROIYH( ROIID ) = YPH + CSCROY( MEMID )
         ENDIF

*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7D'X = Set frame buffer to write
         WORDS( 1 ) = 124
         WORDS( 2 ) = MEMID
         WORDS( 3 ) = 125
         WORDS( 4 ) = MEMID

*   Store the memory in the bindings
         CROIBI( ROIID ) = MEMID

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INMID
         GOTO 99
      ENDIF

*   Plot the new position 
*   Ikon command 164 = 'A4'X = Move to
      WORDS( 5 ) = 164
      WORDS( 6 ) = XPL
      WORDS( 7 ) = YPL

*   Send these commands
      NUMWOR = 7
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   Display the ROI if it is visible, as it may have changed size
      IF ( CROIVI( ROIID ) .NE. 0 ) THEN

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

*   Send these commands
         NUMWOR = 27
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      ENDIF

*   Flush the buffer
      CALL IKNOUT( STATUS )

  99  CONTINUE

      END

