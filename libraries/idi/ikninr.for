*-----------------------------------------------------------------------
*+  IKNINR - Initialize Rectangular Region of Interest

      SUBROUTINE IKNINR ( DISPID, MEMID, ROICOL, XMIN, YMIN, XMAX, YMAX,
     :                    ROIID, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIRINR.
*     The arguments are identical to those in IIRINR.
*
*    Invocation :
*     CALL IKNINR( DISPID, MEMID, ROICOL, XMIN, YMIN, XMAX, YMAX,
*    :             ROIID, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Assign an ROI identifier.
*     Verify the input arguments.
*     Store the position of th ROI.
*
*    Deficiencies :
*     Very non-standard FORTRAN - INTEGER * 2
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     March 1990
*     February 1991  Find best fit colour in LUT
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

*     ROI marker color
      INTEGER ROICOL

*     Minimum x position
      INTEGER XMIN

*     Minimum y position
      INTEGER YMIN

*     Maximum x position
      INTEGER XMAX

*     Maximum y position
      INTEGER YMAX

*    Export :
*     ROI identifier
      INTEGER ROIID

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMID)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local variables :
      INTEGER * 2 BACOL, WORDS( 9 )

      INTEGER J, LOCOL, LMEMID, NUMWOR, X1, X2, XPH, XPL,
     :        Y1, Y2, YPH, YPL
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

*   Assign an ROI identifier
      ROIID = -1
      DO J = 0, MAXROI - 1
         IF ( J .LT. CNROI ) THEN
            IF ( CROIID( J ) .LT. 0 ) THEN
               ROIID = J
               CROIID( J ) = J
               GOTO 10
            ENDIF
         ELSE
            STATUS = IDI__NOROI
            GOTO 99
         ENDIF
      ENDDO
  10  CONTINUE
      IF ( ROIID .LT. 0 ) THEN
         STATUS = IDI__NOROI
         GOTO 99
      ENDIF

*   Check the ROI colour
      IF ( ROICOL .LT. 0 ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ELSEIF ( ROICOL .GT. MAXCOL ) THEN
         ROICOL = MAXCOL
      ENDIF
      CROICO( ROIID ) = ROICOL

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
         LMEMID = 0

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
         LMEMID = MEMID

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INMID
         GOTO 99
      ENDIF

*   Set the symbol colour to the chosen one
      IF ( ROICOL .EQ. 0 ) THEN

*   Read in the current background colour
*   Ikon command 103 = '67'X = Read register - 16 bit low
*   Ikon register 0 = '00'X = Background colour
         WORDS( 1 ) = 103
         WORDS( 2 ) = 0
         NUMWOR = 2
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         CALL IKNOUT( STATUS )
         NUMWOR = 1
         CALL IKNIBW( DISPID, NUMWOR, BACOL, STATUS )
         LOCOL = BACOL

*   Find a best fit match from the LUT
      ELSE
         CALL IDLMAC( LMEMID, ROICOL, LOCOL, STATUS )
      ENDIF

*   Set the rubber band colour
*   Ikon command 85 = '55'X = Set rubber band colour
      WORDS( 5 ) = 85
      WORDS( 6 ) = LOCOL

*   Send these commands
      NUMWOR = 6
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

  99  CONTINUE

      END

