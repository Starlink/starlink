*+  CNTKEY - Draws a key of contour heights in current SGS zone

      SUBROUTINE CNTKEY( NCONT, CNTLEV, CNTUSD, FRMOFF, UNITS, THICK,
     :                   STATUS )
*
*    Description :
*
*     This subroutine uses NCAR (AUTOGRAPH) and SNX to plot an
*     enumerated list of contour heights, units and a title.  The text
*     is drawn with AUTOGRAPH's fancy typeface and it is located within
*     the current SGS zone. The offset of the top of the key from
*     the base of zone may be specified. If this does not permit all of
*     the key to be drawn, the offset is adjusted so that the key abuts
*     the bottom of the zone. Up to 50 heights may be drawn.
*
*     Contour heights that were selected, but not actually used (because
*     they lay beyond the range of values within the sub-array), are
*     excluded from the plotted list.
*
*     Note before using the routine an SGS device must be opened, a call
*     to SNX_AGWV made and an SGS zone selected or created.
*
*    Invocation :
*
*     CALL CNTKEY( NCONT, CNTLEV, CNTUSD, FRMOFF, UNITS, STATUS )
*
*    Arguments :
*     NCONT = INTEGER( READ )
*       Number of contour heights
*     CNTLEV( NCONT ) = REAL( READ )
*       Contour heights
*     CNTUSD( NCONT ) = LOGICAL( READ )
*       If true the corresponding contour height has actually been
*       plotted and so should appear in the key.
*     FRMOFF = REAL( READ )
*       The fractional position in the y direction of the top of the
*       key within the zone.  This is to allow the key to be aligned
*       with an axis of the contour plot.
*     UNITS = CHAR( READ )
*       The units of the heights.  If this is null no units line will
*       be plotted.
*     THICK = REAL (Given)
*       The line thickness.  1.0 is normal thickness.
*     STATUS = INTEGER( UPDATE )
*       This is the global status, if this variable has an error
*       value on entry then an immediate return will occur.
*
*    Method :
*
*     If no error on entry then return
*     Store current AUTOGRAPH grid parameters
*     Set grid parameters to accommodate the width of the title at a
*       suitable character height to be readable on most devices
*     Store current AUTOGRAPH axis-control parameters
*     Set axis-control parameters to make axes invisible
*     Set text thickness
*     Draw invisible axes and labels - seems to be the only way to make
*       the text to be drawn subsequently visible
*     Write title of key left justified
*     Write units of key (if present) left justified
*     Initialise y start position of text checking that there is room
*       and increasing it so the key abuts the base of the zone
*     For each contour height
*        If contour height has been used then
*           Increment number of contour in key
*           Convert contour number and value into strings and
*             concatenate
*           Increment the y start position for this height downwards
*           Write key entry
*        Endif
*     Endfor
*     Flush AUTOGRAPH text
*     Reset AUTOGRAPH grid, line width and axis-control parameters for
*       any subsequent plotting
*     Return
*
*    Authors :
*
*     Malcolm J. Currie  STARLINK (RAL::CUR)
*
*    History :
*
*     1988 Aug 25 : Original (RAL::CUR).
*     1989 Aug 22 : Outputs contour level number in original list, so it
*                   corrsponds to annotated contours (RAL::CUR).
*     1989 Oct 27 : Positioning of the top of the key via the argument
*                   FRMOFF (RAL::CUR).
*     1990 Aug 28 : Add UNITS argument (RAL::CUR).
*     1994 June 5 : Added THICK argument (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE            ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! global SSE definitions

*    Import :

      INTEGER
     :  NCONT

      REAL
     :  CNTLEV( NCONT ),
     :  FRMOFF,
     :  THICK

      LOGICAL
     :  CNTUSD( NCONT )

      CHARACTER * ( * )
     :  UNITS

*    Status

      INTEGER
     :  STATUS

*    Local constants :

      REAL
     :  HG,                    ! character height in grid units
     :  XOFF                   ! x offset of key entry in grid
                               ! co-ordinates
      PARAMETER ( HG = 0.0129 )
      PARAMETER ( XOFF = 0.001 )

*    Local variables :

      INTEGER
     :  I,                     ! loop counter
     :  LINWDT,                ! NCAR current line width
     :  NC1,                   ! number of characters in contour number
     :  NC2,                   ! number of characters in contour height
     :  NOCONU                 ! number of contours used

      CHARACTER
     :  CAPTIO*80,             ! caption string
     :  CI*3,                  ! contour number
     :  CHT*11,                ! contour height
     :  LEVEL*16               ! key string

      REAL
     :  CNTROL( 4 ),           ! input AUTOGRAPH grid parameters
     :  GRID( 4 ),             ! input AUTOGRAPH axis control parameters
     :  HEIGHT,                ! height of the key
     :  XD( 2 ),               ! dummy x co-ordinates
     :  YD( 2 ),               ! dummy y co-ordinates
     :  YOFF                   ! y offset of current key entry in grid
                               ! co-ordinates

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Store current grid parameters

      CALL AGGETF( 'GRID/BOTTOM.', GRID(1) )
      CALL AGGETF( 'GRID/LEFT.', GRID(2) )
      CALL AGGETF( 'GRID/RIGHT.', GRID(4) )
      CALL AGGETF( 'GRID/TOP.', GRID(3) )

*    Maximise area of grid that can be used so that key text is clear

      CALL AGSETF( 'GRID/BOTTOM.', 0.001 )
      CALL AGSETF( 'GRID/LEFT.', 0.001 )
      CALL AGSETF( 'GRID/RIGHT.', 0.999 )
      CALL AGSETF( 'GRID/TOP.', 0.999 )

*    Store current axis control parameters

      CALL AGGETF( 'AXIS/LEFT/CONTROL.', CNTROL(2) )
      CALL AGGETF( 'AXIS/BOTTOM/CONTROL.', CNTROL(1) )
      CALL AGGETF( 'AXIS/RIGHT/CONTROL.', CNTROL(4) )
      CALL AGGETF( 'AXIS/TOP/CONTROL.', CNTROL(3) )

*    Store and reset the line width.
      CALL GETUSV( 'LW', LINWDT )
      CALL SETUSV( 'LW', NINT( THICK*1000 ) )

*    Plot dummy background (invisible axes and labels).  Use dummy data
*    beyond the key bounds.

      CALL AGSETF( 'AXIS/LEFT/CONTROL.', 0. )
      CALL AGSETF( 'AXIS/BOTTOM/CONTROL.', 0. )
      CALL AGSETF( 'AXIS/RIGHT/CONTROL.', 0. )
      CALL AGSETF( 'AXIS/TOP/CONTROL.', 0. )

      XD( 1 ) = -1000.
      YD( 1 ) = -1000.
      CALL SNX_EZRXY( XD, YD, 1, ' ', ' ', ' ' )

*    Find the total number of contours used.

      NOCONU = 0
      DO I = 1, NCONT
         IF ( CNTUSD( I ) ) NOCONU = NOCONU + 1
      END DO

*    Derive the total height of the key.  (1.3 + 0.7 + half-height of
*    the text = 2.5).

      IF ( UNITS .EQ. ' ' ) THEN
         HEIGHT = ( 2.5 + 1.5 * NOCONU ) * HG
      ELSE
         HEIGHT = ( 3.8 + 1.5 * NOCONU ) * HG
      END IF

*    Get a sensible offset, so the full key will always be visible.
*    Find the position to start the key.

      YOFF = MAX( MIN( FRMOFF, 1.0 ), HEIGHT ) - 0.7 * HG

*    Write the title.

      CALL SNX_WRTST( XOFF, YOFF, 'Contour heights', 1.115 * HG, 0, -2 )

*    Write the units offsetting downwards for the title.

      IF ( UNITS .NE. ' ' ) THEN
         YOFF = YOFF - 1.3 * HG
         CAPTIO = 'in '//UNITS
         CALL SNX_WRTST( XOFF, YOFF, CAPTIO, 1.0 * HG, 0, -2 )
      END IF

*    start with the first contour at the top of the key, allowing
*    for y positioning of the text is centred and for the key title

      YOFF = YOFF - 1.3 * HG
      DO I = 1, NCONT

*       If contour height has been used...

         IF ( CNTUSD( I ) ) THEN

*          Convert contour number and heights to strings

            CALL CHR_ITOC( I, CI, NC1 )
            CALL CHR_RTOC( CNTLEV( I ), CHT, NC2 )

*          Concatenate strings

            LEVEL = CI//': '//CHT(1:NC2)

*          Move down the zone to write next entry in the key

            YOFF = YOFF - 1.5 * HG

*          Write key entry, left-justified with good-quality typeface,
*          horizontal, height HG

           CALL SNX_WRTST( XOFF, YOFF, LEVEL(1:5+NC2), HG, 0, -2 )

         END IF

      END DO

*    Flush the output.

      CALL PLOTIT( 0, 0, 2 )

*    Restore changed parameters to their input values:

*    grid parameters

      CALL AGSETF( 'GRID/BOTTOM.', GRID(1) )
      CALL AGSETF( 'GRID/LEFT.', GRID(2) )
      CALL AGSETF( 'GRID/RIGHT.', GRID(4) )
      CALL AGSETF( 'GRID/TOP.', GRID(3) )

*    axis control parameters

      CALL AGSETF( 'AXIS/LEFT/CONTROL.', CNTROL(2) )
      CALL AGSETF( 'AXIS/BOTTOM/CONTROL.', CNTROL(1) )
      CALL AGSETF( 'AXIS/RIGHT/CONTROL.', CNTROL(4) )
      CALL AGSETF( 'AXIS/TOP/CONTROL.', CNTROL(3) )

*    line width
      CALL SETUSV( 'LW', LINWDT )

      END
