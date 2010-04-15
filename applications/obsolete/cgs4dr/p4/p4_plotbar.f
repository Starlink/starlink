*+  P4_PLOTBAR - Plot a colour scale bar alongside an image
      SUBROUTINE P4_PLOTBAR( PORT, STATUS )
*    Description :
*     Plots a colour scale bar alongside an IMAGE plot.
*    Invocation :
*     CALL P4_PLOTBAR( PORT, STATUS )
*    Authors :
*     J. Lightfoot (REVAD::JFL)
*     S. M. Beard (REVAD::SMB)
*     K. Krisciunas (JACH::KEVIN)
*     P. N. Daly (JACH::PND)
*    History :
*     1989:        Original version.                          (JFL)
*     27-Mar-1990: Code tidied up, constants parameterised
*                  and error messages made more explicit.     (SMB)
*     27-Mar-1990: Indecipherable character handling replaced
*                  by CHR routines.                           (SMB)
*     27-Nov-1990: Indecipherable gobbledegook deleted.       (SMB)
*     26-Jul-1991: Bug fix. This routine was defining a
*                  rectangular array which was horizontal in
*                  its longest dimension and then using the
*                  TRAN array given to PGGRAY to rotate this
*                  array through 90 degrees to become vertical!
*                  This caused PGGRAY to use a GKS segment
*                  tranformation, which not only made the
*                  routine inefficient but exacerbated a bug
*                  in the GKS/VWS driver which caused virtual
*                  memory to be used up. The original array is
*                  now made vertical in its longest dimension
*                  so no rotation is necessary.               (SMB)
*     07-Apr-1993: replace call to PGGRAY by call to PGPIXL (KLK)
*     25-Mar-1994: Fix crash when plotting only one column (PND)
*     30-Mar-1994: Make label stay on page for ports 5 and 6! (PND)
*      4-Aug-1994: Port to Unix (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER PORT                          ! Port number
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'P4COM.INC'                   ! P4 common block
      INCLUDE 'COLOURS.INC'                 ! P4 colours common block
*    Local Constants :
      INTEGER LEVELS                        ! Number of levels in bar in pixels
      PARAMETER ( LEVELS = 100 )
      INTEGER WIDTH                         ! Width of bar in pixels
      PARAMETER ( WIDTH = 1 )
*    Local variables :
      INTEGER J                             ! DO loop variable
      REAL VBX1, VBX2, VBY1, VBY2           ! Co-ords of bar viewport
      REAL INCR, OFFSET, XLAB, YLAB         ! Label position etc
      REAL SCALE( WIDTH, LEVELS )           ! Array for colour scale
      INTEGER IDATA( WIDTH, LEVELS )        ! PGPIXL input array
      INTEGER  RLEN                         ! Length of range label
      CHARACTER*40 RANGE                    ! Character string for data range
*-

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Define X dimension of viewport for bar
      IF ( ABS( VXEND( PORT ) - VXSTART( PORT ) ) .LT. 0.1 ) THEN
        VBX1 = VXEND( PORT ) + 0.10
        VBX2 = VXEND( PORT ) + 0.15
        OFFSET = 0.5
      ELSE
        VBX1 = VXEND( PORT ) + 0.02 * ( VXEND( PORT ) - VXSTART( PORT ) )
        VBX2 = VXEND( PORT ) + 0.07 * ( VXEND( PORT ) - VXSTART( PORT ) )
        OFFSET = 1.5
      END IF

*    Define Y dimension of viewport for bar
      IF ( ABS( VYEND( PORT ) - VYSTART( PORT ) ) .LT. 0.1 ) THEN
        VBY1 = VYSTART( PORT ) - 0.2
        VBY2 = VYEND( PORT ) + 0.2
      ELSE
        VBY1 = VYSTART( PORT ) + 0.2 * ( VYEND( PORT ) - VYSTART( PORT ) )
        VBY2 = VYSTART( PORT ) + 0.8 * ( VYEND( PORT ) - VYSTART( PORT ) )
      END IF

*    Define offset of viewport for bar
      IF ( PORT. LE. 4 ) THEN
        OFFSET = 1.5
      ELSE
        OFFSET = 0.7
      END IF
      IF ( PORT.EQ.7 .OR. PORT.EQ.8 ) OFFSET = 2.0

*    Check limits are inside display surface
      IF ( ( VBX1 .LT. 0.0 ) .OR. ( VBX2 .GT. 1.0 ) .OR.
     :     ( VBY1 .LT. 0.0 ) .OR. ( VBY2 .GT. 1.0 ) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'P4_PLOTBAR: '/
     :    /'No room for colour scale bar', STATUS )
      ELSE

*      Define viewport
        CALL PGVPORT( VBX1, VBX2, VBY1, VBY2 )

*      Define window to enclose scale array
        CALL PGWINDOW( 0.5, 1.5, 1.0, REAL( LEVELS ) )

*      Calculate scale array
        INCR = ( HIGH( PORT ) - LOW( PORT ) ) / REAL( LEVELS - 1 )
        DO J = 1, LEVELS
           SCALE( 1, J ) = LOW( PORT ) + REAL( J - 1 ) * INCR
        END DO
        CALL P4_SCALE_REAL( WIDTH, LEVELS, SCALE, LOW( PORT ),
     :    HIGH( PORT ), 2, CI2, IDATA, STATUS )

*      Reset the FG/BG levels
        CALL PGSCR( 0, BG_RD, BG_GR, BG_BL )
        CALL PGSCR( 1, FG_RD, FG_GR, FG_BL )

*      Plot the image
        CALL PGPIXL( IDATA, WIDTH, LEVELS, 1, WIDTH, 1,
     :    LEVELS, 0.5, 1.5, 1.0, REAL( LEVELS ) )

*      Finally, annotate it
        XLAB = 1.5 + OFFSET
        YLAB = ( REAL( LEVELS ) - 1.0 ) / 2.0
        RLEN = 0
        CALL CHR_PUTR( LOW( PORT ), RANGE, RLEN )
        CALL CHR_PUTC( ' (', RANGE, RLEN )
        CALL CHR_PUTI( CI1, RANGE, RLEN )
        CALL CHR_PUTC( ') - (', RANGE, RLEN )
        CALL CHR_PUTI( CI2, RANGE, RLEN )
        CALL CHR_PUTC( ') ', RANGE, RLEN )
        CALL CHR_PUTR( HIGH( PORT ), RANGE, RLEN )
        CALL PGPTEXT( XLAB, YLAB, 90.0, 0.5, RANGE(1:RLEN) )
      ENDIF
      END
