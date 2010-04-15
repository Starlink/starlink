      SUBROUTINE SPD_WYZA( PICID, OVER,
     :   LIN, BIN, MARK, ERROR, WIDTH, FRAME,
     :   TEXT, LEGIVN, LABEL, VIEW, WORLD, LEGEND, CELLSZ,
     :   LABSPC, ROMAN, CHIGHT, COLOUR, THICK,
     :   AXES, TICK, NUML, MAJOR, MINOR, DASH,
     :   SVISND, NCHAN, NROWS, COORD1, COORD2, XVAL, DATA, STATUS )
*+
*  Name:
*     SPD_WYZA

*  Purpose:
*     Do the plot for SPECGRID.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WYZA( PICID, OVER, LIN, BIN, MARK, ERROR, WIDTH, FRAME,
*        TEXT, LEGIVN, LABEL, VIEW, WORLD, LEGEND, CELLSZ,
*        LABSPC, ROMAN, CHIGHT, COLOUR, THICK,
*        AXES, TICK, NUML, MAJOR, MINOR, DASH,
*        SVISND, NCHAN, NROWS, COORD1, COORD2, XVAL, DATA, STATUS )

*  Description:
*     This routine does the plotting for SPECGRID. The view surface is
*     derived from the given AGI picture. Depending on whether the new
*     plot is an overlay or not, will there be space left on the edges
*     of the view surface to label the coordinate view port.
*
*     There are two view ports and plot windows. An outer one represents
*     coordinate space. Each row of the data array is assigned a single
*     position in coordinate space. This position is the centre of a
*     smaller view port with a spectroscopic plot window. That is to say
*     the spectrum (row of the data array) is plotted in a small cell
*     which is positioned on the bigger plot according to its
*     coordinates.
*
*     Each spectrum is plotted in a similar manner (with similar
*     controls) as with SPECPLOT.

*  Arguments:
*     PICID = INTEGER (Given)
*        The AGI picture identifier. Depending on OVER this is a DATA
*        picture to be used as view port or a picture inside which a new
*        view port and DATA picture is to be created.
*     OVER = INTEGER (Given)
*        Non-zero if the plot is to be an overlay on a previous plot. In
*        this case the AGI data base is checked for a recent DATA
*        picture to define the view port.
*     LIN = INTEGER (Given)
*        Non-zero if the spectra are to be plotted as lines connecting
*        points.
*     BIN = INTEGER (Given)
*        Non-zero if the spectra are to be plotted in histogram style.
*     MARK = INTEGER (Given)
*        The marker code to be used for the data points of the spectra.
*     ERROR = INTEGER (Given)
*        Non-zero if the spectra are to be plotted with error bars.
*     WIDTH = INTEGER (Given)
*        Non-zero if the spectra are to be plotted with width bars.
*     FRAME = INTEGER (Given)
*        Non-zero if each spectral cell is to get a plain box.
*     TEXT( 4 ) = INTEGER (Given)
*        For each axis in coordinate space (bottom, left, top, right),
*        telling whether a text label is to be drawn. Used only if OVER
*        is zero (false).
*     LEGIVN = INTEGER (Given)
*        Non-zero if a position in coordinate space is given for a
*        legend cell. If zero, no legend cell will be drawn.
*     LABEL( 6 ) = CHARACTER * ( * ) (Given)
*        The plot labels to be used for the bottom, left, top, right
*        edge of the coordinate view port, and for the bottom and left
*        of the legend cell. Each label is used only if TEXT() indicates
*        so, or if LEGIVN indicates that a legend cell is to be drawn.
*     VIEW( 4 ) = REAL (Given)
*        The view port for coordinate space.
*     WORLD( 8 ) = REAL (Given)
*        The first four elements are the plot window in coordinate
*        space. The latter four elements are the plot window for each
*        spectral cell.
*     LEGEND( 2 ) = REAL (Given)
*        The position in coordinate space where the legend cell is to be
*        drawn. Not used if LEGIVN is zero (false).
*     CELLSZ( 2 ) = REAL (Given)
*        The size of the spectral cells measured in coordinate space.
*     LABSPC( 4 ) = REAL (Given)
*        The fraction of the view surface to be reserved for labels. The
*        elements apply to the bottom, left, top, right in that order.
*        Each element must be between 0. and 0.5, or this routine will
*        abort with an error message. Used only if OVER is zero (false).
*     ROMAN = INTEGER (Given)
*        Non-zero if the double-stroke roman font is to be used.
*     CHIGHT = REAL (Given)
*        Character height in PGPLOT units. This height applies to the
*        box labels. Legend cell labels are plotted half this size.
*     COLOUR = INTEGER (Given)
*        PGPLOT pen number to be used.
*     THICK = INTEGER (Given)
*        PGPLOT line thickness parameter.
*     AXES( 4 ) = INTEGER (Given)
*        0 or 1, telling whether the axis is to be drawn. 1 for yes.
*        Used only if OVER is zero (false).
*     TICK( 4 ) = INTEGER (Given)
*        -1, 0, or +1, telling whether ticks are to be drawn at all and
*        whether they are inside or outside the box. +1 for outside.
*        Used only if OVER is zero (false).
*     NUML( 4 ) = INTEGER (Given)
*        Telling whether numeric labels are to be drawn. 0 for no. The
*        first and third element can be 1 for yes. The second and fourth
*        element can be +1 for horizontal and -1 for vertical labels.
*        Used only if OVER is zero (false).
*     MAJOR( 2 ) = REAL (Given)
*        Distance between major tick marks for horizontal and vertical
*        directions. Used only if OVER is zero (false).
*     MINOR( 2 ) = INTEGER (Given)
*        Number of minor tick intervals per major tick interval for
*        horizontal and vertical directions. Used only if OVER is zero
*        (false).
*     DASH = INTEGER (Given)
*        PGPLOT line style (dash pattern) parameter.
*     SVISND = INTEGER (Given)
*        Non-zero if XVAL is actually n-dimensional. XVAL will always be
*        declared as NCHAN by NROWS, but only the first row will be used
*        if this is zero.
*     NCHAN = INTEGER (Given)
*        Number of channels in each spectrum, the first dimension of the
*        data array.
*     NROWS = INTEGER (Given)
*        Number of rows (spectra) in the data array, second dimension of
*        the data array.
*     COORD1( NROWS ) = REAL (Given)
*        The first coordinate for each spectrum. This specifies where
*        along the horizontal axis each spectrum will be drawn.
*     COORD2( NROWS ) = REAL (Given)
*        The second coordinate for each spectrum. This specifies where
*        along the vertical axis each spectrum will be drawn.
*     XVAL( NCHAN, NROWS ) = REAL (Given)
*        The horizontal coordinate in spectroscopic space for each point
*        in each spectrum. If SVISND is zero, then all spectra share the
*        same, first, row of this array.
*     DATA( NCHAN, NROWS ) = REAL (Given)
*        The vertical coordinate in spectroscopic space for each point
*        in each spectrum. Each row is a spectrum.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     20 Sep 1994 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER PICID
      INTEGER OVER
      INTEGER LIN
      INTEGER BIN
      INTEGER MARK
      INTEGER ERROR
      INTEGER WIDTH
      INTEGER FRAME
      INTEGER TEXT( 4 )
      INTEGER LEGIVN
      CHARACTER * ( * ) LABEL( 6 )
      REAL VIEW( 4 )
      REAL WORLD( 8 )
      REAL LEGEND( 2 )
      REAL CELLSZ( 2 )
      REAL LABSPC( 4 )
      INTEGER ROMAN
      REAL CHIGHT
      INTEGER COLOUR
      INTEGER THICK
      INTEGER AXES( 4 ), TICK( 4 ), NUML( 4 )
      REAL MAJOR( 2 )
      INTEGER MINOR( 2 )
      INTEGER DASH
      INTEGER SVISND
      INTEGER NCHAN, NROWS
      REAL COORD1( NROWS )
      REAL COORD2( NROWS )
      REAL XVAL( NCHAN, NROWS )
      REAL DATA( NCHAN, NROWS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Temporary integer
      INTEGER PGMARK( 6 )        ! Map our to PGPLOT's marker numbers
      INTEGER LMARK              ! Translated marker number
      REAL DELTAX, DELTAY        ! Size of coordinate view port
      REAL NORM( 4 )             ! Spectroscopic view port location
      REAL XMAJOR, YMAJOR        ! Legend cell major tick intervals
      REAL LEFT, RIGHT, N        ! To calculate legend tick intervals
      REAL DIGITS, FACTOR        ! dto.

*  Local Data:
      DATA PGMARK / 5, 3, 4, 0, 17, 16 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Plot attributes.
*  ================

      IF ( ROMAN .EQ. 0 ) THEN
         CALL PGSCF( 1 )
      ELSE
         CALL PGSCF( 2 )
      END IF
      CALL PGSCH( CHIGHT )
      CALL PGSCI( COLOUR )
      CALL PGSLW( THICK )
      IF ( MARK .GE. 1 .AND. MARK .LE. 6 ) THEN
         LMARK = PGMARK(MARK)
      ELSE
         LMARK = 0
      END IF


*  Plot scales.
*  ============

*  Plot scales: Extent of coordinate view port in units of view surface
*  size.
*  To specify a view port we need to work in the normalised coordinates
*  that run from 0 to 1 over the view surface. But internally we have to
*  use the coordinate space. The two are related as follows:
*
*     NORM1 = ( COORD1 - WORLD1 ) / ( WORLD2 - WORLD1 )
*           * ( VIEW2 - VIEW1 )
*           + VIEW1
*
*     NORM2 = ( COORD2 - WORLD3 ) / ( WORLD4 - WORLD3 )
*           * ( VIEW4 - VIEW3 )
*           + VIEW3
*
      DELTAX = VIEW(2) - VIEW(1)
      DELTAY = VIEW(4) - VIEW(3)


*  Legend box.
*  ===========

*  Legend box view port.
      NORM(1) = ( LEGEND(1) - CELLSZ(1) / 2. - WORLD(1) )
     :        * DELTAX / ( WORLD(2) - WORLD(1) ) + VIEW(1)
      NORM(2) = ( LEGEND(1) + CELLSZ(1) / 2. - WORLD(1) )
     :        * DELTAX / ( WORLD(2) - WORLD(1) ) + VIEW(1)
      NORM(3) = ( LEGEND(2) - CELLSZ(2) / 2. - WORLD(3) )
     :        * DELTAY / ( WORLD(4) - WORLD(3) ) + VIEW(3)
      NORM(4) = ( LEGEND(2) + CELLSZ(2) / 2. - WORLD(3) )
     :        * DELTAY / ( WORLD(4) - WORLD(3) ) + VIEW(3)

*  If the view port is wholly inside the coordinate view port.
      IF ( NORM(1) .GE. VIEW(1) .AND. NORM(1) .LE. VIEW(2) .AND.
     :     NORM(2) .GE. VIEW(1) .AND. NORM(2) .LE. VIEW(2) .AND.
     :     NORM(3) .GE. VIEW(3) .AND. NORM(3) .LE. VIEW(4) .AND.
     :     NORM(4) .GE. VIEW(3) .AND. NORM(4) .LE. VIEW(4) ) THEN

*     Set view port and window.
         CALL PGSVP( NORM(1), NORM(2), NORM(3), NORM(4) )
         CALL PGSWIN( WORLD(5), WORLD(6), WORLD(7), WORLD(8) )


*     Legend box tick spacing.
*     ------------------------

*     Work out the major tick spacing for the legend box.
*     The aim is to have exactly two major ticks. Minor ticks can be
*     supressed simply by using LMINOR=1.
*     Major ticks are at multiples of LMAJOR, and we must adjust that to
*     give us two ticks in the window range.

*     If the origin is in the x range.
         IF ( WORLD(5) * WORLD(6) .LE. 0. ) THEN

*        The following yields one tick at the origin and another at the
*        end that is furthest away from the origin.
            XMAJOR = MAX( ABS(WORLD(5)), ABS(WORLD(6)) )

         ELSE

*        Use an equivalent range that is positive and increasing.
            LEFT  = MIN( ABS(WORLD(5)), ABS(WORLD(6)) )
            RIGHT = MAX( ABS(WORLD(5)), ABS(WORLD(6)) )

*        We can fix the centre of the only visible tick interval on the
*        centre of the range.
*        For the moment we assume further that the tick interval can be
*        the range itself. This will yield the ticks to be non-integer
*        multiples of the tick interval length. We will fix that below.
*        Said multiple is:
            N = LEFT / ( RIGHT - LEFT )

*        This a positive floating point number. We need a positive
*        integer. Also in adjusting N we have to make the tick interval
*        shorter, not longer. Thus we have to increase N to the next
*        biggest integer. That will be a positive integer.
            N = FLOAT( 1 + INT( N ) )

*        Thus we would have as tick interval:
            XMAJOR = ( LEFT + RIGHT ) / ( 2. * N + 1. )

*        In the limit of LEFT -> +0, N = 1 and LMAJOR -> RIGHT / 3. Thus
*        there is a danger that we get three ticks, at RIGHT, 2/3 *
*        RIGHT and 1/3 * RIGHT. We want to avoid this. This is easy, we
*        know that we want the two ticks at RIGHT and at RIGHT / 2.
            IF ( RIGHT .GE. 3. * LEFT ) THEN
               XMAJOR = RIGHT / 2.
            END IF

         END IF

*     We are still not happy with XMAJOR, because it can have any number
*     of significant digits. We want to limit that to one or two.

*     The number MAJOR * 10 ** ( 2 - NINT(0.5+LG(MAJOR)) ) retains the
*     digits of MAJOR, but is in the range [10,100[. Thus
*     INT(MAJOR*10**(2-NINT(0.5+LG(MAJOR)))) is the integer with the two
*     first significant digits. It is in [10,99].
         FACTOR = 10. ** ( 2. - NINT(0.5+LOG10(XMAJOR)) )
         DIGITS = FLOAT( INT( XMAJOR * FACTOR ) )

*     The following IF clauses work out a factor that will reduce
*     MAJOR to two or less significant digits. In the process MAJOR may
*     become smaller, but not bigger.
*     Remember that MAJOR * FACTOR is of the same magnitude as DIGITS
*     but retains all significant digits. Thus the resulting factor is
*     of order but less than 1.
         IF      ( DIGITS .LT. 15 ) THEN
            FACTOR = 10. / ( XMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 20 ) THEN
            FACTOR = 15. / ( XMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 25 ) THEN
            FACTOR = 20. / ( XMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 30 ) THEN
            FACTOR = 25. / ( XMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 40 ) THEN
            FACTOR = 30. / ( XMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 50 ) THEN
            FACTOR = 40. / ( XMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 60 ) THEN
            FACTOR = 50. / ( XMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 70 ) THEN
            FACTOR = 60. / ( XMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 80 ) THEN
            FACTOR = 70. / ( XMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 90 ) THEN
            FACTOR = 80. / ( XMAJOR * FACTOR )
         ELSE
            FACTOR = 90. / ( XMAJOR * FACTOR )
         END IF

*     Now apply the factor to MAJOR.
*     This multiplication retains the magnitude of MAJOR but adjusts it
*     down to eliminate the digits that we regard as insignificant.
         XMAJOR = FACTOR * XMAJOR

*     And the same for the vertical direction.
         IF ( WORLD(7) * WORLD(8) .LE. 0. ) THEN
            YMAJOR = MAX( ABS(WORLD(7)), ABS(WORLD(8)) )
         ELSE
            LEFT  = MIN( ABS(WORLD(7)), ABS(WORLD(8)) )
            RIGHT = MAX( ABS(WORLD(7)), ABS(WORLD(8)) )
            N = LEFT / ( RIGHT - LEFT )
            N = FLOAT( 1 + INT( N ) )
            YMAJOR = ( LEFT + RIGHT ) / ( 2. * N + 1. )
            IF ( RIGHT .GE. 3. * LEFT ) THEN
               YMAJOR = RIGHT / 2.
            END IF
         END IF
         FACTOR = 10. ** ( 2. - NINT(0.5+LOG10(YMAJOR)) )
         DIGITS = FLOAT( INT( YMAJOR * FACTOR ) )
         IF      ( DIGITS .LT. 15 ) THEN
            FACTOR = 10. / ( YMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 20 ) THEN
            FACTOR = 15. / ( YMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 25 ) THEN
            FACTOR = 20. / ( YMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 30 ) THEN
            FACTOR = 25. / ( YMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 40 ) THEN
            FACTOR = 30. / ( YMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 50 ) THEN
            FACTOR = 40. / ( YMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 60 ) THEN
            FACTOR = 50. / ( YMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 70 ) THEN
            FACTOR = 60. / ( YMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 80 ) THEN
            FACTOR = 70. / ( YMAJOR * FACTOR )
         ELSE IF ( DIGITS .LT. 90 ) THEN
            FACTOR = 80. / ( YMAJOR * FACTOR )
         ELSE
            FACTOR = 90. / ( YMAJOR * FACTOR )
         END IF
         YMAJOR = FACTOR * YMAJOR


*     Plot legend box and labels.
*     ---------------------------

         CALL PGSCH( CHIGHT/2. )
         CALL PGBOX( 'BCTSN', XMAJOR, 1, 'BCTSN', YMAJOR, 1 )
         CALL PGLAB( LABEL(5), LABEL(6), ' ' )
         CALL PGSCH( CHIGHT )

      END IF


*  Plot spectra.
*  =============

*  For each spectrum.
      DO 1001 I = 1, NROWS

*     Work out view port.
         NORM(1) = ( COORD1(I) - CELLSZ(1) / 2. - WORLD(1) )
     :           * DELTAX / ( WORLD(2) - WORLD(1) ) + VIEW(1)
         NORM(2) = ( COORD1(I) + CELLSZ(1) / 2. - WORLD(1) )
     :           * DELTAX / ( WORLD(2) - WORLD(1) ) + VIEW(1)
         NORM(3) = ( COORD2(I) - CELLSZ(2) / 2. - WORLD(3) )
     :           * DELTAY / ( WORLD(4) - WORLD(3) ) + VIEW(3)
         NORM(4) = ( COORD2(I) + CELLSZ(2) / 2. - WORLD(3) )
     :           * DELTAY / ( WORLD(4) - WORLD(3) ) + VIEW(3)

*     If the view port is wholly inside the coordinate view port.
         IF ( NORM(1) .GE. VIEW(1) .AND. NORM(1) .LE. VIEW(2) .AND.
     :        NORM(2) .GE. VIEW(1) .AND. NORM(2) .LE. VIEW(2) .AND.
     :        NORM(3) .GE. VIEW(3) .AND. NORM(3) .LE. VIEW(4) .AND.
     :        NORM(4) .GE. VIEW(3) .AND. NORM(4) .LE. VIEW(4) ) THEN

*        Set view port and window.
            CALL PGSVP( NORM(1), NORM(2), NORM(3), NORM(4) )
            CALL PGSWIN( WORLD(5), WORLD(6), WORLD(7), WORLD(8) )

*        Plot spectral cell frame.
            IF ( FRAME .NE. 0 ) CALL PGBOX( 'BC', 0., 0, 'BC', 0., 0 )

*        Plot spectral cell data.
            IF ( SVISND .NE. 0 ) THEN
               CALL SPD_WAAD( ERROR.NE.0, WIDTH.NE.0, LIN.NE.0,
     :            BIN.NE.0, (MARK.GE.1.AND.MARK.LE.6), DASH, LMARK,
     :            NCHAN, XVAL(1,I), DATA(1,I), 0., 0., 0., 0., STATUS )
            ELSE
               CALL SPD_WAAD( ERROR.NE.0, WIDTH.NE.0, LIN.NE.0,
     :            BIN.NE.0, (MARK.GE.1.AND.MARK.LE.6), DASH, LMARK,
     :            NCHAN, XVAL, DATA(1,I), 0., 0., 0., 0., STATUS )
            END IF
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 500
 1001 CONTINUE


*  Plot coordinate box.
*  ====================

*  Plot coordinate space box with labels.
*  Save coordinate view port and window as AGI picture.
      IF ( OVER .EQ. 0 ) THEN
         CALL PGSVP( VIEW(1), VIEW(2), VIEW(3), VIEW(4) )
         CALL PGSWIN( WORLD(1), WORLD(2), WORLD(3), WORLD(4) )
         CALL SPD_WAAC( AXES, TICK, NUML, MAJOR, MINOR,
     :      CHIGHT, TEXT, VIEW, LABSPC, LABEL, STATUS )
         CALL SPD_UGAD( 'DATA', 'SPECDRE_SPECGRID', I, STATUS )
      END IF

*  Return.
 500  CONTINUE
      END
