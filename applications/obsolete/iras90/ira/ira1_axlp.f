      SUBROUTINE IRA1_AXLP( IDA, SCS, ALAX, BLAX, BCEN, GAPLAT, IPHI,
     :                      IPLO, LABELS, TICKLE, LBND, UBND, ACC,
     :                      LAXMIN, STATUS )
*+
*  Name:
*     IRA1_AXLP

*  Purpose:
*     Produce latitude labels and ticks along a meridian.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_AXLP( IDA, SCS, ALAX, BLAX, BCEN, GAPLAT, IPHI, IPLO,
*                     LABELS, TICKLE, LBND, UBND, ACC, LAXMIN, STATUS )

*  Description:
*     If LABELS is true, this routine produces latitude labels placed
*     at equal intervals along a specified meridian. Also, if the
*     drawing of lines has been supressed using the LINES graphics
*     option (see routine IRA_DROPT), it produces short tick marks at
*     regular intervals along the meridian. Each of these tick marks
*     is actually a short section of a parallel intersecting the
*     meridian.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     SCS = CHARTACTER (Given)
*        The sky coordinate system, An abbreviation will do. If no
*        equinox is supplied, a default is used.
*     ALAX = DOUBLE PRECISION (Given and Returned)
*        The longitude of the meridian along which the labels and ticks
*        are to be placed.
*     BLAX = DOUBLE PRECISION (Given and Returned)
*        The latitude of the parallel which will receive labels and
*        ticks.
*     BCEN = DOUBLE PRECISION (Given)
*        The latitude of the parallel with index zero.
*     GAPLAT = DOUBLE PRECISION (Given)
*        The latitude gap between parallels.
*     IPHI = INTEGER (Given)
*        The highest parallel index.
*     IPLO = INTEGER (Given)
*        The lowest parallel index.
*     LABELS = LOGICAL (Given)
*        True if labels are to be drawn.
*     TICKLE = DOUBLE PRECISION (Given)
*        The arc-length of tick marks, in radians.
*     LBND( 2 ) = REAL (Given)
*        Lower world coordinate bounds for each axis defining the
*        section of the current SGS zone in which the curve is to be
*        drawn.
*     UBND( 2 ) = REAL (Given)
*        Upper world coordinate bounds for each axis defining the
*        section of the current SGS zone in which the curve is to be
*        drawn.
*     ACC = DOUBLE PRECISION (Given)
*        The accuracy of the displayed latitude values, in radians.
*     LAXMIN = REAL (Given and Returned)
*        The lowest X coordinate covered by any coordinate labels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-MAR-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_DROPT( 6 ) = DOUBLE PRECISION (Read and Write)
*           The values of the graphics options set up by routine
*           IRA_DROPT.
*        ACM_DRVPO( 5 ) = REAL (Read)
*           Values defining the area occupied by the text, in the
*           order; (X,Y) at start of box, (X,Y) at end of box, height
*           of box (perpendicular to the line joing start and end of
*           box).

*  Arguments Given:
      INTEGER IDA
      CHARACTER SCS*(*)
      DOUBLE PRECISION ALAX
      DOUBLE PRECISION BLAX
      DOUBLE PRECISION BCEN
      DOUBLE PRECISION GAPLAT
      INTEGER IPHI
      INTEGER IPLO
      LOGICAL LABELS
      DOUBLE PRECISION TICKLE
      REAL LBND( 2 )
      REAL UBND( 2 )
      DOUBLE PRECISION ACC

*  Arguments Given and Returned:
      REAL LAXMIN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER        BJ*1      ! The type of epoch (Besselian or
                                 ! Julian) held by variable EQU.
      DOUBLE PRECISION BLAB
      REAL             BREAK( 2, IRA__MXBRK )
      CHARACTER        CONTXT*10 ! Label context.
      CHARACTER        CONSAV*10 ! Saved label context.
      DOUBLE PRECISION COSB
      DOUBLE PRECISION EQU       ! The epoch of the reference equinox
                                 ! specified in argument SCS.
      INTEGER          I         ! Loop count
      INTEGER          IHI       ! High limit of parallel index.
      INTEGER          ILO       ! Low limit of parallel index.
      DOUBLE PRECISION INCA
      INTEGER          INCI      ! Increment in parallel index.
      INTEGER          IPAR      ! Index of labelled parallel.
      INTEGER          IZERO     ! Index of parallel with latitude
                                 ! closest to zero.
      INTEGER          J         ! Loop count
      REAL             LENGTH
      DOUBLE PRECISION LINES     ! Negative if curves are to be drawn.
      REAL             MINX      ! Min. X coord. covered by value just
                                 ! drawn.
      CHARACTER        NAME*(IRA__SZSCS)! Full SCS name.
      INTEGER          NBREAK
      LOGICAL          OUT
      REAL             SEPLIM
      INTEGER          STYLE     ! Formatting style.
      REAL             VBREAK( 2, IRA__MXBRK )
      REAL             XPOS
      REAL             XPOSL
      REAL             YPOS
      REAL             YPOSL

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invert the current value of the 'LINES' graphics option. This results
*  in visible tick marks being produced by IRA_DRPAR if lines have been
*  suppressed, and no ticks being drawn if visible lines have been
*  drawn.
      ACM_DROPT( 6 ) = -ACM_DROPT( 6 )

*  Get the full name of the sky coordinate system, and set the style
*  number.
      CALL IRA1_CHSCS( SCS, NAME, EQU, BJ, STATUS )
      IF( NAME( : 10 ) .EQ. 'EQUATORIAL' ) THEN
         STYLE = 2
      ELSE
         STYLE = 5
      END IF

*  Latitude labels are produced with BR justification.
      CALL SGS_STXJ( 'BC' )

*  Find the index of the labelled parallel.
      IPAR = NINT( ( BLAX - BCEN )/GAPLAT )

*  Find the index of the parallel with latitude closest to zero.
      IZERO = MAX( IPLO, MIN( IPHI, NINT( -BCEN/GAPLAT ) ) )

*  Set up the minimum distance between printable labels.
      SEPLIM = 0.03*MAX( UBND( 1 ) - LBND( 1 ), UBND( 2 ) - LBND( 2 ) )

*  Initialise "last times label position".
      XPOSL = VAL__MAXR
      YPOSL = VAL__MAXR

*  Latitude labels are produced in an order which ensures that the
*  absolute value of successive labels always increases. This is done to
*  make sure that un-changed leading fields are not omitted when they
*  should be included. Latitude increases with parallel index, so start
*  at the closest parallel to zero and work up to the parallel with
*  largest latitude. Then work down from the closest parallel to zero
*  to the parallel with minimum latitude.
      ILO = IZERO
      IHI = IPHI
      INCI = 1

*  Do each of the two batches of labels.
      DO J = 1, 2

*  Loop round each parallel in this batch.
         DO I = ILO, IHI, INCI

*  Get the latitude at this parallel.
            BLAB = BCEN + GAPLAT*REAL( I )
            CALL IRA_NORM( ALAX, BLAB, STATUS )

*  Set up a longitude increment which corresponds to an arc-distance of
*  TICKLE. Limit it to PI.
            COSB = COS( BLAB )
            IF( COSB .GT. TICKLE/IRA__PI ) THEN
               INCA = TICKLE/COSB
            ELSE
               INCA = IRA__PI
            END IF

*  Draw a short section of the parallel, centred on the labelled
*  meridian.
            CALL IRA_DRPAR( IDA, ALAX - 0.5*INCA, BLAB, INCA, SCS, LBND,
     :                      UBND, STATUS )

*  Get information about the breaks in the curve just drawn.
            CALL IRA_DRBRK( IRA__MXBRK, OUT, BREAK, VBREAK, NBREAK,
     :                      LENGTH, STATUS )

*  Find the mid point of the parallel section.
            IF( NBREAK .GE. 2 ) THEN
               XPOS = 0.5*( BREAK( 1, 1 ) + BREAK( 1, 2 ) )
               YPOS = 0.5*( BREAK( 2, 1 ) + BREAK( 2, 2 ) )

*  If labels are required...
               IF( LABELS .AND.
     :             XPOS .GT. LBND( 1 ) .AND. XPOS .LT. UBND( 1 ) .AND.
     :             YPOS .GT. LBND( 2 ) .AND. YPOS .LT. UBND( 2 ) ) THEN

*  Ensure that the intersection of the two labelled axes has a full
*  label.
                  IF( I .EQ. IPAR ) CONTXT = ' '

*  Produce the latitude label so long as it is far enough away from
*  the last label.
                  IF( ABS( XPOS - XPOSL ) .GT. SEPLIM .OR.
     :                ABS( YPOS - YPOSL ) .GT. SEPLIM ) THEN

                     CALL IRA1_IDRVA( BLAB, NAME, 2, XPOS, YPOS, STYLE,
     :                                ACC, CONTXT, STATUS )

*  Update the lowest X coordinate covered by any coordinate value.
                  MINX = MIN( ACM_DRVPO( 1 ), ACM_DRVPO( 3 ) ) -
     :                   0.5*ACM_DRVPO( 5 )
                  LAXMIN = MIN( LAXMIN, MINX )

                  END IF

*  Save the current X and Y position.
                  XPOSL = XPOS
                  YPOSL = YPOS

*  If this is the closest label to zero, save the context.
                  IF( I .EQ. IZERO ) CONSAV = CONTXT

               END IF

            END IF

         END DO

*  Restore the context from the label of the parallel with latitude
*  closest to zero.
         CONTXT = CONSAV

*  Work downwards through parallels with negative latitude values.
         IHI = IPLO
         ILO = IZERO - 1
         INCI = -1

      END DO

*  Reinstate the original value for the 'LINES' graphics option.
      ACM_DROPT( 6 ) = -ACM_DROPT( 6 )

      END
