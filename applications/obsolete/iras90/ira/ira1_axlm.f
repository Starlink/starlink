      SUBROUTINE IRA1_AXLM( IDA, SCS, ALAX, BLAX, ACEN, GAPLON, IMHI,
     :                      IMLO, LABELS, TICKLE, LBND, UBND, ACC,
     :                      LAXMIN, STATUS )
*+
*  Name:
*     IRA1_AXLM

*  Purpose:
*     Produce longitude labels and ticks along a parallel.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_AXLM( IDA, SCS, ALAX, BLAX, ACEN, GAPLON, IMHI, IMLO,
*                     LABELS, TICKLE, LBND, UBND, ACC, LAXMIN, STATUS )

*  Description:
*     If LABELS is true, this routine produces longitude labels placed
*     at equal intervals along a specified parallel. Also, if the
*     drawing of lines has been supressed using the LINES graphics
*     option (see routine IRA_DROPT), it produces short tick marks at
*     regular intervals along the parallel. Each of these tick marks
*     is actually a short section of a meridian intersecting the
*     parallel.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     SCS = CHARTACTER (Given)
*        The sky coordinate system, An abbreviation will do. If no
*        equinox is supplied, a default is used.
*     ALAX = DOUBLE PRECISION (Given and Returned)
*        The longitude of the meridian which will receive labels and
*        ticks.
*     BLAX = DOUBLE PRECISION (Given and Returned)
*        The latitude of the parallel along which the labels and ticks
*        are to be placed.
*     ACEN = DOUBLE PRECISION (Given)
*        The longitude of the meridian with index zero.
*     GAPLON = DOUBLE PRECISION (Given)
*        The longitude gap between meridians.
*     IMHI = INTEGER (Given)
*        The highest meridian index.
*     IMLO = INTEGER (Given)
*        The lowest meridian index.
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
*        The accuracy for the displayed longitude values.
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
      INCLUDE 'IRA_PAR'          ! IRA_ constants.
      INCLUDE 'PRM_PAR'          ! Starlink data constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
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
      DOUBLE PRECISION ACEN
      DOUBLE PRECISION GAPLON
      INTEGER IMHI
      INTEGER IMLO
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
      DOUBLE PRECISION ALAB
      CHARACTER        BJ*1      ! The type of epoch (Besselian or
                                 ! Julian) held by variable EQU.
      REAL             BREAK( 2, IRA__MXBRK )
      CHARACTER        CONTXT*10 ! Label context.
      DOUBLE PRECISION EQU       ! The epoch of the reference equinox
                                 ! specified in argument SCS.
      INTEGER          I         ! Loop count
      INTEGER          IMER      ! Index of labelled meridian.
      REAL             LENGTH
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

*  Get the full name of the sky coordinate system, and set the style
*  number.
      CALL IRA1_CHSCS( SCS, NAME, EQU, BJ, STATUS )
      IF( NAME( : 10 ) .EQ. 'EQUATORIAL' ) THEN
         STYLE = 2
      ELSE
         STYLE = 5
      END IF

*  Longitude labels are produced with TC justification.
      CALL SGS_STXJ( 'TC' )

*  Find the index of the labelled meridian.
      IMER = NINT( ( ALAX - ACEN )/GAPLON )

*  Set up the minimum distance between printable labels.
      SEPLIM = 0.03*MAX( UBND( 1 ) - LBND( 1 ), UBND( 2 ) - LBND( 2 ) )

*  Initialise "last times label position".
      XPOSL = VAL__MAXR
      YPOSL = VAL__MAXR

*  Loop round each meridian, placing a label and a tick mark at the
*  intersection of the meridian and the parallel.
      DO I = IMLO, IMHI

*  Get the longitude at this meridian.
         ALAB = ACEN + GAPLON*REAL( I )
         CALL IRA_NORM( ALAB, BLAX, STATUS )

*  Draw a short section of the meridian, centred on the labelled
*  parallel.
         CALL IRA_DRMER( IDA, ALAB, BLAX - 0.5*TICKLE, TICKLE, SCS,
     :                   LBND, UBND, STATUS )

*  Get information about the breaks in the curve just drawn.
         CALL IRA_DRBRK( IRA__MXBRK, OUT, BREAK, VBREAK, NBREAK,
     :                   LENGTH, STATUS )

*  Find the mid point of the meridian section.
         IF( NBREAK .GE. 2 ) THEN
            XPOS = 0.5*( BREAK( 1, 1 ) + BREAK( 1, 2 ) )
            YPOS = 0.5*( BREAK( 2, 1 ) + BREAK( 2, 2 ) )

*  If labels are required...
            IF( LABELS .AND. XPOS .GT. LBND( 1 ) .AND.
     :          XPOS .LT. UBND( 1 ) .AND. YPOS .GT. LBND( 2 ) .AND.
     :          YPOS .LT. UBND( 2 ) ) THEN

*  Ensure that the intersection of the two labelled axes has a full
*  label.
               IF( I .EQ. IMER ) CONTXT = ' '

*  Produce the longitude label so long as it is far enough away from
*  the last label.
               IF( ABS( XPOS - XPOSL ) .GT. SEPLIM .OR.
     :             ABS( YPOS - YPOSL ) .GT. SEPLIM ) THEN

                  CALL IRA1_IDRVA( ALAB, NAME, 1, XPOS, YPOS, STYLE,
     :                             ACC, CONTXT, STATUS )

*  Update the lowest X coordinate covered by any coordinate value.
                  MINX = MIN( ACM_DRVPO( 1 ), ACM_DRVPO( 3 ) ) -
     :                   0.5*ACM_DRVPO( 5 )
                  LAXMIN = MIN( LAXMIN, MINX )

               END IF

*  Save the current X and Y position.
               XPOSL = XPOS
               YPOSL = YPOS

            END IF

         END IF

      END DO

      END
