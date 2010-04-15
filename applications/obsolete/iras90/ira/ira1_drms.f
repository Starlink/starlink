      SUBROUTINE IRA1_DRMS( ACEN, BCEN, GAPLON, GAPLAT, SCS, IDA, IMLO,
     :                      IMHI, IPLO, IPHI, LBND, UBND,
     :                      MAXLAB, MAXTIC, ALAX, LABS, NLABS, TICKS,
     :                      NTICKS, STATUS )
*+
*  Name:
*     IRA1_DRMS

*  Purpose:
*     Draw a set of meridians.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_DRMS( ACEN, BCEN, GAPLON, GAPLAT, SCS, IDA, IMLO, IMHI,
*                     IPHI, IPLO, LBND, UBND, MAXLAB,
*                     MAXTIC, ALAX, LABS, NLABS, TICKS, NTICKS, STATUS )

*  Description:
*     This routine draws each of the meridians in turn. Positions and
*     directions of longitude tick marks are returned, together with
*     information about the positions of longitude "end" labels (i.e.
*     longitude labels placed at the start or end of each meridian).
*     The longest meridian is also found. If end labels cannot be used
*     (eg if a pole is in the image, or if north is at a position angle
*     of 90 degrees), then latitude labels are placed on the longest
*     meridian.

*  Arguments:
*     ACEN = DOUBLE PRECISION (Given)
*        The longitude of the meridian with index zero.
*     BCEN = DOUBLE PRECISION (Given)
*        The latitude of the parallel with index zero.
*     GAPLON = DOUBLE PRECISION (Given)
*        The longitude gap between meridians.
*     GAPLAT = DOUBLE PRECISION (Given)
*        The latitude gap between parallels.
*     SCS = CHARTACTER (Given)
*        The sky coordinate system, An abbreviation will do. If no
*        equinox is supplied, a default is used.
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     IMLO = INTEGER (Given)
*        The lowest meridian index.
*     IMHI = INTEGER (Given)
*        The highest meridian index.
*     IPLO = INTEGER (Given)
*        The lowest parallel index.
*     IPHI = INTEGER (Given)
*        The highest parallel index.
*     LBND( 2 ) = REAL (Given)
*        Lower world coordinate bounds for each axis defining the
*        section of the current SGS zone in which the curve is to be
*        drawn.
*     UBND( 2 ) = REAL (Given)
*        Upper world coordinate bounds for each axis defining the
*        section of the current SGS zone in which the curve is to be
*        drawn.
*     MAXLAB = INTEGER (Given)
*        The size of the LABS array.
*     MAXTIC = INTEGER (Given)
*        The size of the TICKS array.
*     ALAX = DOUBLE PRECISION (Given and Returned)
*        The longitude of the longest meridian.
*     LABS( MAXLAB, 5 ) = DOUBLE PRECISION (Returned)
*        Information about "end" labels. Element (I,1) contains the
*        longitude value for the I'th label, (I,2) contains the X
*        coordinate at which the label is to be put, (I,3) contains the
*        Y coordinate at which the label is to be put, (I,4) contains
*        the X component of the unit vector along the meridian at the
*        position of the label, (I,5) contains the Y component.
*     NLABS = INTEGER (Returned)
*        The number of labels for which information is stored in LABS.
*     TICKS( MAXTIC, 4 ) = DOUBLE PRECISION (Returned)
*        Information about tick marks. Element (I,1) contains the
*        contains the X coordinate at which the I'th tick should start,
*        (I,2) contains the Y coordinate, (I,3) contains the X
*        component of the unit vector along the meridian at the
*        position of the tick, (I,4) contains the Y component.
*     NTICKS = INTEGER (Returned)
*        The number of tick marks for which information is stored in
*        TICKS.
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
      INCLUDE 'IRA_PAR'          ! IRA_ constants

*  Arguments Given:
      DOUBLE PRECISION ACEN
      DOUBLE PRECISION BCEN
      DOUBLE PRECISION GAPLON
      DOUBLE PRECISION GAPLAT
      CHARACTER SCS*(*)
      INTEGER IDA
      INTEGER IMLO
      INTEGER IMHI
      INTEGER IPLO
      INTEGER IPHI
      REAL LBND( 2 )
      REAL UBND( 2 )
      INTEGER MAXLAB
      INTEGER MAXTIC

*  Arguments Given and Returned:
      DOUBLE PRECISION ALAX

*  Arguments Returned:
      DOUBLE PRECISION LABS( MAXLAB, 5 )
      INTEGER NLABS
      DOUBLE PRECISION TICKS( MAXTIC, 4 )
      INTEGER NTICKS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION A
      REAL             BREAK( 2, IRA__MXBRK )
      DOUBLE PRECISION CINC
      LOGICAL          CLOSED
      DOUBLE PRECISION CSTART
      INTEGER          I
      INTEGER          IINC
      INTEGER          IHI
      INTEGER          ILO
      INTEGER          INCNT
      INTEGER          J
      REAL             LENGTH
      REAL             MAXLEN
      INTEGER          NBREAK
      LOGICAL          OUT
      REAL             VBREAK( 2, IRA__MXBRK )
      REAL             ZCLEN
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the starting latitude and length of each meridian.
      CSTART = MAX( -IRA__PIBY2, MIN( IRA__PIBY2,
     :                             BCEN + ( IPLO - 1 )*GAPLAT ) )
      CINC = ( IPHI - IPLO + 2 )*GAPLAT

*  Ensure that the meridian does not extend through a pole.
      CINC = MAX( -IRA__PIBY2, MIN( IRA__PIBY2, CINC + CSTART ) )
     :       - CSTART

*  Initialise the count of meridians which intersected the plotting
*  zone.
      INCNT = 0

*  Initialise the maximum length of all meridian with two or less
*  breaks.
      MAXLEN = 0.0

*  Initialise the length of the zero longitude meridian to zero.
      ZCLEN = 0.0

*  "End" labels are labels placed at the start or end of a meridian or
*  parallel. If possible, end labels for meridians (i.e. longitude
*  values) are placed on the bottom edge of the plotting zone.
*  Initialise the number of meridians which can be labelled using end
*  labels.
      NLABS = 0

*  Tick marks are not drawn by this routine, because the decision as to
*  where to put the tick marks cannot be decided until information about
*  all the meridians is available. Therefore, the position and direction
*  at the start and end of each meridian are stored for future use as
*  potential tick marks. Initialise the number of ticks to zero.
      NTICKS = 0

*  Indicate that no meridians are closed curves.
      CLOSED = .FALSE.

*  Draw each meridian in turn. The meridians are drawn in two batches,
*  each batch starts from the centre of the field and works out to the
*  edge.  Each batch is terminated as soon as a meridian is found which
*  does not intersect the plotting zone. Set up do loop limits and
*  increment for the meridians with zero or positive indices.
      ILO = 0
      IHI = IMHI
      IINC = 1

*  Loop round the two batches of meridians.
      DO J = 1, 2
         OUT = .FALSE.

*  Loop round all the meridian indices in the current batch.
         DO I = ILO, IHI, IINC

*  If the previous meridian lay entirely outside the plotting zone,
*  don't waste time by plotting another.
            IF( .NOT. OUT ) THEN

*  Set up the longitude of the meridian and draw it.
               A = ACEN + I*GAPLON
               CALL IRA_DRMER( IDA, A, CSTART, CINC, SCS, LBND, UBND,
     :                         STATUS )
               CALL SGS_FLUSH

*  Get information about the breaks in the curve just drawn.
               CALL IRA_DRBRK( IRA__MXBRK, OUT, BREAK, VBREAK, NBREAK,
     :                         LENGTH, STATUS )

*  Abort if an error has occurred.
               IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Count the meridians which intersected the plotting zone.
               IF( .NOT. OUT ) THEN
                  INCNT = INCNT + 1

*  If possible save the position of the first and last break in the
*  plotted meridian as potential positions for a longitude "end" label.
                  CALL IRA1_POTL( 1, A, LBND( 1 ), LBND( 2 ), UBND( 1 ),
     :                            UBND( 2 ), IRA__MXBRK, BREAK, VBREAK,
     :                            NBREAK, MAXLAB, LABS, NLABS, STATUS )

*  If possible save the position of the first and last break in the
*  plotted meridian as potential positions for tick marks.
                  CALL IRA1_TPOT( 1, LBND( 1 ), LBND( 2 ), UBND( 1 ),
     :                            UBND( 2 ), IRA__MXBRK, BREAK, VBREAK,
     :                            NBREAK, MAXTIC, TICKS, NTICKS,
     :                            STATUS )

*  Save the longitude of the longest meridian which had two or less
*  breaks. If "end" labels cannot be used (eg if a pole is in the
*  image), then latitude labels are placed along the longest meridian.
                  IF( NBREAK .LE. 2 ) THEN

                     IF( LENGTH .GT. MAXLEN ) THEN
                        MAXLEN = LENGTH
                        ALAX= A
                     END IF

*  If the longitude of the current meridian is zero, remember the length
*  of the meridian.
                     IF( ABS( A ) .LT. 1.0E-6 ) ZCLEN = LENGTH

*  Remember if any meridian has no breaks.
                     IF( NBREAK .EQ. 0 ) CLOSED = .TRUE.

                  END IF

               ENDIF

            END IF

         END DO

*  Go round to draw the meridians with negative indices.
         ILO = -1
         IHI = IMLO
         IINC = -1

      END DO

*  If the meridian at zero longitude was only slightly shorter than the
*  longest meridian, label the zero longitude meridian if "end" labels
*  cannot be produced.
      IF( ZCLEN .GT. 0.7*MAXLEN ) ALAX = 0.0D0

*  If any of the meridians had no breaks, "end" labels cannot be used.
      IF( CLOSED ) THEN
         NLABS = 0

*  Otherwise, determine if the stored candidate "end" label positions
*  can be used.
      ELSE
         CALL IRA1_POTU( LBND( 1 ), LBND( 2 ), UBND( 1 ), UBND( 2 ),
     :                   INCNT, MAXLAB, LABS, NLABS, STATUS )
      END IF

 999  CONTINUE

      END
