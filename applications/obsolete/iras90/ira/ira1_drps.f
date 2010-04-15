      SUBROUTINE IRA1_DRPS( ACEN, BCEN, GAPLON, GAPLAT, SCS, IDA, IMLO,
     :                      IMHI, IPLO, IPHI, LBND, UBND,
     :                      MAXLAB, MAXTIC, BLAX, LABS, NLABS, TICKS,
     :                      NTICKS, STATUS )
*+
*  Name:
*     IRA1_DRPS

*  Purpose:
*     Draw a set of parallels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_DRPS( ACEN, BCEN, GAPLON, GAPLAT, SCS, IDA, IMLO, IMHI,
*                     IPLO, IPHI, LBND, UBND, MAXLAB,
*                     MAXTIC, BLAX, LABS, NLABS, TICKS, NTICKS, STATUS )

*  Description:
*     This routine draws each of the parallels in turn.
*     Positions and directions of latitude tick marks are
*     returned, together with information about the positions of
*     latitude "end" labels (i.e. latitude labels placed at the start
*     or end of each parallel). The longest parallel is also found. If
*     end labels cannot be used (eg if a pole is in the image, or if
*     north is at a position angle of 90 degrees), then longitude labels
*     are placed on the longest parallel.

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
*     BLAX = DOUBLE PRECISION (Given and Returned)
*        The latitude of the longest parallel. Left unchanged if no
*        parallel had 2 or less breaks.
*     LABS( MAXLAB, 5 ) = DOUBLE PRECISION (Returned)
*        Information about "end" labels. Element (I,1) contains the
*        latitude value for the I'th label, (I,2) contains the X
*        coordinate at which the label is to be put, (I,3) contains the
*        Y coordinate at which the label is to be put, (I,4) contains
*        the X component of the unit vector along the parallel at the
*        position of the label, (I,5) contains the Y component.
*     NLABS = INTEGER (Returned)
*        The number of labels for which information is stored in LABS.
*     TICKS( MAXTIC, 4 ) = DOUBLE PRECISION (Returned)
*        Information about tick marks. Element (I,1) contains the
*        contains the X coordinate at which the I'th tick should start,
*        (I,2) contains the Y coordinate, (I,3) contains the X
*        component of the unit vector along the parallel at the
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
      DOUBLE PRECISION BLAX

*  Arguments Returned:
      DOUBLE PRECISION LABS( MAXLAB, 5 )
      INTEGER NLABS
      DOUBLE PRECISION TICKS( MAXTIC, 4 )
      INTEGER NTICKS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION B
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

*  Set up the starting longitude and length of each parallel.
      CSTART = ACEN + ( IMLO - 1 )*GAPLON
      CINC = ( IMHI - IMLO + 2 )*GAPLON

*  Initialise the count of parallels which intersected the plotting
*  zone.
      INCNT = 0

*  Initialise the maximum length of all parallel with two or less
*  breaks.
      MAXLEN = 0.0

*  Initialise the length of the zero latitude parallel to zero.
      ZCLEN = 0.0

*  "End" labels are labels placed at the start or end of a meridian or
*  parallel. If possible, end labels for parallels (i.e. latitude
*  values) are placed on the left edge of the plotting zone.
*  Initialise the number of parallels which can be labelled using end
*  labels.
      NLABS = 0

*  Tick marks are not drawn by this routine, because the decision as to
*  where to put the tick marks cannot be decided until information about
*  all the parallels is available. Therefore, the position and direction
*  at the start and end of each parallel are stored for future use as
*  potential tick marks. Initialise the number of ticks to zero.
      NTICKS = 0

*  Indicate that no parallels are closed curves.
      CLOSED = .FALSE.

*  Draw each parallel in turn. The parallels are drawn in two batches,
*  each batch starts from the centre of the field and works out to the
*  edge.  Each batch is terminated as soon as a parallel is found which
*  does not intersect the plotting zone. Set up do loop limits and
*  increment for the parallels with zero or positive indices.
      ILO = 0
      IHI = IPHI
      IINC = 1

*  Loop round the two batches of parallels.
      DO J = 1, 2
         OUT = .FALSE.

*  Loop round all the parallel indices in the current batch.
         DO I = ILO, IHI, IINC

*  If the previous parallel lay entirely outside the plotting zone,
*  don't waste time by plotting another.
            IF( .NOT. OUT ) THEN

*  Set up the latitude of the parallel and draw it.
               B = BCEN + I*GAPLAT
               CALL IRA_DRPAR( IDA, CSTART, B, CINC, SCS, LBND, UBND,
     :                         STATUS )
               CALL SGS_FLUSH

*  Get information about the breaks in the curve just drawn.
               CALL IRA_DRBRK( IRA__MXBRK, OUT, BREAK, VBREAK, NBREAK,
     :                         LENGTH, STATUS )

*  Abort if an error has occurred.
               IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Count the parallels which intersected the plotting zone.
               IF( .NOT. OUT ) THEN
                  INCNT = INCNT + 1

*  If possible save the position of the first and last break in the
*  plotted parallel as potential positions for a latitude "end" label.
                  CALL IRA1_POTL( 2, B, LBND( 1 ), LBND( 2 ), UBND( 1 ),
     :                            UBND( 2 ), IRA__MXBRK, BREAK, VBREAK,
     :                            NBREAK, MAXLAB, LABS, NLABS, STATUS )

*  If possible save the position of the first and last break in the
*  plotted parallel as potential positions for tick marks.
                  CALL IRA1_TPOT( 2, LBND( 1 ), LBND( 2 ), UBND( 1 ),
     :                            UBND( 2 ), IRA__MXBRK, BREAK, VBREAK,
     :                            NBREAK, MAXTIC, TICKS, NTICKS,
     :                            STATUS )

*  Save the latitude of the longest parallel which had two or less
*  breaks. If "end" labels cannot be used (eg if a pole is in the
*  image), then longitude labels are placed along the longest parallel.
                  IF( NBREAK .LE. 2 ) THEN

                     IF( LENGTH .GT. MAXLEN ) THEN
                        MAXLEN = LENGTH
                        BLAX = B
                     END IF

*  If the latitude of the current parallel is zero, remember the length
*  of the parallel.
                     IF( ABS( B ) .LT. 1.0E-6 ) ZCLEN = LENGTH

*  Remember if any parallel has no breaks.
                     IF( NBREAK .EQ. 0 ) CLOSED = .TRUE.

                  END IF

               ENDIF

            END IF

         END DO

*  Go round to draw the parallels with negative indices.
         ILO = -1
         IHI = IPLO
         IINC = -1

      END DO

*  If the parallel of zero latitude was only slightly shorter than the
*  longest parallel, label the zero latitude parallel if "end" labels
*  cannot be produced.
      IF( ZCLEN .GT. 0.7*MAXLEN ) BLAX = 0.0

*  If any of the parallels had no breaks, "end" labels cannot be used.
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
