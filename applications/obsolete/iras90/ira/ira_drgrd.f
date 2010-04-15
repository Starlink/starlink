      SUBROUTINE IRA_DRGRD( IDA, SCS, LBND, UBND, STATUS )
*+
*  Name:
*     IRA_DRGRD

*  Purpose:
*     Draw a sky coordinate grid.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_DRGRD( IDA, SCS, LBND, UBND, STATUS )

*  Description:
*     This routine draws a complete sky coordinate grid.  The grid is
*     drawn over a section of the current SGS zone, specified by UBND
*     and LBND. It is assumed that the world coordinate system
*     associated with the current zone correspond to image (or pixel)
*     coordinates. A boundary line is drawn around the region
*     containing valid sky coordinate data.
*
*     If the graphics option LINES (see routine IRA_DROPT) is set to a
*     negative value, then lines of constant latitude and longitude are
*     drawn across the plot. Otherwise, tick marks are used to indicate
*     longitude and latitude.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     SCS = CHARACTER * ( * ) (Given)
*        The name of the sky coordinate system to display. Any unambiguous
*        abbreviation will do. This need not be the same as the sky
*        coordinate system stored in the astrometry structure
*        identified by IDA. See ID/2 section "Sky Coordinates" for more
*        information. A blank value will cause the system associated
*        with IDA to be used.
*     LBND( 2 ) = REAL (Given)
*        Lower world coordinate bounds for each axis defining the
*        section of the current SGS zone covered by the grid. Labels
*        may be placed outside this region.
*     UBND( 2 ) = REAL (Given)
*        Upper world coordinate bounds for each axis defining the
*        section of the current SGS zone covered by the grid. Labels
*        may be placed outside this region.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is effected by the TOLERANCE, TEXT_SIZE,
*     COORD_SIZE, LONG_GAP, LAT_GAP, LINES, LAT_ACC, LONG_ACC, PEN1,
*     PEN2, PEN3 and PEN4 options set up by routine IRA_DROPT.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-MAR-1992 (DSB):
*        Original version.
*     17-MAR-1992 (DSB):
*        Arguments GAPLAT and GAPLON added.
*     9-NOV-1992 (DSB):
*        Introduced use of graphics options.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'IRA_ERR'          ! IRA_ error constants

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_DROPT( IRA__NOPT ) = DOUBLE PRECISION (Read)
*           Graphics options parameters.
*        ACM_SCS( IRA__MAX ) = CHARACTER (Read)
*           The sky coordinate system used by the projection.

*  Arguments Given:
      INTEGER IDA
      CHARACTER SCS*(*)
      REAL LBND( 2 )
      REAL UBND( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  External Routines:
      EXTERNAL IRA1_INIT         ! Initialise graphics options in common

*  Local Constants:
      INTEGER MAXLAB             ! Max. no. of labels on each edge.
      INTEGER MAXTIC             ! Max. no. of longitude or latitude ticks.
      REAL TICKLE                ! Normalised tick length.

      PARAMETER ( MAXLAB = 30 )
      PARAMETER ( MAXTIC = 60 )
      PARAMETER ( TICKLE = 0.17 )

*  Local Variables:
      CHARACTER        ABBREV*(IRA__SZSCD)! Coordinate abbreviation.
      DOUBLE PRECISION ACCLAT
      DOUBLE PRECISION ACCLON
      DOUBLE PRECISION ACEN      ! Central longitude in sparse grid.
      DOUBLE PRECISION ALABS( MAXLAB, 5 )
      DOUBLE PRECISION ALAX
      REAL             AR
      DOUBLE PRECISION ATICS( MAXTIC, 4 )
      DOUBLE PRECISION BCEN      ! Central latitude in sparse grid.
      DOUBLE PRECISION BLABS( MAXLAB, 5 )
      DOUBLE PRECISION BLAX      ! Central latitude in sparse grid.
      DOUBLE PRECISION BTICS( MAXTIC, 4 )
      LOGICAL          COLAB     ! True if coordinate labels are required.
      CHARACTER        DESCR*(IRA__SZSCD)! Coordinate description.
      REAL             HEIGHT    ! Text height in pixels.
      REAL             HT
      INTEGER          IMHI
      INTEGER          IMLO
      INTEGER          IPHI
      INTEGER          IPLO
      INTEGER          LA        ! Used characters in ABBREV.
      LOGICAL          LABS
      REAL             LAXMIN    ! Minimum X coordinates covered by
                                 ! coordinate labels.
      INTEGER          LD        ! Used characters in DESCR.
      DOUBLE PRECISION LGPLAT    ! Gap in latitude between parallels.
      DOUBLE PRECISION LGPLON    ! Gap in longitude between meridians.
      LOGICAL          LINES     ! True if lines (not ticks) are
                                 ! required.
      CHARACTER LSCS*(IRA__SZSCS)! Local copy of SCS.
      INTEGER          NALABS
      INTEGER          NATICS
      INTEGER          NBLABS
      INTEGER          NBTICS
      INTEGER          NF
      INTEGER          NPEN      ! SGS pen on entry.
      INTEGER          NPR
      REAL             SIZE      ! Max size of zone in pixels.
      REAL             SP
      LOGICAL          TICKS
      CHARACTER        TXJ*2
      LOGICAL          TXTLAB    ! True if axis text labels are required.
      REAL             X1
      REAL             X2
      REAL             XM
      REAL             XU
      REAL             Y1
      REAL             Y2
      REAL             YM
      REAL             YU
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Check that the LBND and UBND arguments are OK.
      IF( LBND( 1 ) .GE. UBND( 1 ) .OR.
     :    LBND( 2 ) .GE. UBND( 2 ) ) THEN
         STATUS = IRA__ORDER
         CALL MSG_SETR( 'U1', UBND( 1 ) )
         CALL MSG_SETR( 'U2', UBND( 2 ) )
         CALL MSG_SETR( 'L1', LBND( 1 ) )
         CALL MSG_SETR( 'L2', LBND( 2 ) )
         CALL ERR_REP( 'IRA_DRGRD_ERR1',
     :'IRA_DRGRD: Viewport upper bounds (^U1,^U2) are inconsistent '//
     :'with lower bounds (^L1,^L2).', STATUS )
         GO TO 999
      END IF

*  Set up a flag indicating if text labels are required.
      IF( ACM_DROPT( 1 ) .LE. 0.0D0 ) THEN
         TXTLAB = .FALSE.
      ELSE
         TXTLAB = .TRUE.
      END IF

*  Set up a flag indicating if coordinate labels are required.
      IF( ACM_DROPT( 2 ) .LE. 0.0D0 ) THEN
         COLAB = .FALSE.
      ELSE
         COLAB = .TRUE.
      END IF

*  Set up a flag indicating if lines or ticks are required.
      IF( ACM_DROPT( 4 ) .LE. 0.0D0 ) THEN
         LINES = .FALSE.
      ELSE
         LINES = .TRUE.
      END IF

*  If a blank SCS was given, use the value associated with IDA.
      IF( SCS .EQ. ' ' ) THEN
         LSCS = ACM_SCS( IDA )
      ELSE
         LSCS = SCS
      END IF

*  Save the maximum dimension of the data zone.
      SIZE = MAX( UBND( 1 ) - LBND( 1 ), UBND( 2 ) - LBND( 2 ) )

*  Save the current text attributes, and SGS pen.
      CALL SGS_ITXA( NF, NPR, HT, AR, XU, YU, SP, TXJ )
      CALL SGS_IPEN( NPEN )

*  Determine the longitudes and latitudes of the meridians and
*  parallels to be displayed.
      LGPLAT = ACM_DROPT( 6 )
      LGPLON = ACM_DROPT( 5 )
      CALL IRA1_LLEX( IDA, LSCS, LBND( 1 ), LBND( 2 ), UBND( 1 ),
     :                UBND( 2 ), LGPLON, LGPLAT, ACEN, BCEN, IMLO,
     :                IMHI, IPLO, IPHI, STATUS )

*  Set up the SGS pen no. for drawing curves (option PEN2).
      CALL SGS_SPEN( NINT( ACM_DROPT( 8 ) ) )

*  Draw the meridians and find where "end" labels and tick marks could
*  go. These are longitude labels and ticks marks which are drawn
*  around the edge of the sky grid, at the end of each meridian.
      ALAX = ACEN
      CALL IRA1_DRMS( ACEN, BCEN, LGPLON, LGPLAT, LSCS, IDA, IMLO, IMHI,
     :                IPLO, IPHI, LBND, UBND, MAXLAB, MAXTIC, ALAX,
     :                ALABS, NALABS, ATICS, NATICS, STATUS )

*  Draw the parallels and find where "end" labels and tick marks should
*  go. These are latitude labels and ticks marks which are drawn around
*  the edge of the sky grid, at the end of each parallel.
      BLAX = BCEN
      CALL IRA1_DRPS( ACEN, BCEN, LGPLON, LGPLAT, LSCS, IDA, IMLO, IMHI,
     :                IPLO, IPHI, LBND, UBND, MAXLAB, MAXTIC, BLAX,
     :                BLABS, NBLABS, BTICS, NBTICS, STATUS )

*  If no usable information about labels has been stored, ignore any
*  stored tick mark information.
      IF( NALABS .EQ. 0 ) NATICS = 0
      IF( NBLABS .EQ. 0 ) NBTICS = 0

*  If solid lines have not been produced, tick marks are needed.
      IF( .NOT. LINES ) THEN

*  If possible, produce ticks marks around the edge of the sky grid
*  using the information stored by IRA1_DRMS and IRA1_DRPS.
         IF( NATICS .GT. 0 ) CALL IRA1_EGTK( NATICS, MAXTIC, ATICS,
     :                                       0.1*TICKLE*SIZE, STATUS )
         IF( NBTICS .GT. 0 ) CALL IRA1_EGTK( NBTICS, MAXTIC, BTICS,
     :                                       0.1*TICKLE*SIZE, STATUS )

      END IF

*  Initialise the minimum X coordinate covered by coordinate labels to
*  be just to the left of the left hand edge of the data area.
      LAXMIN = 1.03*LBND( 1 ) - 0.03*UBND( 1 )

*  If coordinate labels are required...
      IF( COLAB ) THEN

*  Now set text height for the coordinate value labels.
         HEIGHT = ACM_DROPT( 2 )*SIZE
         CALL SGS_SHTX( HEIGHT )

*  Determine the default accuracies for the displayed values.
         CALL IRA1_LACC( LSCS, LGPLAT, LGPLON, ACCLAT, ACCLON, STATUS )

*  Replace these with any values specified by the LONG_ACC and LAT_ACC
*  options.
         IF( ACM_DROPT( 9 ) .GT. 0.0D0 ) ACCLAT = ACM_DROPT( 9 )
         IF( ACM_DROPT( 10 ) .GT. 0.0D0 ) ACCLON = ACM_DROPT( 10 )

*  If possible put longitude and latitude labels around the edge of the
*  grid, using information stored by IRA1_DRMS and IRA1_DRPS.
         IF( NALABS .GT. 0 ) CALL IRA1_EGLB( 1, LSCS, NALABS, MAXLAB,
     :                             ALABS, ACCLON, LBND, LAXMIN, STATUS )

         IF( NBLABS .GT. 0 ) CALL IRA1_EGLB( 2, LSCS, NBLABS, MAXLAB,
     :                             BLABS, ACCLAT, LBND, LAXMIN, STATUS )

      END IF

*  If the stored longitude information cannot be used, label and/or
*  tick a central parallel.
      TICKS = NATICS .EQ. 0 .AND. .NOT. LINES
      LABS = NALABS .EQ. 0 .AND. COLAB
      IF( TICKS .OR. LABS ) THEN

*  If visible ticks are required, set the LINES option to ensure plotted
*  curves are visisble.
         IF( TICKS ) ACM_DROPT( 4 ) = 1.0D0

*  Draw the lables and/or ticks.
         CALL IRA1_AXLM( IDA, LSCS, ALAX, BLAX, ACEN, LGPLON, IMHI,
     :                   IMLO, LABS, DBLE( 1.2*TICKLE*LGPLAT ), LBND,
     :                   UBND, ACCLON, LAXMIN, STATUS )

*  If LINES were originally suppressed, reset the LINES option.
         IF( .NOT. LINES ) ACM_DROPT( 4 ) = -1.0D0

      END IF

*  If the stored latitude information cannot be used, label and/or
*  tick a central meridian.
      TICKS = NBTICS .EQ. 0 .AND. .NOT. LINES
      LABS = NBLABS .EQ. 0 .AND. COLAB
      IF( TICKS .OR. LABS ) THEN
         IF( TICKS ) ACM_DROPT( 4 ) = 1.0D0
         CALL IRA1_AXLP( IDA, LSCS, ALAX, BLAX, BCEN, LGPLAT, IPHI,
     :                   IPLO, LABS, DBLE( 1.2*TICKLE*LGPLAT ), LBND,
     :                   UBND, ACCLAT, LAXMIN, STATUS )
         IF( .NOT. LINES ) ACM_DROPT( 4 ) = -1.0D0
      END IF

*  If text labels are required...
      IF( TXTLAB ) THEN

*  ...set text height, justification and pen for the axis descriptions.
         HEIGHT = ACM_DROPT( 1 )*SIZE
         CALL SGS_SHTX( HEIGHT )
         CALL SGS_STXJ( 'CC' )
         CALL SGS_SPEN( NINT( ACM_DROPT( 11 ) ) )

*  Produce axis descriptions.
         CALL IRA_SCNAM( LSCS, 1, DESCR, LD, ABBREV, LA, STATUS )
         CALL SGS_TX( 0.5*( LBND( 1 ) + UBND( 1 ) ),
     :                LBND( 2 ) - 4.0*HEIGHT, DESCR( : LD ) )

         CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )
         CALL SGS_SUPTX( -1.0, 0.0 )
         CALL IRA_SCNAM( LSCS, 2, DESCR, LD, ABBREV, LA, STATUS )
         CALL SGS_TX( MAX( LAXMIN - 0.5*HEIGHT, MIN( X1, X2 ) ),
     :                0.5*( LBND( 2 ) + UBND( 2 ) ), DESCR( : LD ) )
         CALL SGS_SUPTX( XU, YU )

      END IF

*  Draw the boundary.
      CALL IRA_DRBND( IDA, LBND, UBND, STATUS )

*  Reinstate the original text attributes and pen.
      CALL SGS_SHTX( HT )
      CALL SGS_STXJ( TXJ )
      CALL SGS_SPEN( NPEN )

*  If an error has occurred, give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_DRGRD_ERR2',
     :               'IRA_DRGRD: Error drawing a sky coordinate grid.',
     :                 STATUS )
      END IF

      END
