      SUBROUTINE DREXAM2( STATUS )
*+
*  Name:
*     DREXAM2

*  Purpose:
*     Produce plot file for DRGRD example 2 in ID2.TEX.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DREXAM2( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Calls IRA_DRGRD to produce a grid showing an aitoff
*     projection in galactic coordinates, covering all the
*     sky. Device must be CANON_PT or PSCRIPT_PTEX in order to
*     produce files for inclusion in TEX documents.

*  ADAM Parameters:
*     DEVICE = DEVICE (Write)
*        The graphics device name.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAR-1992 (DSB):
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
      INCLUDE 'NDF_PAR'          ! NDF_ constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL              AR       ! Aspect ration of image.
      REAL              BORDER	 ! Size of border.
      REAL              FXHI     ! Upper X limit in frame zone.
      REAL              FXLO     ! Lower X limit in frame zone.
      REAL              FYHI     ! Upper Y limit in frame zone.
      REAL              FYLO     ! Lower Y limit in frame zone.
      INTEGER 		IDA      ! IRA identifier.
      INTEGER           IZ       ! SGS identifier for default zone.
      INTEGER           IZDATA   ! SGS identifier for data zone.
      INTEGER           IZFRAM   ! SGS identifier for frame zone.
      INTEGER           IZTEMP   ! SGS identifier for temporary zone.
      DOUBLE PRECISION  LBND(2)  ! Lower pixel bound on each axis.
      DOUBLE PRECISION  P(8)     ! Projection parameters.
      CHARACTER 	PROJ*(IRA__SZPRJ)! Projection name.
      REAL              PXSIZE   ! Zone X size, in metres.
      REAL              PYSIZE   ! Zone Y size, in metres.
      CHARACTER 	SCS*(IRA__SZSCS)! Selected SCS for projection.
      REAL              SLBND( 2 )! Single precision version of LBND.
      REAL              SUBND( 2 )! Single precision version of UBND.
      DOUBLE PRECISION  UBND(2)  ! Upper pixel bound on each axis.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise IRA.
      CALL IRA_INIT( STATUS )

*  Set up projection parameters which puts the reference point at the
*  image centre, with image coordinates (0,0).
      P( 1 ) = 0.0D0
      P( 2 ) = 0.0D0
      P( 3 ) = 0.0D0
      P( 4 ) = 0.0D0
      P( 5 ) = IRA__DTOR
      P( 6 ) = IRA__DTOR
      P( 7 ) = 0.0D0
      P( 8 ) = 0.0D0

*  Store the projection and sky coordinate system.
      PROJ = 'AIT'
      SCS = 'GAL'

*  Create a temporary astrometry structure.
      CALL IRA_CREAT( PROJ, 8, P, SCS, 1991D0, NDF__NOID, IDA, STATUS )

*  Find the bounds of the image using these projection parameters.
      CALL IRA_XYLIM( IDA, P(1), P(2), 360.0*IRA__DTOR, 360.0*IRA__DTOR,
     :                LBND, UBND, STATUS )

*  Convert the image coordinates to single precision.
      SLBND( 1 ) = REAL( LBND( 1 ) )
      SUBND( 1 ) = REAL( UBND( 1 ) )
      SLBND( 2 ) = REAL( LBND( 2 ) )
      SUBND( 2 ) = REAL( UBND( 2 ) )

*  Open the device name for plotting.
      CALL SGS_ASSOC( 'DEVICE', 'WRITE', IZ, STATUS )

*  Create a zone 15cm by 12cm to match the gap left for the picture in
*  the TEX document.
      CALL SGS_ZSIZE( 0.15, 0.12, 'TL', IZFRAM, STATUS )

*  Get the extent of the current zone, which will contain the data zone
*  surrounded by a border for annotation. This zone is called the frame
*  zone.
      CALL SGS_IZONE( FXLO, FXHI, FYLO, FYHI, PXSIZE, PYSIZE )

*  Determine the size of the border between the edge of the data zone
*  and the edge of the frame zone. This border will contain labels,
*  etc.
      BORDER = 0.075*SQRT( PXSIZE**2 + PYSIZE**2 )

*  Create a smaller zone within the frame zone.
      CALL SGS_ZSIZE( PXSIZE - 2*BORDER, PYSIZE - 2*BORDER, 'CC',
     :                IZTEMP, STATUS )

*  Create the largest zone which has the same shape as the image and
*  will fit within the zone identified by IZTEMP. The new zone is the
*  data zone, in which the sky grid will be drawn.
      AR = ( SUBND( 1 ) - SLBND( 1 ) )/( SUBND( 2 ) - SLBND( 2 ) )
      CALL SGS_ZSHAP( AR, 'CC', IZDATA, STATUS )

*  Establish image coordinates as world coordinates within the data
*  zone, so that the required part of the image just fills the zone.
      CALL SGS_SW( SLBND( 1 ), SUBND( 1 ), SLBND( 2 ), SUBND( 2 ),
     :             STATUS )

*  Find the extent of the frame zone within the world coordinate system
*  of the data zone (i.e. in image coordinates).
      CALL SGS_TPZ( IZFRAM, FXLO, FYLO, IZDATA, FXLO, FYLO, STATUS )
      CALL SGS_TPZ( IZFRAM, FXHI, FYHI, IZDATA, FXHI, FYHI, STATUS )

*  Select the frame zone and release the other zones.
      CALL SGS_SELZ( IZFRAM, STATUS )
      CALL SGS_RELZ( IZDATA )
      CALL SGS_RELZ( IZTEMP )

*  Set the world coordinates for the frame zone to match those of the
*  data zone. This means that a single point is represented by the same
*  coordinates (image coordinates) in both zones.
      CALL SGS_SW( FXLO, FXHI, FYLO, FYHI, STATUS )

*  Draw the grid.
      CALL IRA_DROPT( 'LINES', 1.0D0, STATUS )
      CALL IRA_DROPT( 'TOL', 0.0D0, STATUS )
      CALL IRA_DRGRD( IDA, SCS, SLBND, SUBND, STATUS )

*  Close down SGS.
      CALL SGS_ANNUL( IZ, STATUS )

*  Close IRA.
      CALL IRA_CLOSE( STATUS )

      END
