      SUBROUTINE PICTRANS( STATUS )
*+
*  Name:
*     PICTRANS

*  Purpose:
*     Transforms co-ordinates between the current and BASE pictures.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PICTRANS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts a position's co-ordinates from the
*     current picture to their equivalent in the BASE picture, or
*     vice versa.

*  Usage:
*     pictrans inxy [outx] [outy] [bound]

*  ADAM Parameters:
*     BOUND = _LOGICAL (Write)
*        BOUND is TRUE when the transformed co-ordinates lie within the
*        bounds of their associated picture.  
*     COSYS = LITERAL (Read)
*        The co-ordinate system to be used.  This can be either "World"
*        or "Data".  "World" makes the application convert between the
*        world co-ordinates of the position in the two pictures.  World
*        co-ordinates that relate to a location in a data array will be
*        in array pixels.  For COSYS = "Data" the conversion is between
*        data co-ordinates, stored via a transformation.  The BASE
*        picture will not normally have data co-ordinates, so the value
*        of COSYS usually selects in which co-ordinate system positions
*        in the current picture are specified.
*
*        Data co-ordinates are arbitrary but most often they will be a
*        linear or logarithmic transformation of the world
*        co-ordinates.  For example, the x co-ordinate of a spectrum
*        would be given in pixels if COSYS = "World", but if COSYS =
*        "Data" the x co-ordinate could be in wavelength units, such as
*        Angstroms.  If the database does not have a world-to-data
*        transformation for a given picture, the value of this
*        parameter is ignored for that picture, and supplied or
*        computed positions in that picture will be in world
*        co-ordinates.  [Current co-ordinate system]
*     DEVICE = DEVICE (Read)
*        The graphics workstation. [The current graphics device]
*     INXY( 2 ) = _DOUBLE (Read)
*        The x-y co-ordinates to be transformed.  These need not lie
*        within the physical bounds of their associated picture.  The
*        suggested value is the current value.
*     OUTX = _DOUBLE (Write)
*        The transformed x co-ordinate.
*     OUTY = _DOUBLE (Write)
*        The transformed y co-ordinate.
*     TOBASE = _LOGICAL (Read)
*        This decides the direction of the transformation.  If TOBASE
*        is TRUE, the conversion is from the current to the BASE
*        picture.  If TOBASE is FALSE, BASE-picture co-ordinates are
*        converted to a position within the current picture.
*        The suggested value is the current value.  [TRUE]

*  Examples:
*     pictrans [100.3,-20.1] outx=(dx) outy=(dy) cosys=w
*        This converts the position (100.3,-20.1), in world co-ordinates
*        of the current picture of the current graphics device, to the
*        world co-ordinates of that point in the BASE picture.  The base
*        co-ordinates are written to ICL variables DX and DY (as well as
*        the application's parameter file).
*     pictrans [-1.e4,2.56] outy=(dy) device=xwindows
*        This converts the position (-10000.0,2.56), in the current
*        picture of the xwindows device, to the co-ordinates of that
*        point in the BASE picture.  All positions use the current
*        co-ordinate system.  The base y co-ordinate is written to ICL
*        variable DY.
*     pictrans [0.314,0.137] (dx) (dy) (within) cosys=d notobase 
*        This converts the position (0.314,0.137), in the data
*        co-ordinates of the BASE picture of the current graphics
*        device, to the data co-ordinates of that point in the current
*        picture.  The transformed co-ordinates are written to ICL
*        variables DX and DY.  ICL variable WITHIN contains a flag
*        to indicate whether or not the point lies within the current
*        picture.

*  Implementation Deficiencies:
*     -  Does use an array for the output co-ordinates.  (This is
*     because ICL does not support arrays yet.)
*     [routine_deficiencies]...

*  Related Applications:
*     KAPPA: GDSTATE, PICIN, PICXY.

*  [optional_A_task_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 August 19 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Number of dimensions in a picture
      PARAMETER ( NDIM = 2 )

*  Local Variables:
      LOGICAL BOUND              ! True if the output co-ordinate lies
                                 ! within the output picture's bounds
      CHARACTER * ( 6 ) COSYS    ! Co-ordinate system
      LOGICAL DEVCAN             ! True if device parameter is to be
                                 ! cancelled
      DOUBLE PRECISION INXY( NDIM ) ! Input co-ordinates
      DOUBLE PRECISION OUTXY( NDIM ) ! Output co-ordinates
      INTEGER PICID              ! Current (input) picture identifier
      INTEGER PICIDB             ! BASE picture identifier
      LOGICAL TOBASE             ! True when the conversion is from the
                                 ! current to the BASE picture
      LOGICAL WORLD              ! True when the co-ordinate system is
                                 ! world
      DOUBLE PRECISION WX        ! Input picture's x world co-ordinate
      REAL WXO                   ! Output picture's x world co-ordinate
      DOUBLE PRECISION WY        ! Input picture's y world co-ordinate
      REAL WYO                   ! Output picture's y world co-ordinate
      REAL XL                    ! X lower bound world co-ordinate of
                                 ! current picture
      REAL XLB                   ! X lower bound world co-ordinate of
                                 ! BASE picture
      REAL XU                    ! X upper bound world co-ordinate of
                                 ! current picture
      REAL XUB                   ! X upper bound world co-ordinate of
                                 ! current picture
      REAL YL                    ! Y lower bound world co-ordinate of
                                 ! current picture
      REAL YLB                   ! Y lower bound world co-ordinate of
                                 ! BASE picture
      REAL YU                    ! Y upper bound world co-ordinate of
                                 ! current picture
      REAL YUB                   ! Y upper bound world co-ordinate of
                                 ! current picture
      INTEGER ZONID              ! SGS zone identifier of the current
                                 ! picture
      INTEGER ZONIDB             ! SGS zone identifier of the base
                                 ! picture
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Assume that the device name is not to be cancelled.
      DEVCAN = .FALSE.

*  Get the input parameters.
*  =========================

*  Get the type of co-ordinates to convert.
      CALL PAR_CHOIC( 'COSYS', 'Data', 'Data,World', .FALSE., COSYS,
     :                STATUS )
      WORLD = COSYS .EQ. 'WORLD'

*  Determine the sense of the conversion.
      CALL PAR_GET0L( 'TOBASE', TOBASE, STATUS )

*  Get the input co-ordinates.
      CALL PAR_EXACD( 'INXY', NDIM, INXY, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Start the graphics system.
*  ==========================

*  Get the graphics device, and open SGS.  Get an SGS zone.
      CALL AGS_ASSOC( 'DEVICE', 'READ', ' ', PICID, ZONID, STATUS )

*  Obtain the limits of the world co-ordinates in the current picture.
      CALL AGI_IWOCO( XL, XU, YL, YU, STATUS )

*  Get the BASE picture.
*  =====================

*  Inquire the BASE picture for the current workstation.
      CALL AGI_IBASE( PICIDB, STATUS )

*  Select this as the current picture.
      CALL AGI_SELP( PICIDB, STATUS )

*  Create a new SGS zone from the BASE picture.
      CALL AGS_NZONE( ZONIDB, STATUS )

*  Obtain the limits of the world co-ordinates in the BASE picture.
      CALL AGI_IWOCO( XLB, XUB, YLB, YUB, STATUS )

*  Restore the current picture and zone.
      CALL AGI_SELP( PICID, STATUS )
      CALL SGS_SELZ( ZONID, STATUS )

*  If there was a problem with the graphics device (e.g. not available),
*  flag not to cancel the device, and leave the application.
      IF ( STATUS .NE. SAI__OK ) THEN
         DEVCAN = .TRUE.
         GOTO 980
      END IF

*  Perform a conversion from the current to the BASE picture.
*  ==========================================================
      IF ( TOBASE ) THEN

*  Obtain the world co-ordinates of the input position.
         IF ( WORLD ) THEN
            WX = INXY( 1 )
            WY = INXY( 2 )

*  Convert the data co-ordinates in the current picture to world
*  co-ordinates.
         ELSE
            CALL AGI_TDDTW( -1, 1, INXY( 1 ), INXY( 2 ), WX, WY,
     :                      STATUS )
         END IF

*  Transform world co-ordinates from the current to the BASE picture.
         CALL SGS_TPZ( ZONID, REAL( WX ), REAL( WY ), ZONIDB, WXO, WYO,
     :                 STATUS )

*  Check whether or not the output point lies within the bounds of the
*  BASE picture.  This might not strictly be correct at the boundaries
*  due to the type conversion.
         BOUND = WXO .GE. XLB .AND. WXO .LE. XUB .AND.
     :           WYO .GE. YLB .AND. WYO .LE. YUB

*  Obtain the data co-ordinates of the output position.
         IF ( WORLD ) THEN
            OUTXY( 1 ) = DBLE( WXO )
            OUTXY( 2 ) = DBLE( WYO )

*  Convert the output world co-ordinates in the BASE picture to data
*  co-ordinates.
         ELSE
            CALL AGI_TWTDD( PICIDB, 1, DBLE( WXO ), DBLE( WYO ),
     :                      OUTXY( 1 ), OUTXY( 2 ), STATUS )
         END IF

*  Perform a conversion from the base to the current picture.
*  ==========================================================
      ELSE

*  Obtain the world co-ordinates of the input position.
         IF ( WORLD ) THEN
            WX = INXY( 1 )
            WY = INXY( 2 )

*  Convert the data co-ordinates in the BASE picture to world
*  co-ordinates.
         ELSE
            CALL AGI_TDDTW( PICIDB, 1, INXY( 1 ), INXY( 2 ), WX, WY,
     :                      STATUS )
         END IF

*  Transform world co-ordinates from the base to the current picture.
         CALL SGS_TPZ( ZONIDB, REAL( WX ), REAL( WY ), ZONID, WXO, WYO,
     :                 STATUS )

*  Check whether or not the output point lies within the bounds of the
*  current picture.  This might not strictly be correct at the
*  boundaries due to the type conversion.
         BOUND = WXO .GE. XL .AND. WXO .LE. XU .AND.
     :           WYO .GE. YL .AND. WYO .LE. YU

*  Obtain the data co-ordinates of the output position.
         IF ( WORLD ) THEN
            OUTXY( 1 ) = DBLE( WXO )
            OUTXY( 2 ) = DBLE( WYO )

*  Convert the output world co-ordinates in the current picture to data
*  co-ordinates.
         ELSE
            CALL AGI_TWTDD( -1, 1, DBLE( WXO ), DBLE( WYO ), OUTXY( 1 ),
     :                      OUTXY( 2 ), STATUS )
         END IF

      END IF

*  Write the results to output parameters.
*  =======================================

*  Output the co-ordinates.  In time these should be merged into an
*  array, hence the use of an array variable.
      CALL PAR_PUT0D( 'OUTX', OUTXY( 1 ), STATUS )
      CALL PAR_PUT0D( 'OUTY', OUTXY( 2 ), STATUS )

*  Output the bound check.
      CALL PAR_PUT0L( 'BOUND', BOUND, STATUS )

*  AGI closedown sequence.
*  =======================

  980 CONTINUE

*  Deactivate SGS and close the workstation.
      CALL AGS_DEASS( 'DEVICE', DEVCAN, STATUS )

*  Come here for any errors that occurred before the graphics device
*  was opened.

  999 CONTINUE

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PICTRANS_ERR',
     :     'PICTRANS: Unable to convert co-ordinates between the '/
     :     /'current and BASE pictures.', STATUS )
      END IF

      END
