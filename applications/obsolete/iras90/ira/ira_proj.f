      SUBROUTINE IRA_PROJ( NVAL, IN1, IN2, FORWRD, PROJ, NP, P,
     :                        OUT1, OUT2, STATUS )
*+
*  Name:
*     IRA_PROJ

*  Purpose:
*     Apply a specified projection to supplied coordinate data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_PROJ( NVAL, IN1, IN2, FORWRD, PROJ, NP, P, OUT1, OUT2,
*                    STATUS )

*  Description:
*     Coordinate data are transformed from sky coordinates to image
*     coordinates, or vice-versa using the requested projection, with
*     the supplied projection parameter values. The available
*     projections and their parameters are described in routine
*     IRA_CREAT, and in the appendix "Projection Equations" of ID2.
*     All sky coordinates (whether given or returned) are assumed to be
*     in the same sky coordinate system as the reference point
*     specified by the values held in P(1) and P(2).

*  Arguments:
*     NVAL = INTEGER (Given)
*        The number of coordinate points to be transformed.
*     IN1( NVAL ) = DOUBLE PRECISION (Given)
*        If FORWRD is true, then IN1 holds values of the first image
*        coordinate (X), otherwise IN1 holds values of the sky
*        longitude.
*     IN2( NVAL ) = DOUBLE PRECISION (Given)
*        If FORWRD is true, then IN2 holds values of the second image
*        coordinate (Y), otherwise IN2 holds values of the sky
*        latitude.
*     FORWRD = LOGICAL (Given)
*        If true then the forward mapping of the projection is used
*        from image coordinate to sky coordinate. Otherwise, the
*        inverse mapping from sky coordinate to image coordinates is
*        used.
*     PROJ = CHARACTER * ( * ) (Given)
*        The name of the projection to use. Any unambiguous abbreviation
*        will do.
*     NP = INTEGER (Given)
*        The size of the array P.
*     P( NP ) = DOUBLE PRECISION (Given)
*        The values to use for the projection parameters.
*     OUT1( NVAL ) = DOUBLE PRECISION (Returned)
*        If FORWRD is true, then OUT1 holds values of the sky longitude
*        corresponding to the image coordinates given in arrays IN1 and
*        IN2. Otherwise, OUT1 holds values of the first image
*        coordinate (X) corresponding to the input sky coordinates. If
*        either the corresponding IN1 or IN2 value is equal to the
*        Starlink "BAD" value (VAL__BADD), then OUT1 is set bad.
*     OUT2( NVAL ) = DOUBLE PRECISION (Returned)
*        If FORWRD is true, then OUT2 holds values of the sky latitude
*        corresponding to the image coordinates given in arrays IN1 and
*        IN2. Otherwise, OUT2 holds values of the second image
*        coordinate corresponding to the input sky coordinates. If
*        either the corresponding IN1 or IN2 value is equal to the
*        Starlink "BAD" value (VAL__BADD), then OUT2 is set bad.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*

*  History:
*     21-JAN-1991 (DSB):
*        Original version.
*     23-APR-1991 (DSB):
*        Include Orthographic projection
*     11-SEP-1992 (DSB):
*        P(8) added.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA error values.

*  Arguments Given:
      INTEGER          NVAL
      DOUBLE PRECISION IN1( NVAL )
      DOUBLE PRECISION IN2( NVAL )
      LOGICAL          FORWRD
      CHARACTER        PROJ*(*)
      INTEGER          NP
      DOUBLE PRECISION P( NP )

*  Arguments Returned:
      DOUBLE PRECISION OUT1( NVAL )
      DOUBLE PRECISION OUT2( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER   IP               ! Parameter loop count.
      INTEGER   NPREQ            ! Required no. of parameters for
                                 ! specified projection.
      CHARACTER TPROJ*(IRA__SZPRJ) ! Full projection name.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Identify the requested projection.
      CALL IRA1_CHPRJ( PROJ, TPROJ, NPREQ, STATUS )

*  Check the right number of parameters have been supplied.
      IF( STATUS .EQ. SAI__OK .AND. NP .NE. NPREQ ) THEN
         STATUS = IRA__TFEWP
         CALL MSG_SETI( 'NP', NP )
         CALL MSG_SETI( 'NPR', NPREQ )
         CALL MSG_SETC( 'PROJ', TPROJ )
         CALL ERR_REP( 'IRA_PROJ_ERR1', 'IRA_PROJ: ^PROJ projection '//
     :           'requires ^NPR parameters; ^NP supplied', STATUS )
      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Check that non of the supplied parameter values is bad.
      DO IP = 1, NP
         IF( P( IP ) .EQ. VAL__BADD ) THEN
            STATUS = IRA__BADPA
            CALL ERR_REP( 'IRA_PROJ_ERR2',
     :              'IRA_PROJ: BAD projection parameter value supplied',
     :                    STATUS )
            GO TO 999
         END IF
      END DO

*  Call IRA1_IPRJ to do the work.
      CALL IRA1_IPRJ( NVAL, IN1, IN2, FORWRD, TPROJ, NP, P, OUT1, OUT2,
     :                STATUS )

*  If an error occurred, give a context message.
 999  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_PROJ_ERR3',
     :                 'IRA_PROJ: Unable to project coordinate values',
     :                 STATUS )
      END IF

      END
