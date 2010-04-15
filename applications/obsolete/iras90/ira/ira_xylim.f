      SUBROUTINE IRA_XYLIM( IDA, ACEN, BCEN, XSIZE, YSIZE, LBND, UBND,
     :                      STATUS )
*+
*  Name:
*     IRA_XYLIM

*  Purpose:
*     Find image coordinate bounds which encloses a given area of sky.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_XYLIM( IDA, ACEN, BCEN, XSIZE, YSIZE, LBND, UBND,
*                     STATUS )

*  Description:
*     A box in image coordinates is specified by giving the sky
*     coordinates corresponding to the centre of the box, and the
*     arc-length of each dimension of the box, parallel to the image X
*     and Y axes. The bounds of this box in image coordinates are
*     returned. The effects of varying pixelk size are taken into
*     account. Note, if the box extends beyond the edge of the
*     projection (as may happen, for instance, if the box centre is
*     placed close to the elliptical boundary of an Aitoff projection)
*     then the box is truncated at the edge of the projection, but no
*     error is reported. In this case, the returned bounds will not
*     represent a box of the requested dimensions, but will be smaller.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     ACEN = DOUBLE PRECISION (Given)
*        The longitude at the centre of the box.
*     BCEN = DOUBLE PRECISION (Given)
*        The latitude at the centre of the box.
*     XSIZE = DOUBLE PRECISION (Given)
*        The arc-length of the image X axis, in radians.
*     YSIZE = DOUBLE PRECISION (Given)
*        The arc-length of the image Y axis, in radians.
*     LBND( 2 ) = DOUBLE PRECISION (Returned)
*        The lower bounds of the X and Y image axes, in image
*        coordinates.
*     UBND( 2 ) = DOUBLE PRECISION (Returned)
*        The upper bounds of the X and Y image axes, in image
*        coordinates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-MAY-1991 (DSB):
*        Original version.
*     12-FEB-1993 (DSB):
*        Changed to accept IDA as input rather than PROJ, P, NP etc.
*     24-FEB-1993 (DSB):
*        Arguments ACEN and BCEN added.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! BAD data values.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA error values.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common values.
*        ACM_PROJN( IRA__MAX ) = CHARACTER (Read)
*           Full projection name from the associated AS.
*        ACM_PROJP( IRA__MAXP, IRA__MAX ) = DOUBLE PRECISION (Read)
*           Projection parameter values from the associated AS.

*  Arguments Given:
      INTEGER IDA
      DOUBLE PRECISION ACEN
      DOUBLE PRECISION BCEN
      DOUBLE PRECISION XSIZE
      DOUBLE PRECISION YSIZE

*  Arguments Returned:
      DOUBLE PRECISION LBND( 2 )
      DOUBLE PRECISION UBND( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION
     :        MARGIN,            ! Width of safety margin added to
                                 ! image.
     :        XXSIZE,            ! XSIZE in range 0 to 2*PI
     :        YYSIZE             ! YSIZE in range 0 to 2*PI

      INTEGER
     :        NP                 ! Number of projection parameters.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )

*  Check that input arguments are not BAD.
      IF( XSIZE .EQ. VAL__BADD .OR. YSIZE .EQ. VAL__BADD .AND.
     :    STATUS .EQ. SAI__OK ) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRA_XYLIM_ERR1',
     :                 'IRA_XYLIM: BAD image size supplied',
     :                  STATUS )
      END IF

      IF( ACEN .EQ. VAL__BADD .OR. BCEN .EQ. VAL__BADD .AND.
     :    STATUS .EQ. SAI__OK ) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRA_XYLIM_ERR2',
     :                 'IRA_XYLIM: BAD box centre supplied',
     :                  STATUS )
      END IF

*  Ensure that XSIZE and YSIZE are in the range zero to 2*PI.
      XXSIZE = MIN( ABS( XSIZE ), 2.0D0*IRA__PI )
      YYSIZE = MIN( ABS( YSIZE ), 2.0D0*IRA__PI )

*  Get the number of projection parameters.
      CALL IRA1_CHPRJ( ACM_PROJN( IDA ), ACM_PROJN( IDA ), NP, STATUS )

*  Find the upper pixel bound on the X axis.
      CALL IRA1_LIMIT( ACM_PROJN( IDA ), NP, ACM_PROJP( 1, IDA ),
     :                 ACEN, BCEN, .TRUE., 1, XXSIZE*0.5,
     :                 0.5*ACM_PROJP( 5, IDA ), 1.0D0, UBND(1), STATUS )

*  Find the lower pixel bound on the X axis.
      CALL IRA1_LIMIT( ACM_PROJN( IDA ), NP, ACM_PROJP( 1, IDA ),
     :                 ACEN, BCEN, .TRUE., -1, XXSIZE*0.5,
     :                 0.5*ACM_PROJP( 5, IDA ), 1.0D0, LBND(1), STATUS )

*  Find the upper pixel bound on the Y axis.
      CALL IRA1_LIMIT( ACM_PROJN( IDA ), NP, ACM_PROJP( 1, IDA ),
     :                 ACEN, BCEN, .FALSE., 1, YYSIZE*0.5,
     :                 0.5*ACM_PROJP( 6, IDA), 1.0D0, UBND(2), STATUS )

*  Find the lower pixel bound on the Y axis.
      CALL IRA1_LIMIT( ACM_PROJN( IDA ), NP, ACM_PROJP( 1, IDA ),
     :                 ACEN, BCEN, .FALSE., -1, YYSIZE*0.5,
     :                 0.5*ACM_PROJP( 6, IDA ), 1.0D0, LBND(2), STATUS )

*  Make the image 1% bigger for safety.
      MARGIN = 0.005*( UBND(1) - LBND(1) + 1.0 )
      UBND(1) = UBND(1) + MARGIN
      LBND(1) = LBND(1) - MARGIN

      MARGIN = 0.005*( UBND(2) - LBND(2) + 1.0 )
      UBND(2) = UBND(2) + MARGIN
      LBND(2) = LBND(2) - MARGIN

*  If an error occurred, give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_XYLIM_ERR3',
     :              'IRA_XYLIM: Unable to determine image pixel bounds',
     :                 STATUS )
      END IF

      END
