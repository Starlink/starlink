      SUBROUTINE IRM_DIMEN( PARAM, DEFLT, LIMIT, DIMENS, STATUS )
*+
*  Name:
*     IRM_DIMEN

*  Purpose:
*     Obtain a pair of dimensions using the ADAM parameter system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_DIMEN( PARAM, DEFLT, LIMIT, DIMENS, STATUS )

*  Description:
*     This routine can be used to get pixel dimensions, image
*     dimensions, etc. An attempt is made to obtain an array of two
*     values for the supplied ADAM parameter. These are interpreted as
*     dimension sizes in arc-minutes. If only one value is obtained, the
*     same value is used for both dimensions.  If either dimension
*     is less than the value of LIMIT, it is set equal to LIMIT and the
*     user is warned of this.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The ADAM parameter to associate with the dimensions.
*     DEFLT = LOGICAL (Given)
*        If true, the values of DIMENS on entry are supplied to the
*        user as run-time defaults.
*     LIMIT = REAL (Given)
*        The smallest acceptable dimension, in radians.
*     DIMENS( 2 ) = REAL (Given and Returned)
*        If DEFLT is true, then on entry DIMENS contains the values to
*        use as the run-time default for the given ADAM parameter (in
*        radians). On exit, DIMENS contains the required dimensions,
*        in radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-NOV-1991 (DSB):
*        Original version.
*     22-APR-1992 (WG):
*        Pass it through FORCHECK and fix found bugs.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard SAE constants
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Arguments Given:
      CHARACTER PARAM*(*)
      LOGICAL DEFLT
      REAL LIMIT

*  Arguments Returned:
      REAL DIMENS( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL    DEFVAL( 2 )        ! The default parameter values (in
                                 ! arc-mins).
      INTEGER NVAL               ! The number of values obtained from
                                 ! the environment.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If required, set up the default dimensions.
      IF( DEFLT) THEN
         DEFVAL( 1 ) = REAL( IRA__RTOD ) * DIMENS( 1 ) * 60.0
         DEFVAL( 2 ) = REAL( IRA__RTOD ) * DIMENS( 2 ) * 60.0
         CALL PAR_DEF1R( PARAM, 2, DEFVAL, STATUS )
      END IF

*  Get new values from the environment.
      CALL PAR_GET1R( PARAM, 2, DIMENS, NVAL, STATUS )

*  If no error occurred...
      IF( STATUS .EQ. SAI__OK ) THEN

*  Convert the supplied dimensions from arcminutes to radians.
         DIMENS( 1 ) = REAL( IRA__DTOR ) * DIMENS( 1 ) / 60.0
         DIMENS( 2 ) = REAL( IRA__DTOR ) * DIMENS( 2 ) / 60.0

*  If only one dimension was supplied, use the same value for the second
*  dimension.
         IF( NVAL .EQ. 1 ) DIMENS( 2 ) = DIMENS( 1 )

*  Limit dimensions to be greater than or equal to the value of
*  argument LIMIT.
         IF( DIMENS( 1 ) .LT. LIMIT .OR. DIMENS( 2 ) .LT. LIMIT ) THEN
            DIMENS( 1 ) = MAX( DIMENS( 1 ), LIMIT )
            DIMENS( 2 ) = MAX( DIMENS( 2 ), LIMIT )

*  If either of the supplied dimensions was too small, warn the user
*  that a larger size will be used.
            CALL MSG_BLANK( STATUS )
            CALL MSG_SETC( 'P',  PARAM )
            CALL MSG_OUT( 'IRM_DIMEN_MSG1',
     :  'WARNING - Dimensions supplied for parameter ^P are too small.',
     :                     STATUS )

            CALL MSG_SETR( 'D1', REAL( IRA__RTOD )*DIMENS( 1 )*60.0 )
            CALL MSG_SETR( 'D2', REAL( IRA__RTOD )*DIMENS( 2 )*60.0 )
            CALL MSG_OUT( 'IRM_DIMEN_MSG2',
     :                ' Using dimensions ^D1 x ^D2 arc-mins', STATUS )

            CALL MSG_BLANK( STATUS )
         END IF

      END IF

      END
