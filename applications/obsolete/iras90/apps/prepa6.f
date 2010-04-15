      SUBROUTINE PREPA6( INDF, INDF1, INDF2, STATUS )
*+
*  Name:
*     PREPA6

*  Purpose:
*     Extract two 2D sections from a 3D CPC NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPA6( INDF, INDF1, INDF2, STATUS )

*  Description:
*     CPC FITS images are three dimensional. They consist of two planes;
*     the lower plane holds 50um data and the upper plane hold 100um
*     data. This routine returns NDF identifiers for sections of the
*     input NDF  each consisting of just one of these two planes. The
*     returned NDF sections are two dimensional.

*  Arguments:
*     INDF = INTEGER (Given)
*        The identifier for the 3D input NDF.
*     INDF1 = INTEGER (Returned)
*        The identifier for the section holding the 50um data.
*     INDF2 = INTEGER (Returned)
*        The identifier for the section holding the 100um data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      INTEGER INDF1
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LBND(3)            ! NDF lower bounds.
      INTEGER LBND3              ! Lower bound of third dimension of the
                                 ! input NDF.
      INTEGER NDIM               ! Number of dimensions in input NDF.
      INTEGER SHIFT(3)           ! Shift to apply in each dimension.
      INTEGER UBND(3)            ! NDF upper bounds.
      INTEGER UBND3              ! Upper bound of third dimension of the
                                 ! input NDF.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the bounds of the input NDF.
      CALL NDF_BOUND( INDF, 3, LBND, UBND, NDIM, STATUS )

*  If the input NDF does not have two planes, report an error.
      LBND3 = LBND( 3 )
      UBND3 = UBND( 3 )

      IF( UBND3 - LBND3 .NE. 1 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'LO', LBND3 )
         CALL MSG_SETI( 'HI', UBND3 )
         CALL ERR_REP( 'PREPA6_ERR1',
     : 'PREPA6: Input CPC NDF has bad bounds [^LO,^HI] for the third '//
     : 'dimension ', STATUS )
         GO TO 999
      END IF

*  Create a section holding the lower plane. This is the 50um data.
      UBND( 3 ) = LBND3
      CALL NDF_SECT( INDF, 3, LBND, UBND, INDF1, STATUS )

*  If necessary, shift the NDF in the third dimension, so that the 50
*  um plane has an index of 1.
      IF( LBND3 .NE. 1 ) THEN
         SHIFT( 1 ) = 0
         SHIFT( 2 ) = 0
         SHIFT( 3 ) = 1 - LBND3
         CALL NDF_SHIFT( 3, SHIFT, INDF1, STATUS )
      END IF

*  Reduce the NDF section to 2 dimensions.
      CALL NDF_SBND( 2, LBND, UBND, INDF1, STATUS )

*  Create a section holding the upper plane. This is the 100um data.
      LBND( 3 ) = UBND3
      UBND( 3 ) = UBND3
      CALL NDF_SECT( INDF, 3, LBND, UBND, INDF2, STATUS )

*  If necessary, shift the NDF in the third dimension, so that the 100
*  um plane has an index of 1.
      IF( UBND3 .NE. 1 ) THEN
         SHIFT( 1 ) = 0
         SHIFT( 2 ) = 0
         SHIFT( 3 ) = 1 - UBND3
         CALL NDF_SHIFT( 3, SHIFT, INDF2, STATUS )
      END IF

*  Reduce the NDF section to 2 dimensions.
      CALL NDF_SBND( 2, LBND, UBND, INDF2, STATUS )

*  Finish
 999  CONTINUE

      END
