      SUBROUTINE KPS1_MLPMP( NAX, FRM, MAP, NPOS, IN, FORWRD, IAXIS, 
     :                       OUT, WORK, STATUS )
*+
*  Name:
*     KPS1_MLPMP

*  Purpose:
*     Maps a set of 1D positions, normalizing the returned values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MLPMP( NAX, FRM, MAP, NPOS, IN, FORWRD, IAXIS, OUT, WORK,
*                      STATUS )

*  Description:
*     This routine maps the supplied 1D positions using the specified
*     Mapping. This mapping maps the 1D positions into the supplied
*     nD Frame. These positions are then normalized using the AST_NORM
*     method for the supplied Frame, and the resulting values for a
*     requested axis are returned.

*  Arguments:
*     NAX = INTEGER (Given)
*        The number of axes in FRM.
*     FRM = INTEGER (Given)
*        The AST Frame decribing the positions produced by the transformation
*        specified by MAP and FORWRD.
*     MAP = INTEGER (Given)
*        The AST Mapping.
*     NPOS = INTEGER (Given)
*        No. of positions to map.
*     IN( NPOS ) = DOUBLE PRECISION (Given)
*        The input 1D positions.
*     FORWRD = LOGICAL (Given)
*        Use the forward transformation of MAP?
*     IAXIS = INTEGER (Given)
*        The index of the required output axis within FRM.
*     OUT( NPOS ) = DOUBLE PRECISION (Returned)
*        The required normalized output axis values.
*     WORK( NPOS, NAX ) = DOUBLE PRECISION (Returned)
*        Work space. 
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER NAX
      INTEGER FRM
      INTEGER MAP
      INTEGER NPOS
      DOUBLE PRECISION IN( NPOS )
      LOGICAL FORWRD
      INTEGER IAXIS

*  Arguments Returned:
      DOUBLE PRECISION OUT( NPOS )
      DOUBLE PRECISION WORK( NPOS, NAX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION POS( NDF__MXDIM )! Single position within FRM
      INTEGER I                  ! Position index
      INTEGER J                  ! Axis index
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Transform the supplied 1D positions into the supplied nD Frame.
      CALL AST_TRANN( MAP, NPOS, 1, NPOS, IN, FORWRD, NAX, NPOS, 
     :                WORK, STATUS ) 

*  Check each position.
      DO I = 1, NPOS

*  Copy the nD position into a vector.
         DO J = 1, NAX
            POS( J ) = WORK( I, J )
         END DO

*  Normalise the position.
         CALL AST_NORM( FRM, POS, STATUS ) 

*  Extract the required axis.
         OUT( I ) = POS( IAXIS )

      END DO

      END
