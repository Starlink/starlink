      SUBROUTINE IRM_STLOG( XLOG, YLOG, STATUS )
*+
*  Name:
*     IRM_STLOG

*  Purpose:
*     Set axes to logarithmic where required

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_STLOG( XLOG, YLOG, STATUS )

*  Description:
*     This routine sets the axes to logarithmic where required.  

*  Arguments:
*     XLOG = LOGICAL (Given)
*        If it is true, the x axis will be set to logarithmic,
*        otherwise, to linear.
*     YLOG = LOGICAL (Given)
*        If it is true, the y axis will be set to logarithmic,
*        otherwise, to linear.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     14-JAN-1991 (WG):
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
      LOGICAL XLOG
      LOGICAL YLOG

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If x axis is required to set to logarithmic,
      IF ( XLOG ) THEN
         CALL AGSETF( 'X/LOGARITHMIC.', -1. )

*  Otherwise, set x axis to linear.
      ELSE
         CALL AGSETF( 'X/LOGARITHMIC.', 0. )
      END IF

*  If y axis is required to set to logarithmic,
      IF ( YLOG ) THEN
         CALL AGSETF( 'Y/LOGARITHMIC.', -1. )

*  Otherwise, set y axis to linear.
      ELSE
         CALL AGSETF( 'Y/LOGARITHMIC.', 0. )
      END IF

      END
