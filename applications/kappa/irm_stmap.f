      SUBROUTINE IRM_STMAP( XORDER, YORDER, STATUS )
*+
*  Name:
*     IRM_STMAP

*  Purpose:
*     Set the ways of mapping coordinates to axes of NCAR display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_STMAP( XORDER, YORDER, STATUS )

*  Description:
*     This routine set the way of mapping x coordinates to the
*     horizontal axis of the grid window, and the way of mapping y
*     coordinates to the vertical axis of the grid window.  

*  Arguments:
*     XORDER = INTEGER (Given)
*        When it is 0, the values of user x coordinates mapped to the
*        horixontal axis of the grid window should increase from left to
*        right. When it is 1, the values of user x coordinates mapped to
*        the horixontal axis of the grid window should decrease from
*        left to right.
*     YORDER = INTEGER (Given)
*        When it is 0, the values of user y coordinates mapped to the
*        vertical axis of the grid window should increase from bottom to
*        top. When it is 0, the values of user y coordinates mapped to
*        the vertical axis of the grid window should decrease from
*        bottom to top.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     27-FEB-1991 (WG):
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
      INTEGER XORDER
      INTEGER YORDER

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If X coordinates is mapped increasing from left to right,
      IF ( XORDER .EQ. 0 ) THEN
         CALL AGSETF( 'X/OR.', 0. )

*  If X coordinates is mapped decreasing from left to right,
      ELSE IF ( XORDER .EQ. 1 ) THEN
         CALL AGSETF( 'X/OR.', 1. )
      END IF

*  If Y coordinates is mapped increasing from bottom to top,
      IF ( YORDER .EQ. 0 ) THEN
         CALL AGSETF( 'Y/OR.', 0. )

*  If Y coordinates is mapped decreasing from bottom to top,
      ELSE IF ( YORDER .EQ. 1 ) THEN
         CALL AGSETF( 'Y/OR.', 1. )
      END IF

      END
