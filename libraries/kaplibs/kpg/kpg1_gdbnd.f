      SUBROUTINE KPG1_GDBND( PNLOW, PNUPP, LBND, UBND, STATUS )
*+
*  Name:
*     KPG1_GDBND

*  Purpose:
*     Gets the bounds of a new PGPLOT window from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GDBND( PNLOW, PNUPP, LBND, UBND, STATUS )

*  Description:
*     This routine defines a two-dimensional region in the current
*     PGPLOT window, getting the bounds of the region from the
*     environment.

*  Arguments:
*     PNLOW = CHARACTER * ( * ) (Given)
*         Parameter name of lower-bound co-ordinates that defines a
*         region.
*     PNUPP = CHARACTER * ( * ) (Given)
*         Parameter name of lower-bound co-ordinates that defines a
*         region.
*     LBND( 2 ) = REAL (Returned)
*         Co-ordinates of the lower bound that defines a region.
*     UBND( 2 ) = REAL (Returned)
*         Co-ordinates of the upper bound that defines a region.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-AUG-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors

*  Arguments Given:
      CHARACTER PNLOW*(*)
      CHARACTER PNUPP*(*)

*  Arguments Returned:
      REAL LBND( 2 )
      REAL UBND( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL REPEAT          ! Continue?
      REAL LIML( 2 )          ! Minimum permitted lower bound
      REAL LIMU( 2 )          ! Maximum permitted upper bound
      REAL XCEN, YCEN         ! Centre of the current window
      REAL XLOW, YLOW         ! Lower bound of the current window
      REAL XHIGH, YHIGH       ! Upper bound of the current window
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get bounds of window.
      CALL PGQWIN( XLOW, XHIGH, YLOW, YHIGH )
      XCEN = 0.5 * ( XLOW + XHIGH )
      YCEN = 0.5 * ( YLOW + YHIGH )

*  Set defaults to current region. Report the co-ordinates of the current
*  window.
      CALL MSG_SETR( 'X1', XLOW )
      CALL MSG_SETR( 'Y1', YLOW )
      CALL MSG_SETR( 'X2', XHIGH )
      CALL MSG_SETR( 'Y2', YHIGH )
      CALL MSG_OUT( 'KPG1_GDBND_1', 'Bounds are ( ^X1, ^Y1 ) and '//
     :              '( ^X2, ^Y2 )', STATUS )

      REPEAT = .TRUE.
      DO WHILE ( REPEAT )

*  Store the maximum and minimum values permitted for passing to the PAR
*  routine.  Also use these as the suggested defaults.
         LIML( 1 ) = XLOW
         LIML( 2 ) = YLOW
         LIMU( 1 ) = XHIGH
         LIMU( 2 ) = YHIGH

*  Obtain the lower bound of the region.
         CALL PAR_GRM1R( PNLOW, 2, LIML, LIML, LIMU, .FALSE., LBND,
     :                   STATUS )

*  Revise minimum values permitted for the upper bound of the region.
         LIML( 1 ) = LBND( 1 )
         LIML( 2 ) = LBND( 2 )

*  Obtain the upper bound of the region.
         CALL PAR_GRM1R( PNUPP, 2, LIMU, LIML, LIMU, .FALSE., UBND,
     :                   STATUS )

         IF ( STATUS .EQ. PAR__ABORT ) THEN
            REPEAT = .FALSE.

*  If the points are not distinct, then report the error.
         ELSE IF ( ( LBND( 1 ) .EQ. UBND( 1 ) ) .AND.
     :             ( LBND( 2 ) .EQ. UBND( 2 ) ) ) THEN

*  Start new error context.
            CALL ERR_MARK

*  Flush the error so the user can try again.
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_GDBND_DPR', 'Two distinct points are '//
     :                    'required.', STATUS )
            CALL ERR_FLUSH( STATUS )

*  Close the error context.
            CALL ERR_RLSE

*  Success --- end loop
         ELSE
            REPEAT = .FALSE.

         END IF

*  Tidy up so that subroutine may be called again
         CALL PAR_CANCL( PNLOW, STATUS )
         CALL PAR_CANCL( PNUPP, STATUS )

      END DO

      END
