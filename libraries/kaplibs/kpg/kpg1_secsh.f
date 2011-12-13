      SUBROUTINE KPG1_SECSH( NDF, MXDIM, STATUS )
*+
*  Name:
*     KPG1_SECSH

*  Purpose:
*     Shifts bounds of upper dimensions that have one element to index
*     one.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SECSH( NDF, MXDIM, STATUS )

*  Description:
*     This routine shifts the origin to the default of one of each
*     dimension of an NDF greater than a nominated dimension and whose
*     size is one.
*
*     The routine is required in applications that require NDFs of a
*     specific dimensionality and which also call NDF_SECT.  This is
*     because a user-defined section may be from an NDF of higher
*     dimensionality, and a subsequent call to NDF_SECT can result in
*     an array of bad data, when the bounds of higher dimensions are
*     not one.  It is intended to be used alongside KPG1_SGDIM.

*  Arguments:
*     NDF = INTEGER (Given)
*        The NDF identifier.
*     MXDIM = INTEGER (Given)
*        The dimension above which the shifts are to be made.  This
*        should be the last significant bound for the required
*        dimensionality of NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 March 31 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER NDF
      INTEGER MXDIM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER NDIM               ! Total number of dimensions in the NDF
      INTEGER SHIFT( NDF__MXDIM ) ! The shifts to be NDF bounds
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Valid the dimension.
      IF ( MXDIM .LT. 1 .AND. MXDIM .GT. NDF__MXDIM ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'N', MXDIM )
         CALL ERR_REP( 'KPG1_SECSH_BADDIM',
     :     'The dimensionality (^N) of the NDF is not valid. (Probable'/
     :     /'programming error.)', STATUS )
         GOTO 999
      END IF

*  Obtain the bounds.
      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Set the shifts when necessary.  No shifts are applied to the
*  significant dimensions.
         IF ( NDIM .GT. MXDIM ) THEN
            DO  I = 1, MXDIM
               SHIFT( I ) = 0
            END DO
            DO  I = MXDIM + 1, NDIM
               SHIFT( I ) = 1 - LBND( I )
            END DO

*  Apply the shifts to the NDF bounds.
            CALL NDF_SHIFT( NDIM, SHIFT, NDF, STATUS )
         END IF
      END IF

  999 CONTINUE

      END
