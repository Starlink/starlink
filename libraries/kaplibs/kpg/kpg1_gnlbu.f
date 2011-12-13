      SUBROUTINE KPG1_GNLBU( NDF, PNLAB, COMP, AXSLAB, STATUS )
*+
*  Name:
*     KPG1_GNLBU

*  Purpose:
*     Obtains an axis annotation for NDF data or variance.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GNLBU( NDF, PNLAB, COMP, AXSLAB, STATUS )

*  Description:
*     This routine obtains an axis annotation from the parameter system.
*     The suggested default has the form "label (units)" when the NDF
*     has both a label and units, or "label" if there is a label but
*     not units.  When neither component is present the default is the
*     component name follwed by " values".  The units are squared for
*     the variance component.  If a bad status, other than abort, is
*     returned by the parameter system when getting the value, the
*     error is annulled and the output annotation is the suggested
*     default value.

*  Arguments:
*     NDF = INTEGER (Given)
*        The NDF identifier.
*     PNLAB = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter from which the annotation will
*        be obtained.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component: 'DATA', 'QUALITY', 'VARIANCE',
*        or 'ERROR', though it is used literally and not checked to
*        be a member of this set.
*     AXSLAB = CHARACTER * ( * ) (Returned)
*        The annotation obtained from the parameter system.  The dynamic
*        default is limited to 128 characters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     1991 February 24 (MJC):
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
      INTEGER NDF

      CHARACTER*( * ) PNLAB
      CHARACTER*( * ) COMP

*  Arguments Returned:
      CHARACTER*( * ) AXSLAB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*( 128 ) AXSDEF   ! Limitation by parameter system
                                 ! Default annotation

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new error context.
      CALL ERR_MARK

*  Find the label and units of the NDF so that they can form
*  the default axis label, otherwise take the component name appended
*  with " values"
      CALL KPG1_DANOT( NDF, COMP, AXSDEF, STATUS )

*  Set the suggested default for the parameter.
      CALL PAR_DEF0C( PNLAB, AXSDEF, STATUS )

*  Obtain the annotation from the parameter.
      CALL PAR_GET0C( PNLAB, AXSLAB, STATUS )

*  Look for the null condition.
      IF ( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         AXSLAB = AXSDEF
      END IF

*  Release the new error context.
      CALL ERR_RLSE

      END
