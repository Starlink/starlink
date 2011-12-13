      SUBROUTINE KPG1_GNTIT( NDF, PNTITL, DEFAUL, TITLE, STATUS )
*+
*  Name:
*     KPG1_GNTIT

*  Purpose:
*     Obtains a title using the NDF title as the suggested default.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GNTIT( NDF, PNTITL, DEFAUL, TITLE, STATUS )

*  Description:
*     This routine obtains a title from the parameter system.  The
*     suggested default is title of the input NDF if it has one,
*     otherwise a supplied default is used.  If a bad status, other
*     than abort, is returned by the parameter system when getting the
*     value, the error is annulled and the output title is the default
*     value.

*  Arguments:
*     NDF = INTEGER (Given)
*        The NDF identifier.
*     PNTITL = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter from which the title will be
*        obtained.
*     DEFAUL = CHARACTER * ( * ) (Given)
*        The suggested default when the NDF does not have a title, or
*        the actual value returned when a null (!) value or any other
*        non-abort bad status is obtained from the parameter system.
*     TITLE = CHARACTER * ( * ) (Returned)
*        The title obtained from the parameter system.
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
      INTEGER
     :  NDF

      CHARACTER * ( * )
     :  PNTITL,
     :  DEFAUL

*  Arguments Returned:
      CHARACTER * ( * )
     :  TITLE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL                    ! True if:
     :  THERE                    ! NDF has a title

*.

*  Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Start a new error context.

      CALL ERR_MARK

*    In case there is no NDF title, initialise the suggested default
*    with supplied default.

      TITLE = DEFAUL

*    Find the title of the NDF so that it can be the default title.

      CALL NDF_STATE( NDF, 'TITLE', THERE, STATUS )
      IF ( THERE ) CALL NDF_CGET( NDF, 'TITLE', TITLE, STATUS )

*    Set the suggested default for the parameter.

      CALL PAR_DEF0C( PNTITL, TITLE, STATUS )

*    Obtain the title from the parameter.

      CALL PAR_GET0C( PNTITL, TITLE, STATUS )

*    Look for the null condition.

      IF ( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         TITLE = DEFAUL
      END IF

*    Release the new error context.

      CALL ERR_RLSE

      END
