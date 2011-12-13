      SUBROUTINE KPG_ENV0C( VARNAM, CVAL, STATUS )
*+
*  Name:
*     KPG_ENV0C

*  Purpose:
*     Reads a string value from an environment variable, using a
*     default value if the variable is undefined.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG_ENV0C( VARNAM, CVAL, STATUS )

*  Description:
*     This routine reads a string value of up to forty characters from
*     a specified environment variable.  No error occurs should the
*     environment variable not be defined, and the supplied value is
*     returned unchanged.

*  Arguments:
*     VARNAM = CHARACTER * ( * ) (Given)
*        The environment variable to check.
*     CVAL = CHARACTER * ( * ) (Given and Returned)
*        The string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     MJC:  Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2009 July 21 (MJC):
*        Original version derived from KPG1_ENV0R.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PSX_ERR'          ! PSX error constants

*  Arguments Given:
      CHARACTER*(*) VARNAM

*  Arguments Returned:
      CHARACTER*(*) CVAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*40 VAL           ! Value of the environment variable

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Attempt to get the value of the environment variable.
      CALL PSX_GETENV( VARNAM, VAL, STATUS )

*  If the environment variable was not defined, annul the error,
*  and leave the supplied value unchanged.
      IF ( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )

*  Use the environment variable's value.
      ELSE
         CVAL = VAL
      END IF

      END
