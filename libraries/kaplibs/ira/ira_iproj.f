      SUBROUTINE IRA_IPROJ( LIST, STATUS )
*+
*  Name:
*     IRA_IPROJ

*  Purpose:
*     Returns a list of supported projection names.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_IPROJ( LIST, STATUS )

*  Description:
*     A string is returned containing the list of supported projection
*     names and equivalent names. The names are separated by commas. The
*     currently supported projections are:
*
*     GNOMONIC ( or equivalently TANGENT_PLANE )
*
*     LAMBERT (  or equivalently CYLINDRICAL )
*
*     AITOFF (  or equivalently ALL_SKY )
*
*     ORTHOGRAPHIC
*
*     See ID/2 appendix "Projection Equations" for more details about
*     the supported projections.

*  Arguments:
*     LIST = CHARACTER * ( * ) (Returned)
*        The list of supported projections and equivalent names. The
*        character variable supplied for this argument should have a
*        declared size equal to the value of parameter IRA__SZPLS. If
*        the supplied string is not long enough to hold all the names, a
*        warning message is given, but no error status is returned. Each
*        returned projection name has a maximum length given by symbolic
*        constant IRA__SZPRJ.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JAN-1991 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified for IRA Version 2.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Arguments Returned:
      CHARACTER LIST*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Return the list of projection names currently recognised by the
*  IRA system. The length of this string is stored in parameter
*  IRA__SZPLS which should be updated when new projections are added
*  to the list.
      LIST = 'GNOMONIC,TANGENT_PLANE,LAMBERT,CYLINDRICAL,'//
     :       'AITOFF,ALL_SKY,ORTHOGRAPHIC'

*  If the list was truncated, give a warning message.
      IF( LEN( LIST ) .LT. IRA__SZPLS ) THEN
         CALL MSG_OUT( 'IRA_IPROJ_MSG1',
     :                 'WARNING - The list of supported projections'//
     :                 ' had to be truncated', STATUS )
      END IF

      END
