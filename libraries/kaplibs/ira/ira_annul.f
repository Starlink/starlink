      SUBROUTINE IRA_ANNUL( IDA, STATUS )
*+
*  Name:
*     IRA_ANNUL

*  Purpose:
*     Annuls an IRA identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_ANNUL( IDA, STATUS )

*  Description:
*     This routine should be called when access to the astrometry
*     information associated with an IRA identifier is no longer
*     needed. It releases the resources used to store the astrometry
*     information.
*
*     This routine attempts to execute even if STATUS is bad on entry.
*     However, in this case no error report will be produced if this
*     routine subsequently fails.

*  Arguments:
*     IDA = INTEGER ( Given )
*        The IRA identifier to be annulled.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
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
*     21-JAN-1991 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified for IRA version 2
*     12-FEB-1993 (DSB):
*        Remove HDS functions.
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

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_VALID( IRA__MAX ) = LOGICAL (Write)
*           If true, then the associated elements of the other arrays
*           held in common contain valid astrometry information.

*  Arguments Given:
      INTEGER IDA

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Start a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  Check the supplied identifier is valid.
      CALL IRA1_CHECK( IDA, STATUS )

*  If it is, set the IRA identifier invalid.
      IF( STATUS .EQ. SAI__OK ) ACM_VALID( IDA ) = .FALSE.

*  If an error has been reported, give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_ANNUL_ERR1',
     :          'IRA_ANNUL: Unable to annul an IRA identifier', STATUS )
      END IF

*  End the error reporting environment.
      CALL ERR_END( STATUS )

      END
