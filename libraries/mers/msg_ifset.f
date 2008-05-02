      SUBROUTINE MSG_IFSET( FILTER, STATUS )
*+
*  Name:
*     MSG_IFSET

*  Purpose:
*     Set the filter level for conditional message output.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG_IFSET( FILTER, STATUS )

*  Description:
*     The value of the message filtering level is set using the given
*     filtering value. If no such level exists, then an error is 
*     reported and the status returned set to MSG__IFINV: the current 
*     filtering level remains unchanged.

*  Arguments:
*     FILTER = INTEGER (Given)
*        The filtering level.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1991 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1991 (PCTR):
*        Original version.
*     02-MAY-2008 (TIMJ):
*        Added MSG__DEBUG
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ public constants
      INCLUDE 'MSG_ERR'          ! MSG_ error codes

*  Global Variables:
      INCLUDE 'MSG_CMN'          ! MSG_ output filter level

*  Arguments Given:
      INTEGER FILTER

*  Status:
      INTEGER STATUS

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the given filter value is acceptable.
      IF ( FILTER .LT. MSG__QUIET .OR. FILTER .GT. MSG__DEBUG ) THEN

*     The given value for message filtering is outside the allowed
*     range: set status and report an error message.
         CALL EMS_MARK
         STATUS = MSG__INVIF
         CALL EMS_SETI( 'FILTER', FILTER )
         CALL EMS_REP( 'MSG_IFSET_INVIF',
     :   'MSG_IFSET: Invalid message filtering value: ^FILTER', 
     :   STATUS )
         CALL EMS_RLSE
      ELSE

*     Assign the message filtering level.
         MSGINF = FILTER
      END IF

      END
