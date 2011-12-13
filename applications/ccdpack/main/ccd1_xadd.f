      SUBROUTINE CCD1_XADD( PARAM, LOC, NAME, MODIFY, CANCEL, VALUE,
     :                      EXISTS, FRMEXT, STATUS )
*+
*  Name:
*     CCD1_XADD

*  Purpose:
*     Enters a corroborated value to a HDS named object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_XADD( PARAM, LOC, NAME, MODIFY, CANCEL, VALUE, EXISTS,
*                     FRMEXT, STATUS )

*  Description:
*     The routine accesses a double precision user supplied value
*     through the ADAM parameter PARAM. If no value is supplied
*     indicated by a null return) then the status is annulled and the
*     routine continues. The routine then checks to see if the NAMEd
*     item of the object pointed to by LOC already exists. If it does
*     then no further action is taken unless the MODIFY value is set
*     true. If MODIFY is true then the new value is written to the NAMEd
*     object. If the object does not exist then the new value (if
*     it has been supplied) is written. A message is issued if a
*     modification has occurred.
*
*     On exit the value which is already in the named object or which is
*     entered by this routine is returned in VALUE. If no value exists
*     or is entered then EXISTS is false, if the value exists and is
*     unmodified then FRMEXT is true, otherwise it is false.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter which is to be used to specify
*        the value.
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to parent object of the object which contains the value
*        or which is to contain the value.
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the object which contains the value or which is to
*        contain the value. Note that the storage object is located by a
*        combination of LOC and NAME.
*     MODIFY = LOGICAL (Given)
*        If true then an existing value in the storage object may be
*        overwritten.
*     CANCEL = LOGICAL (Given)
*        If true then the parameter association will be cancelled.
*     VALUE = DOUBLE PRECISION (Returned)
*        The value entered into the extension. This is the current value
*        (if one exists and modify is false), or the value obtained from
*        the environment.
*     EXISTS = LOGICAL (Returned)
*        Whether or not the extension item exists after this routine is
*        called.
*     FRMEXT = LOGICAL (Returned)
*        Whether the value was in the NDF extension already and is
*        unmodified or not.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1992 (PDRAPER):
*        Original version.
*     7-FEB-1994 (PDRAPER):
*        Added EXISTS and VALUE for reports at higher level.
*     8-FEB-1994 (PDRAPER):
*        Added FRMEXT, removed warning message.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter system error constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) LOC
      CHARACTER * ( * ) NAME
      LOGICAL MODIFY
      LOGICAL CANCEL

*  Arguments Returned:
      DOUBLE PRECISION VALUE
      LOGICAL EXISTS
      LOGICAL FRMEXT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL NEED               ! Need a value to return to caller

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an error context.
      CALL ERR_MARK

*  Look for existing object, may only change this if modify is set.
      NEED = .TRUE.
      CALL DAT_THERE( LOC, NAME, EXISTS, STATUS )
      IF ( ( .NOT. EXISTS ) .OR. MODIFY ) THEN

*  Object exists, do we really want to supercede it?
         CALL PAR_GET0D( PARAM, VALUE, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN

*  No value. Take this as an indication that value shouldn't be changed.
            CALL ERR_ANNUL( STATUS )
         ELSE

*  Write the value into the named object.
            CALL CMP_MOD( LOC, NAME, '_DOUBLE', 0, 0, STATUS )
            CALL CMP_PUT0D( LOC, NAME, VALUE, STATUS )
            NEED = .FALSE.
         END IF
      END IF

*  If we need a value to pass back and havn't received one from the
*  environment then get the value from the object.
      FRMEXT = .FALSE.
      IF ( EXISTS .AND. NEED ) THEN
         CALL CMP_GET0D( LOC, NAME, VALUE, STATUS )
         FRMEXT = .TRUE.
      END IF

*  Cancel parameter association, if requested.
      IF ( CANCEL )  CALL PAR_CANCL( PARAM, STATUS )

*  End the error context.
      CALL ERR_RLSE
      END
* $Id$
