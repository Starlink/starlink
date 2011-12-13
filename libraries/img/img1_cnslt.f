      SUBROUTINE IMG1_CNSLT( SLOT, STATUS )
*+
*  Name:
*     IMG1_CNSLT

*  Purpose:
*     Cancel a parameter slot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_CNSLT( SLOT, STATUS )

*  Description:
*     The routine frees all the NDF_ and IMG_ system resources
*     associated with a specified parameter slot. It also frees the
*     slot and cancels the parameter association. This makes the
*     associated data inaccessible. and any later attempt to re-use the
*     parameter will cause a new NDF to be obtained.

*  Arguments:
*     SLOT = INTEGER (Given)
*        PCB slot number to cancel.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine attempts to execute even if STATUS is set on entry,
*     although no further error report will be made if it subsequently
*     fails under these circumstances.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     21-FEB-1992 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ private constants
      INCLUDE 'IMG_ERR'          ! IMG_ error codes
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Global Variables:
      INCLUDE 'IMG_PCB'          ! IMG_ Parameter Control Block
*        PCB_PARAM( IMG__MXPAR ) = CHARACTER * ( IMG__SZPAR ) (Write)
*           Parameter name.

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks

*  Arguments Given:
      INTEGER SLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) PARAM ! Parameter name
      LOGICAL TEMP               ! NDF is temporary?

*.

*  Start a new error context.
      CALL ERR_BEGIN( STATUS )

*  Check that the slot number supplied is valid. Report an error if it
*  is not.
      IF ( ( SLOT .LT. 1 ) .OR. ( SLOT .GT. IMG__MXPAR ) ) THEN
         STATUS = IMG__FATIN
         CALL MSG_SETI( 'SLOT', SLOT )
         CALL ERR_REP( 'IMG1_CNSLT_SLOT',
     :                 'Routine IMG1_CNSLT called with an invalid ' //
     :                 'SLOT argument of ^SLOT (internal ' //
     :                 'programming error).', STATUS )

*  Retain the parameter name and check if the NDF associated with the
*  slot is temporary.
      ELSE
         PARAM = PCB_PARAM( SLOT )
         TEMP = .FALSE.
         CALL NDF_ISTMP( PCB_INDF( SLOT ), TEMP, STATUS )

*  Free the slot.
         CALL IMG1_FRSLT( SLOT, .TRUE., STATUS )

*  If the NDF was not temporary, cancel the parameter association (if
*  it is temporary, then it will not be associated with an external
*  parameter).
         IF ( .NOT. TEMP ) CALL DAT_CANCL( PARAM, STATUS )
      END IF

*  End the error context.
      CALL ERR_END( STATUS )

      END
* $Id$
