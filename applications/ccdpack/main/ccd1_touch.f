      SUBROUTINE CCD1_TOUCH( NDFID, TASK, STATUS )
*+
*  Name:
*     CCD1_TOUCH

*  Purpose:
*     Touches an NDF leaving a taskname item showing the date and time

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_TOUCH( NDFID, TASK, STATUS )

*  Description:
*     This routine accesses the CCDPACK extension of the NDF whose
*     identifier is NDFID and creates an item whose name is given as
*     the value of the TASK argument. The value of the TASK object
*     is a string containing the current date and time.

*  Arguments:
*     NDFID = INTEGER (Given)
*        NDF identifier.
*     TASK = CHARACTER * ( * ) (Given)
*        The name of the TASK which is calling thus routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The extension item created by this application is
*           .MORE.CCDPACK.TASK
*     and has the value of the current time and date.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     17-JAN-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NDFID
      CHARACTER * ( * ) TASK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 30 ) CTIME   ! Time and date as a character string
      INTEGER NTICKS             !  Number of time ticks

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the time and date.
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_CTIME( NTICKS, CTIME, STATUS )

*  Now store this value in the expected object.
      CALL CCG1_STO0C( NDFID, TASK, CTIME, STATUS )

      END
* $Id$
