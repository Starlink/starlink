      SUBROUTINE AGP1_EPSMG( STATUS )
*+
*  Name:
*     AGP1_EPSMG

*  Purpose:
*     Do any required merging of EPS files.

*  Invocation:
*     CALL AGP1_EPSMG( STATUS )

*  Description:
*     This function should be called once PGPLOT has been closed down. If
*     the device that was in use is an accumulating postscript device, then
*     the file holding the new postscript output is appended to any
*     existing file holding postscript output from a previous command.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.
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
*     DSB: David Berry (STARLINK)

*  History:
*     20-MAR-2013 (DSB):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'agi_eps'

*  Status:
      INTEGER STATUS

*  Local Variables:
*.

*  Check status on entry
      IF( STATUS .NE. SAI__OK ) RETURN

*  If both old and new EPS files have been recorded in common, then
*  merge them. First ensure PGPLOT is closed down so that any remaining
*  postscript output is written to the output file.
      IF( OLDEPS .NE. ' ' .AND. NEWEPS .NE. ' ' ) THEN
         CALL PGEND
         CALL AGP1_MERGE( OLDEPS, NEWEPS, CLREPS, STATUS )
      END IF

*  Ensure the names are clear in common.
      OLDEPS = ' '
      NEWEPS = ' '

      END
