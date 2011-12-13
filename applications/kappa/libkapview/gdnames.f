      SUBROUTINE GDNAMES( STATUS )
*+
*  Name:
*     GDNAMES

*  Purpose:
*     Shows which graphics devices are available.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GDNAMES( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The routine displays a list of the graphics devices available and
*     the names (both traditional Starlink GNS names and the equivalent
*     PGPLOT names) which identify them.  Each name is accompanied by a
*     brief descriptive comment.

*  Usage:
*     gdnames

*  Copyright:
*     Copyright (C) 1989-1990 Science & Engineering Research Council.
*     Copyright (C) 1999, 2001 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-MAY-1989 (RFWS):
*        Original version (RFWS).
*     1990 Mar 31 (MJC):
*        Renamed from SHODEV to GDNAMES for consistency (MJC).
*     1999 Jul 19 (TDCA):
*        Replaced call to SGS_WNAME with call to PGLDEV.
*     7-NOV-2001 (DSB):
*        Changed to use AGP_GDLST.
*     {enter_further_changes_here}

*-
*    Type definitions:
      IMPLICIT NONE              ! No implicit typing

*    Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*    Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Display a header for the list of devices.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', 'The following graphics devices are '//
     :              'available. The first column holds the GNS names,'//
     :              ' and the second the equivalent PGPLOT names (in '//
     :              'parentheses). Either form can be used...',
     :              STATUS )
      CALL MSG_BLANK( STATUS )

*  Call PGLDEV to display the list of devices.
      CALL AGP_GDLST( STATUS )

*  Put a blank line at the end of the list.
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

      END
