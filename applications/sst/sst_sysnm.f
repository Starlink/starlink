      SUBROUTINE SST_SYSNM( SYSNAM, STATUS )
*+
*  Name:
*    SST_SYSNM

*  Purpose:
*     Returns the operating system name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_SYSNM( SYSNAM, STATUS )

*  Description:
*     This routine returns the operating system name. The only
*     differentiation is between UNIX and VMS at present.

*  Arguments:
*     SYSNAM = CHARACTER * ( * ) (Returned)
*        Name of the system, either UNIX or VMS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Machine-Specific Features Used:
*     Assumes any operating system which isn't VMS is UNIX.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-DEC-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Returned:
      CHARACTER * ( * ) SYSNAM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 4 ) SYS      ! System name
      CHARACTER * ( 4 ) NOD      ! Node name
      CHARACTER * ( 4 ) REL      ! OS release version
      CHARACTER * ( 4 ) VER      ! Sub-version of OS
      CHARACTER * ( 4 ) MACH     ! Hardware name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the system details from PSX.
      CALL PSX_UNAME( SYS, NOD, REL, VER, MACH, STATUS )
      IF ( SYS .EQ. 'VMS' ) THEN
         SYSNAM = 'VMS'
      ELSE
         SYSNAM = 'UNIX'
      END IF
* @(#)sst_sysnm.f   1.1   94/12/06 17:41:32   96/07/05 10:27:34
      END
