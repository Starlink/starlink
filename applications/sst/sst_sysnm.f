      SUBROUTINE SST_SYSNM( SYSNAM, STATUS )
*+
* Name:
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
