*+
*  Name:
*     DAT1_SYS

*  Purpose:
*     Define private system-dependent constants for HDS.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Global constants include file.

*  Description:
*     This file contains definitions of global constants used
*     internally by Fortran DAT_ routines. These constants are
*     system-dependent; this file contains values for VAX/VMS systems.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-AUG-1991 (RFWS):
*        Original version.
*     {enter_changes_here}

*-

*  Global Constants:

*  Are we on a VAX/VMS system?
      LOGICAL DAT__VMS
      PARAMETER ( DAT__VMS = .TRUE. )

*  Are we on a Unix system?
      LOGICAL DAT__UNIX
      PARAMETER ( DAT__UNIX = .FALSE. )

*.
