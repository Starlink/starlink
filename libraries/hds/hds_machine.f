      PROGRAM HDS_MACHINE
*+
*  Name:
*     HDS_MACHINE

*  Purpose:
*     Display machine-dependent settings for HDS.

*  Language:
*     Starlink Fortran 77

*  Description:
*     This program displays information about machine-dependent
*     settings which affect HDS behaviour; this includes information
*     describing the native data representation of the host machine.
*     It should normally be used whenever HDS is re-built on a new
*     hardware platform or with a new compiler. It provides a check
*     that the correct compiler options have been used and that the
*     native data representation of the host machine has been
*     identified correctly.
*
*     WARNING: Failure to ensure that the machine dependent settings of
*     HDS are correct could result in data files which cannot be read
*     on other systems.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     7-SEP-1992 (RFWS):
*        Original version.
*     17-DEC-1992 (RFWS):
*        Changed name.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Local Variables:
      INTEGER STATUS             ! Global status

*.

*  Initialise the global status.
      STATUS = SAI__OK

*  Use HDS_SHOW to display information about the native data
*  representation of the host machine.
      CALL HDS_SHOW( 'DATA', STATUS )

      END
