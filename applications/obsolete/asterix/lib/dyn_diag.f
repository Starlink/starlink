      SUBROUTINE DYN_DIAG( STATE, STATUS )
*+
*  Name:
*     DYN_DIAG

*  Purpose:
*     Set DYN package diagnostics state

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DYN_DIAG( STATE, STATUS )

*  Description:
*     Set the DYN package diagnostic state. This is simpy on or off
*     corresponding to STATE having the values true and false respectively.

*  Arguments:
*     STATE = LOGICAL (given)
*        Setting of diagnostic flag
*     STATUS = INTEGER (given)
*        The global status.

*  References:
*     DYN Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/dyn.html

*  Keywords:
*     package:dyn, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     21 Dec 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'DYN_CMN'                                 ! DYN common block
*       DYS_DIAG = LOGICAL (returned)
*         DYN package diagnostic state

*  Arguments Given:
      LOGICAL			STATE

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			DYN0_BLK		! Ensures inclusion
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set flag
      DYS_DIAG = STATE

      END
