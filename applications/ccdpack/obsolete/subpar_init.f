      SUBROUTINE SUBPAR_INIT( NAMECODE, STATE, STATUS )
*+
*  Name:
*     SUBPAR_INIT

*  Purpose:
*     Initialise variable to initial state.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL SUBPAR_INIT( NAMECODE, STATE, STATUS )

*  Arguments:
*     NAMECODE = INTEGER (Given)
*        SUBPAR namecode of parameter.
*     STATE = INTEGER (Given)
*        SUBPAR state into which the parameter must be initialised.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This is a proof-of-concept hack, pending AJC coming up with a
*     proper implementation within SUBPAR.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-MAY-2001 (MBT):
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
      INCLUDE 'DAT_PAR'
      INCLUDE '/stardev/local/sources/pcs/subpar/subpar_cmn'
      INCLUDE '/stardev/local/sources/pcs/subpar/subpar_par'
      
*  Arguments Given:
      INTEGER NAMECODE
      INTEGER STATE
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL VOOD               ! Is the current value out of date?
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      
*  See if it has a value which is out of date.  This should be the case
*  if the state into which we are about to restore it (its initial
*  state) is not one which will should have had a value when it was
*  set up.  I may not have the logic here correct for all circumstances.
      VOOD = STATE .NE. SUBPAR__ACTIVE

*  If the value is out of date, mark the locators as inactive.
      IF ( VOOD ) CALL SUBPAR_CANCL( NAMECODE, STATUS )

*  Set the parameter's state to the requested one.
      PARSTATE( NAMECODE ) = STATE

      END
* $Id$
