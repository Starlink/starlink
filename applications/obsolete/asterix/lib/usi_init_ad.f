      SUBROUTINE USI_INIT()
*+
*  Name:
*     USI_INIT

*  Purpose:
*     USI Block Data Initialisation for ADAM operation

*  Language:
*     Starlink Fortran 77

*  Description:
*     Initialise the USI_CMN Common Block for ADAM operation

*  Authors:
*     DJA: David J. Allan (JET-X,University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     17-Aug-1994 (DJA):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              	! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Global Variables:
      INCLUDE 'USI_CMN'			! USI initialisation
*        USI_SYINIT = LOGICAL (Returned)
*           System initialised flag

*  External references:
      EXTERNAL USI_BLK

*  Status:
      INTEGER                   STATUS

*  Local Variables:
      INTEGER			ADAMID			! ADAM system
      INTEGER			CALLID			! CALLABLE system
*.

*    Not already initialised?
      IF ( .NOT. USI_SYINIT ) THEN

*      Clear status
        STATUS = SAI__OK

*      Zero the dataset system
        CALL USI0_DSINIT()

*      Install the ADAM system
        CALL USI0_DEFADAM( ADAMID, STATUS )

*      Install the CALLABLE system
c        CALL USI0_DEFCALL( CALLID, STATUS )

*      Now initialised
        USI_SYINIT = .TRUE.

*      Logging state
        CALL USI0_DEFLOG( STATUS )

*      Define first context
        CALL USI0_MARK( 'ADAM', STATUS )

*      Load command arguments
        CALL USI0_SETCL1( STATUS )

      END IF

      END
