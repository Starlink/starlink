      SUBROUTINE USI_INIT()
*+
*  Name:
*     USI_INIT

*  Purpose:
*     USI initialisation for standalone (non-ADAM operation)

*  Language:
*     Starlink Fortran 77

*  Description:
*     Initialise the USI_CMN Common Block for standalone operation

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

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Global Variables:
      INCLUDE 'USI_CMN'			! USI initialisation
*        USI_SYINIT = LOGICAL (Returned)
*           System initialised flag

*  Status:
      INTEGER			STATUS

*  Local Variables:
      INTEGER                   CALLID                  ! CALLABLE system
*.

*    Initialise status
      STATUS = SAI__OK

*    Already initialised?
      IF ( .NOT. USI_SYINIT ) THEN

*      Zero the dataset system
        CALL USI0_DSINIT()

*      Install the CALLABLE system
c        CALL USI0_DEFCALL( CALLID, STATUS )

*      Now initialised
        USI_SYINIT = .TRUE.

*      Logging state
        CALL USI0_DEFLOG( STATUS )

*      Define first context
c        CALL USI0_MARK( 'CALLABLE', STATUS )

*      Load command arguments
c        CALL USI0_SETCL1( STATUS )

*    End if already initialised switch
      END IF

      END
