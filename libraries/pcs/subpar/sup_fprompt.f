
      SUBROUTINE SUBPAR_FPROMPT ( STATUS )
*+
*  Name:
*     SUBPAR_FPROMPT

*  Purpose:
*     To put parameters into an appropriate force prompt state.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_FPROMPT ( STATUS )

*  Description:
*     Set each parameter for the currently active action into the appropriate
*     FPROMPT state if its current state permits.
*
*     The FPROMPT state may be caused by the special keyword PROMPT on the
*     command line and will cause a prompt to be issued for all required
*     inactive parameters (or the suggested value to be used if ACCEPT is
*     also set) regardless of the VPATH.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     For each parameter relevant to the currently active program, check
*     its current state. Change the state where relevant.

*  Authors:
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     28-MAY-1987 (BDK):
*        Original
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     16-MAR-1993 (AJC):
*        Correct bug if ACCEPT PROMPT
*        Revise for separate RESACC and RESACCPR
*        Remove unnecessary ELSE IF clauses
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  Local Variables:
      INTEGER NAMECODE       ! index to parameters

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   PROGNUM selects the relevant parameters if this is a monolith.
*   Otherwise, it has the effect of selecting all parameters.
*
      DO NAMECODE = PROGADD(1,PROGNUM), PROGADD(2,PROGNUM)

         IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__GROUND ) THEN
            PARSTATE(NAMECODE) = SUBPAR__FPROMPT
         ELSE IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESET ) THEN
            PARSTATE(NAMECODE) = SUBPAR__RESPROM
         ELSE IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACCEPT ) THEN
            PARSTATE(NAMECODE) = SUBPAR__ACCPR
         ELSE IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESACC ) THEN
            PARSTATE(NAMECODE) = SUBPAR__RESACCPR
         ENDIF

      ENDDO

      END
