      SUBROUTINE SUBPAR_ACCPT ( STATUS )
*+
*  Name:
*     SUBPAR_ACCPT

*  Purpose:
*     put parameters into accept prompt state.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_ACCPT ( STATUS )

*  Description:
*     Set each parameter for the currently active action into a relevant 
*     ACCPT state if its current state permits.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     For each parameter call SUBPAR_ACCPT1

*  Authors:
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-MAY-1987 (BDK):
*        Original version
*     07-DEC-1992 (AJC):
*        Use SUBPAR_ACCPR1 to do the business
*     10-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*     16-MAR-1993 (AJC):
*        RENAME FROM SUBPAR_ACCPR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


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

         CALL SUBPAR_ACCPT1( NAMECODE, STATUS )

      ENDDO

      END
