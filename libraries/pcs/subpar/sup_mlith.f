      SUBROUTINE SUBPAR_MLITH( MONO, STATUS )
*+
*  Name:
*     SUBPAR_MONOLITH

*  Purpose:
*     To check if the task is a monolith.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_MONOLITH( MONO, status )

*  Description:
*     The routine copies the variable MONOLITH from the SUBPAR
*     COMMON block SUBPARPTR.

*  Arguments:
*     MONO = LOGICAL (Returned)
*        .TRUE. if the task is a monolith; .FALSE. otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1991 (AJC):
*        Original version.
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'       ! The SUBPAR COMMON blocks
*        MONOLITH = LOGICAL (Read)
*           Signals if the task is a monolith


*  Arguments Returned:
      LOGICAL MONO

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      MONO = MONOLITH

      END
