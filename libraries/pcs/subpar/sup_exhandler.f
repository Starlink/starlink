      SUBROUTINE SUBPAR_EXHANDLER ( STATUS )
*+
*  Name:
*     SUBPAR_EXHANDLER

*  Purpose:
*     Exit handler for parameter system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_EXHANDLER ( STATUS )

*  Description:
*     The exit handler for the parameter system. This routine is called
*     by VMS on image exit. It simply closes HDS.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     Just call the HDS closedown routine.

*  Authors:
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     25-OCT-1984 (BDK):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER ISTAT

*.


      ISTAT = SAI__OK
      CALL HDS_STOP (ISTAT)

      END
