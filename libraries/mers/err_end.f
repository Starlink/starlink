      SUBROUTINE ERR_END( STATUS )
*+
*  Name:
*     ERR_END

*  Purpose:
*     End the current error reporting environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_END( STATUS )

*  Description:
*     Check if any error messages are pending output in the previous 
*     error reporting context. If there are, the current context is 
*     annulled and then released; if not, the current context is just 
*     released. The last reported status value is returned on exit.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-SEP-1990 (PCTR):
*        Original version.
*     15-JAN-1990 (PCTR):
*        Changed to call EMS_END.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Status:
      INTEGER STATUS

*.

*  Call EMS_END.
      CALL EMS_END( STATUS )

      END
