      SUBROUTINE ERR_BEGIN( STATUS )
*+
*  Name:
*     ERR_BEGIN

*  Purpose:
*     Create a new error reporting environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_BEGIN( STATUS )

*  Description:
*     Begin a new error reporting environment by marking a new error 
*     reporting context and then resetting the status argument to SAI__OK.
*     If ERR_BEGIN is called with the status argument set to an error 
*     value, a check is made to determine if there are any messages 
*     pending output in the current context: if there are none, an 
*     error report to this effect is made on behalf of the calling
*     application.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-SEP-1990 (PCTR):
*        Original version.
*     15-JAN-1991 (PCTR):
*        Implemented status check.
*     26-JUN-1991 (PCTR):
*        Added mark and release to prevent message tokens being anulled
*        on error.
*     14-AUG-1991 (PCTR):
*        Changed to call EMS_BEGIN.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Status:
      INTEGER STATUS

*.

*  Call EMS_BEGIN.
      CALL EMS_BEGIN( STATUS )

      END
