      SUBROUTINE ERR_ANNUL( STATUS )
*+
*  Name:
*     ERR_ANNUL

*  Purpose:
*     Annul the contents of the current error context.

*  Language:
*     Starlnk Fortran 77

*  Invocation:
*     CALL ERR_ANNUL( STATUS )

*  Description:
*     Any error messages pending output in the current error context are 
*     annulled, i.e. deleted. The values of any existing message tokens 
*     become undefined and the value of the status argument is reset to 
*     SAI__OK.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status: it is set to SAI__OK on return.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     17-Apr-1983 (SLW):
*        Added MARK and RELEASE mods.
*     20-JUN-1989 (RFWS):
*        Updated prologue, comments and layout.
*     11-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     15-DEC-1989 (PCTR):
*        Converted to call EMS_ANNUL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Status:
      INTEGER STATUS

*.

*  Annul the error message table.
      CALL EMS_ANNUL( STATUS )
 
      END
