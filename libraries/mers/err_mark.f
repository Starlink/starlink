      SUBROUTINE ERR_MARK
*+
*  Name:
*     ERR_MARK

*  Purpose:
*     Mark (start) a new error context. 

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_MARK

*  Description:
*     Begin a new error reporting context so that delivery of subsequently 
*     reported error messages is deferred and the messages held in the 
*     error table. Calls to ERR_ANNUL, ERR_FLUSH and ERR_LOAD will only 
*     flush or annul the contents of the error table within this new 
*     context.

*  Authors:
*     SLW: Sid Wright (UCL)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-APR-1983 (SLW):
*        Original version.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     12-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     15-DEC-1989 (PCTR):
*        Converted to use EMS_ calls.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*.
 
*  Create a new error context.
      CALL EMS_MARK

      END
