      SUBROUTINE ERR_RLSE
*+
*  Name:
*     ERR_RLSE

*  Purpose:
*     Release (end) the current error context.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_RLSE

*  Description:
*     Release a "mark" in the error message table, returning the Error 
*     Reporting System to the previous error context. Any error messages 
*     pending output will be passed to this previous context, not 
*     annulled.

*  Implementation Notes:
*     -  This subroutine is the ADAM version of ERR_RLSE.

*  Authors:
*     SLW: Sid Wright (UCL)
*     AJC: Alan Chipperfield (STARLINK)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-APR-1983 (SLW):
*        Original version.
*     2-DEC-1988 (AJC):
*        Retain unflushed messages from above the mark.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     2-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     15-DEC-1989 (PCTR):
*        Converted to call EMS_RLSE.
*     16-MAR-1990 (PCTR):
*        Added trap to stop ADAM version returning to the lowest context.
*     26-SEP-1990 (PCTR):
*        Changed call from EMS_$IELEV to EMS_LEVEL.
*     22-JAN-1991 (PCTR):
*        Changed to call only EMS_RLSE.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*.

*  Call EMS_RLSE to release the current error context.
      CALL EMS_RLSE

      END
