      SUBROUTINE MSG_SYNC( STATUS )
*+
*  Name:
*     MSG_SYNC

*  Purpose:
*     Synchronise message output via the user interface.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG_SYNC( STATUS )

*  Description:
*     This performs a synchronisation handshake with the user interface.
*     This is required it the current task has been outputting messages
*     via the user interface and now wants to use a graphics cursor on
*     the command device.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status: it is returned set to MSG__SYNER on error.

*  Implementation Notes:
*     -  This subroutine is the STANDALONE version of MSG_SYNC.
*     -  The STANDALONE version does nothing.

*  Authors:
*     BDK: Dennis Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1985 (BDK):
*        Original version.
*     20-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     15-DEC-1989 (PCTR):
*        Standalone version adapted from MSG_SYNC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                      ! No implicit typing

*  Status:
      INTEGER STATUS

*.
 
*  Standalone version does nothing.

      END
