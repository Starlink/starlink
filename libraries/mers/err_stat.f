      SUBROUTINE ERR_STAT( STATUS )
*+
*  Name:
*     ERR_STAT

*  Purpose:
*     Inquire the last reported error status.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_STAT( STATUS )

*  Description:
*     The current error context is checked for any error messages pending 
*     output. If none exist, the status argument is returned set to 
*     SAI__OK. If any messages have been reported, the status argument is 
*     returned set to the last reported value.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status: it returned set to the last reported
*        error status within the current error context; if none exist,
*        it is returned set to SAI__OK.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1990 (PCTR):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Status:
      INTEGER STATUS

*.

*  Call EMS_STAT.
      CALL EMS_STAT( STATUS )

      END
