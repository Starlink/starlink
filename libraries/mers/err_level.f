      SUBROUTINE ERR_LEVEL( LEVEL )
*+
*  Name:
*     ERR_LEVEL

*  Purpose:
*     Inquire the current error context level. 

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_LEVEL( LEVEL )

*  Description:
*     Return the number of context markers set in the error message table. 

*  Arguments:
*     LEVEL = INTEGER (Returned)
*        The error context level: all values greater than one indicate 
*        the deferral of reported error messages.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-OCT-1990 (PCTR):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Arguments Returned:
      INTEGER LEVEL

*.

*  Call EMS_LEVEL.
      CALL EMS_LEVEL( LEVEL )

      END
