      SUBROUTINE ERR_SYSER( TOKEN, SYSTAT )
*+
*  Name:
*     ERR_SYSER

*  Purpose:
*     Assign an operating system error message to a token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_SYSER( TOKEN, SYSTAT )

*  Description:
*     The text of the error message associated with the operating system 
*     status value, SYSTAT, is assigned to the named message token. This
*     token may then be included in an error message.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        The message token name.
*     SYSTAT = INTEGER (Given)
*        The operating system status value.

*  System-specific:
*     The messages generated using this facility will depend on the 
*     computer system upon which the library is implemented.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-OCT-1989 (PCTR):
*        Original version.
*     15-DEC-1989 (PCTR):
*        Converted to call EMS_SYSER.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) TOKEN

      INTEGER SYSTAT

*.

*  Load message string for TOKEN.
      CALL EMS_SYSER( TOKEN, SYSTAT )

      END
