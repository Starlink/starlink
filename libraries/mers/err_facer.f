      SUBROUTINE ERR_FACER( TOKEN, STATUS )
*+
*  Name:
*     ERR_FACER

*  Purpose:
*     Assign the message associated with a Starlink STATUS value to a token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_FACER( TOKEN, STATUS )

*  Description:
*     The text of the message associated with the Starlink facility STATUS 
*     value is assigned to the named message token. This token may then be 
*     included in an error message.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        The message token name.
*     STATUS = INTEGER (Given)
*        The facility status value.

*  System-specific:
*     The messages generated using this facility will depend on the 
*     computer system upon which the library is implemented.

*  Authors:
*     BKM: B.K. McIlwrath (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1994 (BKM):
*        Original version (Converted from ERR_SYSER)
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) TOKEN

      INTEGER STATUS

*.

*  Load message string for TOKEN.
      CALL EMS_FACER( TOKEN, STATUS )

      END
