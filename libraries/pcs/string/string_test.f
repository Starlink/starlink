      PROGRAM STRING_TEST
*+
*  Name:
*     STRING_TEST

*  Purpose:
*     To test the STRING installation - not its functionality

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     STRING_TEST

*  Description:
*     Compiling and linking this program will give a good indication
*     that the STRING library is correctly installed

*  Authors:
*     ENV: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-MAR-1993 (ENV):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*5 STRING
*.

      STATUS = SAI__OK

*  Call a STRING routine
      CALL STRING_STRIPQUOT('''OK''', STRING, STATUS )

      PRINT *,' STRING_TEST ends ',STRING

      END
