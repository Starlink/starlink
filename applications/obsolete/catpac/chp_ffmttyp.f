      SUBROUTINE
     :  CHP_FFMTTYP (FORMAT, TYPE, STATUS)
*+
*  Name:
*     CHP_FFMTTYP

*  Purpose:
*     Find the type of field from the format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_FFMTTYP (FORMAT, TYPE, STATUS)

*  Description :
*     Find the type of field from the format.

*  Arguments:
*     FORMAT = CHARACTER * ( * ) (Given)
*        Format.
*     TYPE = CHARACTER * ( 1 ) (Returned)
*        Type of the field C, D, I, L, R
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     CHP__IVLDCFMT

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-Aug-1992 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHP_ERR'   ! Standard CHP errors

*  Arguments Given:
      CHARACTER * ( * ) FORMAT
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      if (format(1:1).eq.'C') then
        type = 'C'
      elseif (format(1:3).eq.'R*8') then
         type = 'D'
      elseif (format(1:1).eq.'R') then
         type = 'R'
      elseif (format(1:1).eq.'I') then
          type = 'I'
      elseif (format(1:1).eq.'L') then
          type = 'L'
      elseif (format(1:1).eq.'A') then
        type = 'C'
      elseif (format(1:1).eq.'D') then
         type = 'D'
      elseif (format(1:1).eq.'F'.or.format(1:1).eq.'E') then
         type = 'R'
      elseif (format(1:1).eq.'I') then
          type = 'I'
      elseif (format(1:1).eq.'L') then
          type = 'L'
      else
          type = 'C'
      endif
      end
