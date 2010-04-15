      SUBROUTINE
     : CHP_DELADDF( FILE, STATUS )
*+
*  Name:
*     CHI_DELADDF

*  Purpose:
*     Delete a CHP additional information file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_DELADDF( FILE, STATUS )
*
*  Description:
*     Delete a CHP additional information file. Currently assumes that the
*     file is in the current directory. Later to include a search path.

*  Arguments:
*     FILE = CHARACTER * ( * ) (Given)
*        Name of the catalogue whose description file is to be deleted.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     None

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1991 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
*      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'FIO_ERR'

*  Local Variables:
      CHARACTER * ( 32 ) FILENAME ! Full filename
      CHARACTER * ( 28 ) FILENAME1 ! Full filename
      INTEGER LENGTH

*  Arguments Given:
      CHARACTER * ( * ) FILE

*  Arguments Returned:

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER CHR_LEN
*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      filename = 'ADDF'//file
      length = chr_len(filename)
      call chr_appnd('.DAT',filename,length)
      call fio_erase(filename, status)
      if (status .ne. SAI__OK) then
        call err_annul(status)
      endif
*
      END
