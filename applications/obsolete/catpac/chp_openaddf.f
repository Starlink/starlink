      SUBROUTINE
     : CHP_OPENADDF( FILE, ACMODE, FORM, RECSZ, FD, STATUS )
*+
*  Name:
*     CHI_OPENADDF

*  Purpose:
*     Open a CHP additional information file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_OPENADDF( FILE, ACMODE, FORM, RECSZ, FD, STATUS )
*
*  Description:
*     Open a CHP additional information file. Currently assumes that the
*     file is in the current directory. Later to include a search path.

*  Arguments:
*     FILE = CHARACTER * ( * ) (Given)
*        Name of the catalogue whose description file is to be opened.
*     ACMODE = CHARACTER * ( * ) (Given)
*        Expression giving the required access mode. Valid modes are:
*        'READ' - Open the file read only. The file must exist.
*        'WRITE' - Create a new file and open it to write.
*        'UPDATE' - Open a file to write. The file must exist.
*        'APPEND' - Open a file to append. The file must exist.
*     FORM = CHARACTER * ( * ) (Given)
*        Expression giving the required formatting of the file.
*        Valid formats are:
*        'FORTRAN' - Formatted file, normal fortran interpretation of the first
*                    character of each record.
*        'LIST' - Formatted file, single spacing between records.
*        'NONE' - Formatted file, no implied carriage return.
*        'UNFORMATTED' - Unformatted, no implied carriage return.
*     RECSZ = INTEGER (Given)
*        Expression giving the maximum record size in bytes.
*        Set to zero if the FORTRAN default is required.
*     FD = INTEGER (Returned)
*        Variable to contain the file descriptor.
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

*  Local Variables:
      CHARACTER * ( 32 ) FILENAME ! Full filename
      CHARACTER * ( 28 ) FILENAME1 ! Full filename
      INTEGER LENGTH

*  Arguments Given:
      CHARACTER * ( * ) FILE
      CHARACTER * ( * ) ACMODE
      CHARACTER * ( * ) FORM
      INTEGER RECSZ

*  Arguments Returned:
      INTEGER FD

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
      call fio_open(filename, acmode, form, recsz, fd, status)
*
      END
