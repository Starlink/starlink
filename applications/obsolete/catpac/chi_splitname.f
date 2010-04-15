      SUBROUTINE
     : CHI_SPLITNAME( INPUT, DBNAME, CATNAME, STATUS )
*+
*  Name:
*     CHI_SPLITNAME

*  Purpose:
*     Split catalogue name into database name and internal name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_SPLITNAME( INPUT, DBNAME, CATNAME, STATUS )
*
*  Description:
*     Splits the input catalogue name into a database identifier an a
*     catalogue identifier. It is suggested that the first three
*     characters of the catalogue name, as it is known to the user,
*      will be the database name and the remaining characters the name
*      of the catalogue in that database. ADCIRPS would be catalogue
*      IRPS in the ADC database.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue as known be the user.
*     DBNAME = CHARACTER * ( 3 ) (Returned)
*        Database name.
*     CATNAME = CHARACTER * ( CHI__SZNAME ) (Returned)
*        Name of the catalogue within the database.
*     STATUS = INTEGER
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
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHI_PAR'          ! Standard CHI constants
      INCLUDE 'CHI_ERR'          ! Standard CHI errors

*  Arguments Given:
      CHARACTER * ( * ) INPUT

*  Arguments Returned:
      CHARACTER * ( 3 ) DBNAME
      CHARACTER * ( * ) CATNAME

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*  Convert all CHI names to uppercase.
*
*       call chr_ucase(input)
*       if (input(1:3) .eq. 'BIN') then
*         dbname = 'BIN'
*         catname = input(4:)
*       elseif (input(1:3) .eq. 'FIT') then
*         dbname = 'FIT'
*         catname = input(4:)
*       elseif (input(1:3) .eq. 'CDF') then
*         dbname = 'CDF'
*         catname = input(4:)
*       elseif (input(1:3) .eq. 'REX') then
*         dbname = 'REX'
*         catname = input(4:)
*       else
*         dbname = 'HDS'
*         catname = input(4:)
*       endif
          dbname = 'HDS'
          catname = input
*
      END
