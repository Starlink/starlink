      SUBROUTINE
     : CHI_RENAME( INPUT, NEWNAME, STATUS )
*+
*  Name:
*     CHI_RENAME

*  Purpose:
*     RENAME a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_RENAME( INPUT, NEWNAME, STATUS )

*  Description:
*     Rename a catalogue.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue.
*     NEWNAME = CHARACTER * ( CHI__SZNAME ) (Given)
*        New name of the catalogue.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     The input catalogue is RESET to its first entry on exit from this routine.

*  Anticipated Errors:
*     CHI__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     1-JUL-1993 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'    ! Standard SAE constants
      INCLUDE 'CHI_PAR'    ! Standard CHI constants
      INCLUDE 'CHI_ERR'    ! Standard CHI errors

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) NEWNAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 3 ) DBNAME ! Database name
      CHARACTER * ( CHI__SZNAME ) CATNAME ! Catalogue name.
      CHARACTER * ( 3 ) NEWDBNAME ! New database name
      CHARACTER * ( CHI__SZNAME ) NEWCATNAME ! New catalogue name.

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Split the input name into database part and catname part.
*
      call chi_splitname(input, dbname, catname, status)
*
*    Split the newname into database part and catname part.
*
      call chi_splitname(newname, newdbname, newcatname, status)
*
*    If the input and output database system is the same then
*      call RENAME on the appropriate low level system
*    else
*      call the more basic CHI_XRENAME.
*
      if (dbname .eq. newdbname) then
*
        if (dbname .eq. 'HDS') then
*
*   HDS does not support RENAME so use XRENAME
*
           call chi_xrename(input, newname, status)
*
        elseif (dbname .eq. 'BIN') then
           call chi_xrename(input, newname, status)
        elseif (dbname .eq. 'BIN') then
           call chi_xrename(input, newname, status)
        elseif (dbname .eq. 'CDF') then
           call chi_xrename(input, newname, status)
        elseif (dbname .eq. 'FIT') then
           call chi_xrename(input, newname, status)
        endif
      else
        call chi_xrename(input, newname, status)
      endif
*
      END
