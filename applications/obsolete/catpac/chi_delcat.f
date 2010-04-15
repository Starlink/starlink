      SUBROUTINE
     : CHI_DELCAT( INPUT, STATUS )
*+
*  Name:
*     CHI_DELCAT

*  Purpose:
*     DELete a CATalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_DELCAT( INPUT, STATUS )
*
*  Description:
*     Delete a catalogue from the system. Catalogues that do not belong
*     to the user may be protected and will not be deleted.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue to be deleted.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

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
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHI_PAR'   ! Standard CHI constants
      INCLUDE 'CHI_ERR'   ! Standard CHI errors

*  Global Variables:

*  Arguments Given:
      CHARACTER * ( CHI__SZNAME ) INPUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 3 ) DBNAME ! Database name
      CHARACTER * ( CHI__SZNAME ) CATNAME ! Catalogue name.
      INTEGER CD ! Catalogue descriptor.

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Close the catalogue.
*
*      call chi_closecat(input, status)
*
*    Split the input name into database part and catname part.
*
      call chi_splitname(input, dbname, catname, status)
*
*    Call ADELCAT on the appropriate low level system
*
      if (dbname .eq. 'HDS') then
         call chi_relcd( dbname, catname, status)
         call chi_hdelcat(catname, status)
*      elseif (dbname .eq. 'BIN') then
*         call chi_relcd( dbname, catname, status)
*         call chi_bdelcat(catname, status)
*      elseif (dbname .eq. 'CDF') then
*         call chi_relcd( dbname, catname, status)
*         call chi_cdelcat(catname, status)
*      elseif (dbname .eq. 'FIT') then
*         call chi_relcd( dbname, catname, status)
*         call chi_fdelcat(catname, status)
*      elseif (dbname .eq. 'REX') then
*         call chi_relcd( dbname, catname, status)
*         call chi_rdelcat(catname, status)
*
      endif
*
      END
