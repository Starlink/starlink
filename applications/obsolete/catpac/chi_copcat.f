      SUBROUTINE
     : CHI_COPCAT( INPUT, OUTPUT, STATUS )
*+
*  Name:
*     CHI_COPCAT

*  Purpose:
*     COPy a CATalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_COPCAT( INPUT, OUTPUT, STATUS )
*
*  Description:
*     Create a copy of a catalogue.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue.
*     OUTPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the new catalogue.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     Both the input and output catalogues are RESET to their first entries
*     on exit from this routine.

*  Anticipated Errors:
*     CHI__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     26-JUL-1993 (ARW):
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

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) OUTPUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 3 ) DBNAME ! Database name
      CHARACTER * ( CHI__SZNAME ) CATNAME ! Catalogue name.
      CHARACTER * ( 3 ) OUTDBNAME ! Output database name
      CHARACTER * ( CHI__SZNAME ) OUTCATNAME ! Output catalogue name.

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Split the input name into database part and catname part.
*
      call chi_splitname(input, dbname, catname, status)
*
*    Split the output name into database part and catname part.
*
      call chi_splitname(output, outdbname, outcatname, status)
*
*    If the input and output database system is the same then
*      call COPCAT on the appropriate low level system
*    else
*      call the more basic CHI_XCOPCAT.
*
      if (dbname .eq. outdbname) then
*
        if (dbname .eq. 'HDS') then
*
*    HDS does not support COPCAT use XCOPCAT.
*
           call chi_relcd(dbname, catname, status)
           call chi_xcopcat(input, output, status)
           call chi_relcd(outdbname, outcatname, status)
*
        elseif (dbname .eq. 'BIN') then
*
*    BIN does not support COPCAT use XCOPCAT.
*
           call chi_relcd(dbname, catname, status)
           call chi_xcopcat(input, output, status)
           call chi_relcd(outdbname, outcatname, status)
*
        elseif (dbname .eq. 'CDF') then
*
*    CDF does not support COPCAT use XCOPCAT.
*
           call chi_relcd(dbname, catname, status)
           call chi_xcopcat(input, output, status)
           call chi_relcd(outdbname, outcatname, status)
*
        elseif (dbname .eq. 'FIT') then
*
*    FIT does not support COPCAT use XCOPCAT.
*
           call chi_relcd(dbname, catname, status)
           call chi_xcopcat(input, output, status)
           call chi_relcd(outdbname, outcatname, status)
*
        endif
      else
         call chi_xcopcat(input, output, status)
      endif
*
      END
