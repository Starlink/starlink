      SUBROUTINE
     : CHI_REPOCD( INPUT, CNAME, CFORMAT,
     : CUNIT, CCOMMENT, STATUS)
*+
*  Name:
*     CHI_REPOCD

*  Purpose:
*     REPlace One Columns Details.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_REPOCD( INPUT, CNAME, CFORMAT,
*     CUNIT, CCOMMENT, STATUS )
*
*  Description:
*     Replace a columns details. The name and type of a column can
*     never change but a column can be given a new format (consistent
*     with the type), units and comment.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue.
*     CNAME = CHARACTER * ( CHI__SZCNAME ) (Given)
*        Name of the column whose details are to be updated.
*     CFORMAT = CHARACTER * ( CHI__SZCFMT ) (Given)
*        New format for the column.
*     CUNIT = CHARACTER * ( CHI__SZCUNIT ) (Given)
*        New units for the column.
*     CCOMMENT = CHARACTER * ( CHI__SZCCMT ) (Given)
*        New comment for the column.
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
      INCLUDE 'SAE_PAR'    ! Standard SAE constants
      INCLUDE 'CHI_PAR'    ! Standard CHI constants
      INCLUDE 'CHI_ERR'    ! Standard CHI errors

*  Arguments Given:
      CHARACTER * ( CHI__SZNAME ) INPUT
      CHARACTER * ( * ) CNAME
      CHARACTER * ( * ) CFORMAT
      CHARACTER * ( * ) CUNIT
      CHARACTER * ( * ) CCOMMENT

*  Arguments Returned:

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 3 ) DBNAME ! Database name
      CHARACTER * ( CHI__SZNAME ) CATNAME ! Catalogue name.

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Split the input name into database part and catname part.
*
      call chi_splitname(input, dbname, catname, status)
*
      if (dbname .eq. 'HDS') then
        call chi_xrepocd(input, cname, cformat, cunit,
     :  ccomment, status)
      elseif (dbname .eq. 'BIN') then
        call chi_xrepocd(input, cname, cformat, cunit,
     :  ccomment, status)
      elseif (dbname .eq. 'CDF') then
        call chi_xrepocd(input, cname, cformat, cunit,
     :  ccomment, status)
      elseif (dbname .eq. 'FIT') then
        call chi_xrepocd(input, cname, cformat, cunit,
     :  ccomment, status)
      endif
*
      END
