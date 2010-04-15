      SUBROUTINE
     : CHP_REPOCD( INPUT, CNAME, CFORMAT,
     : CUNIT, CCOMMENT, CPREFDIS, CASSERT, CASSEXP, CDOMCHK, STATUS)
*+
*  Name:
*     CHP_REPOCD

*  Purpose:
*     REPlace One Columns Details.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_REPOCD( INPUT, CNAME, CFORMAT,
*      CUNIT, CCOMMENT, CPREFDIS, CASSERT, CASSEXP, CDOMCHK, STATUS)
*
*  Description:
*     Replace a columns details. A column can be given a new format (consistent
*     with the type), units, comment, preferred display flag, assert flag and
*     assert expression or domain checking flag.

*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the catalogue.
*     CNAME = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the column whose details are to be updated.
*     CFORMAT = CHARACTER * ( CHP__SZCFMT ) (Given)
*        New format for the column.
*     CUNIT = CHARACTER * ( CHP__SZCUNIT ) (Given)
*        New units for the column.
*     CCOMMENT = CHARACTER * ( CHP__SZCCMT ) (Given)
*        New comment for the column.
*     CPREFDIS = LOGICAL (Given)
*        New preferred display flag for the column.
*     CASSERT = LOGICAL (Given)
*        New assert flag for the column.
*     CASSEXP = CHARACTER * ( CHP__SZEXP ) (Given)
*        New assert expression for the column.
*     CDOMCHK = LOGICAL (Given)
*        New domain checking flag for the column.
*     STATUS = INTEGER (Given and Returned)
*        Global status.
*
*  Notes:
*     If the new column format is inconsistent with the column type a invalid
*     column format error will be reported.
*
*     If the column details can not be replaced an insufficient privilege to
*     update error will be reported.
*
*  Anticipated Errors:
*     CHP__CATNOTFND
*     CHP__IVLDCFMT
*     CHP__INSPRIVUP

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
      LOGICAL CPREFDIS
      LOGICAL CASSERT
      CHARACTER * ( * ) CASSEXP
      LOGICAL CDOMCHK

*  Arguments Returned:

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
        call chi_repocd(input, cname, cformat, cunit,
     :  ccomment, status)
*
      END
