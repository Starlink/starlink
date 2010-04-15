       SUBROUTINE TBL_CLOSE( TBDSCR, STATUS )
*+
*  Name:
*     TBL_CLOSE


*  Purpose:
*     Close a table.


*  Language:
*     Starlink Fortran 77


*  Invocation:
*     CALL TBL_CLOSE( TBDSCR, STATUS )


*  Description:
*     This routine closes a container file,
*     annuls the table descriptor(TBDSCR)
*     and any other locators associated with
*     that file.


*  Arguments:
*     TBDSCR = CHARACTER (Given)
*        Similar to a pointer ,Only this is
*        a character which identifies the table
*        i.e. used to immediately locate the
*        position of the table.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Notes:
*     -  This routine will return without action
*        if on entry the STATUS is not SAI__OK.
*     -  The table is physically closed only if
*        there are no more descriptors associated
*        with it.

*  Algorithm:
*     -  Check STATUS , exit routine if not SAI__OK
*     -  Call a routine to find name of table
*     -  Report an error if status not SAI__OK.
*     -  Call a routine to annul all locators to that
*        table
*     -  Report an error if status not SAI__OK.
*     -  Close file.
*     -  Report an error if status not SAI__OK.


*  Authors:
*     Z: Z. Iqbal (SA3500)
*     {enter_new_authors_here}


*  History:
*     {enter_further_changes_here}


*  Bugs:
*     {note_any_bugs_here}


*-


*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
        INCLUDE 'DAT_ERR'
      INCLUDE 'CMP_ERR'
      INCLUDE 'TBL_PAR'

*  Arguments Given and Returned:
      CHARACTER*(DAT__SZLOC) TBDSCR


*  Status:
      INTEGER STATUS


*  Local Variables:
      CHARACTER*(DAT__SZNAM) GROUP


*.

        IF (STATUS .NE. SAI__OK) RETURN

        CALL HDS_CLOSE(TBDSCR,STATUS)

        RETURN
        END
