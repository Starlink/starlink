      SUBROUTINE TBL_GETNAM( TBDSCR, CNAME, COLIDN,
     +                         STATUS )
*+
*  Name:
*     TBL_GETNAM


*  Purpose:
*     Get column identifier from column name.


*  Language:
*     Starlink Fortran 77


*  Invocation:
*     CALL TBL_GETNAM( TBDSCR, CNAME, COLIDN
*                            STATUS )


*  Description:
*     Return column identifier for column, based
*     on column name.This routine may be called
*     more than once for a given column name.
*     Use the column identifier for all subsequent
*     references to columns until the table is closed.


*  Arguments:
*     CNAME = CHARACTER (Given)
*        Column name for which identifier is required.
*     COLIDN = CHARACTER (Returned)
*        The locator(identifier) for the given CNAME.
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

*  Algorithm:
*     -  Check STATUS , exit routine if not SAI__OK
*     -  Call a routine to get column identifier.
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


*  Arguments Given and Returned:
      CHARACTER*(DAT__SZLOC) TBDSCR, COLIDN
      CHARACTER*(DAT__SZNAM) CNAME


*  Status:
      INTEGER STATUS


*  Local Variables:
       CHARACTER*(DAT__SZLOC) CLOC

*.

       IF (STATUS .NE. SAI__OK) RETURN

       CALL DAT_FIND( TBDSCR, 'COLUMNS', CLOC, STATUS)
       CALL DAT_FIND( CLOC, CNAME, COLIDN, STATUS)
       CALL DAT_ANNUL( CLOC, STATUS)

       END
