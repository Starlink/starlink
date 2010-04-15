      SUBROUTINE
     : CHP_SORT( INPUT, SORTCOLS, SORTDIR, STATUS )
*+
*  Name:
*     CHP_SORT

*  Purpose:
*     SORT a catalogue on given columns.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_SORT( INPUT, SORTCOLS, SORTDIR, STATUS )
*
*  Description:
*     Sort a catalogue on given columns.
*     The order of column names in the SORTCOLS array is significant.
*     SORTCOLS(1) must contain the primary column.
*     SORTCOL(2) and SORTCOLS(3) contain the secondary and tertiary columns.
*     Spaces in either the secondary or tertiary position simply indicates
*     that there should be no secondary or tertiary ordering.
*     The direction of the sort for each column in given in the corresponding
*     element of the SORTDIR array. TRUE for ascending.

*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Catalogue name.
*     SORTCOLS( 3 ) = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Names of the sort columns.
*     SORTDIR( 3 ) = LOGICAL (Given)
*        Direction of sort for each column (TRUE for ascending).
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     If a column name does not exist in the catalogue a column not found
*     error will be reported.
*
*     It is possible to sort a catalogue on a virtual column but not on array
*     elements.
*
*     Non standard format columns will be sorted on their underlying values.
*
*     The catalogue is RESET to its first entry on exit from this routine.
*
*     If the catalogue can not be sorted an insufficient privilege to update
*     error will be reported.
*
*
*  Anticipated Errors:
*     CHP__CATNOTFND
*     CHP__COLNOTFND
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
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHI_PAR'   ! Standard CHI constants
      INCLUDE 'CHI_ERR'   ! Standard CHI errors

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) SORTCOLS( 3 )
      LOGICAL SORTDIR( 3 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

*.
*
*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      call chi_sort(input, sortcols, sortdir, status)
*
      END
