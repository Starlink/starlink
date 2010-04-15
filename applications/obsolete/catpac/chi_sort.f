      SUBROUTINE
     : CHI_SORT( INPUT, SORTCOLS, SORTDIR, STATUS )
*+
*  Name:
*     CHI_SORT

*  Purpose:
*     SORT a catalogue on given columns.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_SORT( INPUT, SORTCOLS, SORTDIR, STATUS )
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
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Catalogue name.
*     SORTCOLS( 3 ) = CHARACTER * ( CHI__SZCNAME ) (Given)
*        Names of the sort columns.
*     SORTDIR( 3 ) = LOGICAL (Given)
*        Direction of sort for each column (TRUE for ascending).
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     If a column name does not exist in the catalogue an error will be
*     reported.
*
*     The cataalogue is RESET to its first entry on exit from this routine.
*  Anticipated Errors:
*     CHI__CATNOTFND
*     CHI__COLNOTFND

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
      CHARACTER * ( 3 ) DBNAME ! Database name
      CHARACTER * ( CHI__SZNAME ) CATNAME ! Catalogue name.

*.
*
*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Split the input name into database part and catname part.
*
      call chi_splitname(input, dbname, catname, status)
*
*    Call SORT on the appropriate low level system
*
      if (dbname .eq. 'HDS') then
*
*    HDS does not support SORT so use the general XSORT
*
         call chi_xsort(input, sortcols, sortdir, status)
*
*      elseif (dbname .eq. 'BIN') then
*
*    BIN does not support SORT so use the general XSORT
*
*         call chi_xsort(input, sortcols, sortdir, status)
      endif
*
      END
