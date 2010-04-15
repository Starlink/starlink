      SUBROUTINE
     : CHI_DCOLS( INPUT, NUMCOLS, CNAMES, STATUS )
*+
*  Name:
*     CHI_DCOLS

*  Purpose:
*     DELete given COLumns from a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_DCOLS( INPUT, NUMCOLS, CNAMES, STATUS )
*
*  Description:
*     Delete given columns from a catalogue. If the column does not exist in
*     the catalogue no error is reported.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Catalogue name.
*     NUMCOLS = INTEGER (Given)
*        Number of columns to be deleted.
*     CNAMES( CHI__NUMCOLS ) = CHARACTER * ( CHI__SZCNAME ) (Given)
*        Name of the column to be deleted.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     The catalogue is RESET to its first entry on exit from this routine.

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

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      INTEGER NUMCOLS
      CHARACTER * ( CHI__SZCNAME ) CNAMES( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 3 ) DBNAME ! Database name
      CHARACTER * ( CHI__SZNAME ) CATNAME ! Catalogue name.
      INTEGER ICOUNT
      LOGICAL OPENFLG
      LOGICAL NOTFOUND
*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Split the input name into database part and catname part.
*
      call chi_splitname(input, dbname, catname, status)
*
*    Call DCOLS on the appropriate low level system
*
*
      if (dbname .eq. 'HDS') then
         call chi_xdcols(input, numcols, cnames, status)
*      elseif (dbname .eq. 'BIN') then
*         call chi_xdcols(input, numcols, cnames, status)
*      elseif (dbname .eq. 'CDF') then
*         call chi_xdcols(input, numcols, cnames, status)
*      elseif (dbname .eq. 'FIT') then
*         call chi_xdcols(input, numcols, cnames, status)
      endif
*
      END
