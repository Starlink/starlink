      SUBROUTINE
     : CHI_CRECAT( INPUT, ESTNUMENTS, NUMCOLS, CNAMES, CFORMATS,
     : CUNITS, CCOMMENTS, STATUS)
*+
*  Name:
*     CHI_CRECAT

*  Purpose:
*     CREate a new CATalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_CRECAT( INPUT, ESTNUMENTS, NUMCOLS, CNAMES, CFORMATS,
*     CUNITS, CCOMMENTS, STATUS)

*  Description:
*     Creates a new catalogue that contains no entries.
*     The CHI routines that write data into this catalogue will be more
*     efficient if you can provide a good estimate for the size of the
*     catalogue.  (The number of entries).
*     The column formats are checked. If an invalid format is found an error
*     is reported and the offending column and format are returned in CNAMES(1)
*     and  CFORMATS(1).
*
*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue being created.
*     ESTNUMENTS = INTEGER (Given)
*        Estimate for the number of entries that will be put into the catalogue.
*     NUMCOLS = INTEGER (Returned)
*        Number of columns in the catalogue.
*     CNAMES( CHI__NUMCOLS ) = CHARACTER * ( CHI__SZCNAME ) (Given and Returned)
*        Names of the columns in the catalogue.
*     CFORMATS( CHI__NUMCOLS ) = CHARACTER * ( CHI__SZCFMT ) (Given and Returned)
*        Formats of the columns in the catalogue.
*     CUNITS( CHI__NUMCOLS ) = CHARACTER * ( CHI__SZCUNIT ) (Given)
*        Units of the columns in the catalogue.
*     CCOMMENTS( CHI__NUMCOLS ) = CHARACTER * ( CHI__SZCCMT ) (Given)
*        Comments associated with the columns in the catalogue.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     CHI__IVLDCFMT

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
      INTEGER ESTNUMENTS
      INTEGER NUMCOLS
      CHARACTER * ( * ) CNAMES( * )
      CHARACTER * ( * ) CFORMATS( * )
      CHARACTER * ( * ) CUNITS( * )
      CHARACTER * ( * ) CCOMMENTS( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 3 ) DBNAME ! Database name
      CHARACTER * ( CHI__SZNAME ) CATNAME ! Catalogue name.
      INTEGER COUNT

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Split the input name into database part and catname part.
*
      call chi_splitname(input, dbname, catname, status)
*
*    Call CRECAT on the appropriate low level system
*
       if (dbname .eq. 'HDS') then
         do count = 1, numcols
         enddo
         call chi_hcrecat(catname, 100, numcols, cnames, cformats,
     :   cunits, ccomments, status)
*
*       elseif (dbname .eq. 'BIN') then
*         call chi_bnoent(catname, 100, numcols, cnames, cformats,
*     :   cunits, ccomments, status)
*
*       elseif (dbname .eq. 'CDF') then
*         call chi_ccrecat(catname, 100, numcols, cnames, cformats,
*     :   cunits, ccomments, status)
*
*       elseif (dbname .eq. 'FIT') then
*         call chi_fcrecat(catname, 100, numcols, cnames, cformats,
*     :   cunits, ccomments, status)
*
*       elseif (dbname .eq. 'REX') then
*         call chi_rcrecat(catname, 100, numcols, cnames, cformats,
*     :   cunits, ccomments, status)
*
*
      endif
*
      END
