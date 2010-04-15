      SUBROUTINE
     : CHI_CREDUP( INPUT, ESTNUMENTS, OUTPUT, STATUS )
*+
*  Name:
*     CHI_CREDUP

*  Purpose:
*     CREate a DUPlicate catalogue with no entries.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_CREDUP( INPUT, ESTNUMENTS, OUTPUT, STATUS )
*
*  Description:
*     Create a duplicate catalogue with the same columns but no entries.
*     The CHI routines that write data into this catalogue will be more
*     efficient if you can provide a good estimate for the size of the
*     catalogue. (The number of entries).

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue to be duplicated.
*     OUTPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the duplicate catalogue to be created.
*     ESTNUMENTS = INTEGER (Given)
*        Estimate for the number of entries that will be put into the catalogue.
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

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) OUTPUT
      INTEGER ESTNUMENTS

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
*      call CREDUP on the appropriate low level system
*    else
*      call the more basic CHI_XCREDUP.
*
      if (dbname .eq. outdbname) then
*
        if (dbname .eq. 'HDS') then
*
*    HDS does not support CREDUP use XCREDUP.
*
           call chi_xcredup(input, estnuments, output, status)
*
        elseif (dbname .eq. 'BIN') then
*
*    BIN does not support CREDUP use XCREDUP.
*
           call chi_xcredup(input, estnuments, output, status)
*
        elseif (dbname .eq. 'CDF') then
*
*    CDF does not support CREDUP use XCREDUP.
*
           call chi_xcredup(input, estnuments, output, status)
*
        elseif (dbname .eq. 'FIT') then
*
*    FIT does not support CREDUP use XCREDUP.
*
           call chi_xcredup(input, estnuments, output, status)
*
        elseif (dbname .eq. 'REX') then
*
*    REXEC does not support CREDUP use XCREDUP.
*
           call chi_xcredup(input, estnuments, output, status)
*
        endif
      else
         call chi_xcredup(input, estnuments, output, status)
      endif
*
      END
