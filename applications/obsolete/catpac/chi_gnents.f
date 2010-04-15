      SUBROUTINE
     : CHI_GNENTS( INPUT, NUMENTS, STATUS )
*+
*  Name:
*     CHI_GNENTS

*  Purpose:
*     Get the Number of ENTries in a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_GNENTS( INPUT, NUMENTS, STATUS )
*
*  Description:
*     Get the number of entries in a catalogue.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue.
*     NUMENTS = INTEGER (Returned)
*        Number of entries in the catalogue.
*     STATUS = INTEGER
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
      CHARACTER * ( CHI__SZNAME ) INPUT

*  Arguments Returned:
      INTEGER NUMENTS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 3 ) DBNAME ! Database name
      CHARACTER * ( CHI__SZNAME ) CATNAME ! Catalogue name.
      INTEGER CD ! Catalogue Descriptor

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Split the input name into database part and catname part.
*
      call chi_splitname(input, dbname, catname, status)
*
*    Call GNENTS on the appropriate low level system
*
      if (dbname .eq. 'HDS') then
         call chi_getcd(dbname,catname, .FALSE., cd, status)
         call chi_hgnents(cd, numents, status)
*
*      elseif (dbname .eq. 'BIN') then
*         call chi_getcd(dbname,catname, .FALSE., cd, status)
*         call chi_bgnents(cd, numents, status)
*
*      elseif (dbname .eq. 'CDF') then
*         call chi_getcd(dbname,catname, .FALSE., cd, status)
*         call chi_cgnents(cd, numents, status)
*
*      elseif (dbname .eq. 'FIT') then
*         call chi_getcd(dbname,catname, .FALSE., cd, status)
*         call chi_fgnents(cd, numents, status)
*
*      elseif (dbname .eq. 'REX') then
*         call chi_getcd(dbname,catname, .FALSE., cd, status)
*         call chi_rgnents(cd, numents, status)
*
      endif
*
      END
