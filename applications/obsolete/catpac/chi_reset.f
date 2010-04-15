      SUBROUTINE
     : CHI_RESET( INPUT, STATUS )
*+
*  Name:
*     CHI_RESET

*  Purpose:
*     RESET a catalogue back to first entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_RESET( INPUT, STATUS )

*  Description:
*     Reset a catalogue back to the start after getting data.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue to be reset.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     A CHI_RESET is not needed the first time data is retrieved from the
*     catalogue.

*  Anticipated Errors:
*     CHI__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1991 (ARW):
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

*  Global Variables:
*      INCLUDE 'CHI_CMN'   ! CHI comon area

*  Arguments Given:
      CHARACTER * ( CHI__SZNAME ) INPUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 3 ) DBNAME ! Database name
      CHARACTER * ( CHI__SZNAME ) CATNAME ! Catalogue name.
      INTEGER CD ! ADC Catalogue descriptor

*.
*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      call chi_splitname( input, dbname, catname, status)

      if (dbname .eq. 'HDS') then
        call chi_getcd(dbname, catname, .FALSE. ,cd ,status)
        call chi_hreset(cd, status)
*
*      elseif (dbname .eq. 'BIN') then
*        call chi_relcd(dbname, catname, status)
*
*      elseif (dbname .eq. 'CDF') then
*        call chi_getcd(dbname, catname, .FALSE. ,cd ,status)
*        call chi_creset(cd, status)
*
*      elseif (dbname .eq. 'FIT') then
*        call chi_getcd(dbname, catname, .FALSE. ,cd ,status)
*        call chi_freset(cd, status)
*
*      elseif (dbname .eq. 'REX') then
*        call chi_getcd(dbname, catname, .FALSE. ,cd ,status)
*        call chi_rreset(cd, status)
      endif
*
      END
