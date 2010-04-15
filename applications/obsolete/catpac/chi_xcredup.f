      SUBROUTINE
     : CHI_XCREDUP( INPUT, ESTNUMENTS, OUTPUT, STATUS )
*+
*  Name:
*     CHI_XCREDUP

*  Purpose:
*     Create a duplicate catalogue with no entries.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_XCREDUP( INPUT, ESTNUMENTS, OUTPUT, STATUS )
*
*  Description:
*     Create a duplicate catalogue with the same columns but no entries.

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
      CHARACTER * ( CHI__SZCNAME ) CNAMES( CHI__NUMCOLS )
      INTEGER NUMCOLS
      CHARACTER * ( CHI__SZCFMT ) CFORMATS( CHI__NUMCOLS )
      CHARACTER * ( 1 ) CTYPES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCUNIT ) CUNITS( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCCMT ) CCOMMENTS( CHI__NUMCOLS )
      LOGICAL CMDATAACC( CHI__NUMCOLS )
      LOGICAL CDATAACC( CHI__NUMCOLS )

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Get all the information about the fields.
*
      CALL CHI_GALLCD(INPUT, NUMCOLS, CNAMES, CFORMATS, CTYPES,
     :  CUNITS, CCOMMENTS, CMDATAACC, CDATAACC, STATUS)
*
*    Create a catalogue with no entries.
*
      CALL CHI_CRECAT(OUTPUT, ESTNUMENTS, NUMCOLS, CNAMES,
     :    CFORMATS, CUNITS, CCOMMENTS, STATUS)
*
      END
