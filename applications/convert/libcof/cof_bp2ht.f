      SUBROUTINE COF_BP2HT( BITPIX, TYPE, STATUS )
*+
*  Name:
*     COF_BP2HT

*  Purpose:
*     Converts FITS BITPIX into an HDS primitive data type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_BP2HT( BITPIX, TYPE, STATUS )

*  Description:
*     This converts a FITS type specified through a BITPIX value
*     into its equivalent HDS primitive data type.

*  Arguments:
*     BITPIX = INTEGER (Given)
*        The BITPIX code from a FITS header.
*     TYPE = CHARACTER * ( DAT__SZTYP ) (Returned)
*        The HDS primitive data type corresponding to the BITPIX.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The supported BITPIX type codes and their equivalent HDS types
*     are: 8, _UBYTE; 16, _WORD; 32, _INTEGER; -32, _REAL; and
*     -64, _DOUBLE.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 January 21 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER BITPIX

*  Arguments Returned:
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Simply test for each value in turn, and assign the appropriate
*  HDS primitive data type.
      IF ( BITPIX .EQ. 8 ) THEN
         TYPE = '_UBYTE'
         
      ELSE IF ( BITPIX .EQ. 16 ) THEN
         TYPE = '_WORD'

      ELSE IF ( BITPIX .EQ. 32 ) THEN
         TYPE = '_INTEGER'

      ELSE IF ( BITPIX .EQ. -32 ) THEN
         TYPE = '_REAL'

      ELSE IF ( BITPIX .EQ. -64 ) THEN
         TYPE = '_DOUBLE'

*  Report that there is no equivalent HDS primitive type to the BITPIX.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'BP', BITPIX )
         CALL ERR_REP( 'COF_BP2HT_TYPERR',
     :     'The BITPIX code ^BP does not have an HDS counterpart.',
     :     STATUS )
      END IF

      END
