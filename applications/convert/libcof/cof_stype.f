      SUBROUTINE COF_STYPE( NDF, COMP, TYPE, BITPIX, ITYPE, STATUS )
*+
*  Name:
*     COF_STYPE

*  Purpose:
*     Sets NDF array types from the FITS headers and preferred data
*     type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_STYPE( NDF, COMP, TYPE, BITPIX, ITYPE, STATUS )

*  Description:
*     This sets the data type for an NDF array component.  A preferred
*     type value is used, unless the component is the QUALITY.  The
*     type may also be undefined, in which case the supplied FITS
*     BITPIX defines the data type.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF to be converted from the FITS file.
*     COMP = CHARACTER * ( * ) (Given)
*        The array component.  It must be 'Data' or 'Variance'.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the component.  If this is null, the BITPIX
*        defines the data type.
*     BITPIX = INTEGER (Given)
*        The FITS BITPIX value to define the data type when TYPE = ' '.
*        Thus for example -32 would generate _REAL and 16 makes _WORD.
*     ITYPE = CHARACTER * ( DAT__SZTYP ) (Returned)
*        The data type selected for the component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 January 19 (MJC):
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
      INTEGER NDF
      CHARACTER * ( * ) COMP
      CHARACTER * ( * ) TYPE
      INTEGER BITPIX

*  Arguments Returned:
      CHARACTER * ( * ) ITYPE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the data type of the output array component.  QUALITY
*  arrays must be unsigned byte.
      IF ( COMP .EQ. 'QUALITY' ) THEN
         ITYPE = '_UBYTE'

*  A null supplied type means use the data type of the FITS file.
      ELSE IF ( TYPE .EQ. ' ' ) THEN
         CALL COF_BP2HT( BITPIX, ITYPE, STATUS )

*  Just use the supplied data type.
      ELSE
         ITYPE = TYPE
      END IF

*  Change the type of the component.
      CALL NDF_STYPE( ITYPE, NDF, COMP, STATUS )
 
      END
