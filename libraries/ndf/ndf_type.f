      SUBROUTINE NDF_TYPE( INDF, COMP, TYPE, STATUS )
*+
*  Name:
*     NDF_TYPE

*  Purpose:
*     Obtain the numeric type of an NDF array component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_TYPE( INDF, COMP, TYPE, STATUS )

*  Description:
*     The routine returns the numeric type of one of the array
*     components of an NDF as an upper-case character string (e.g.
*     '_REAL').

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component whose type is required:
*        'DATA', 'QUALITY' or 'VARIANCE'.
*     TYPE = CHARACTER * ( * ) (Returned)
*        Numeric type of the component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be supplied
*     to this routine. In this case the result returned will be the
*     lowest precision numeric type to which all the specified
*     components can be converted without unnecessary loss of
*     information.
*     -  The value returned for the QUALITY component is always
*     '_UBYTE'.
*     -  The symbolic constant NDF__SZTYP may be used for declaring the
*     length of a character variable which is to hold the numeric type
*     of an NDF array component. This constant is defined in the
*     include file NDF_PAR.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Obtain the integer type code of the result.
*     -  Convert the type code to a type string and return it as the
*     result.
*     -  If an error has occurred, then report context information.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-JAN-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP

*  Arguments Returned:
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( NDF__SZTYP ) DATYP( NDF__TYPUB : NDF__TYPD ) ! Data
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER ITYPE              ! Integer type code of result

*  Local Data:
      DATA DATYP( NDF__TYPB ) / '_BYTE' / ! Data type code translations
      DATA DATYP( NDF__TYPD ) / '_DOUBLE' /
      DATA DATYP( NDF__TYPI ) / '_INTEGER' /
      DATA DATYP( NDF__TYPR ) / '_REAL' /
      DATA DATYP( NDF__TYPUB ) / '_UBYTE' /
      DATA DATYP( NDF__TYPUW ) / '_UWORD' /
      DATA DATYP( NDF__TYPW ) / '_WORD' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Determine the integer type code of the result.
      CALL NDF1_TYP( IACB, COMP, ITYPE, STATUS )

*  Translate the type code into a string and return it.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL NDF1_CCPY( DATYP( ITYPE ), TYPE, STATUS )
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_TYPE_ERR',
     :   'NDF_TYPE: Error obtaining the numeric type of an NDF ' //
     :   'array component.', STATUS )
         CALL NDF1_TRACE( 'NDF_TYPE', STATUS )
      END IF

      END
