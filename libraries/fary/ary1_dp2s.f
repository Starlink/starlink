      SUBROUTINE ARY1_DP2S( IDCB, STATUS )
*+
*  Name:
*     ARY1_DP2S

*  Purpose:
*     Convert a data object from primitive to simple form.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DP2S( IDCB, STATUS )

*  Description:
*     The routine converts a data object, identified by its DCB entry,
*     from primitive to simple form. The DCB entry is updated to
*     reflect the change.

*  Notes:
*     -  This routine does not create an ORIGIN component in the array
*     structure.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the DCB entry for the data object to be converted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Ensure that data type information and component locators are
*     available for the data object in the DCB.
*     -  Annul the non-imaginary data component locator.
*     -  Obtain a locator to the data object's parent structure.
*     -  Obtain the data object's name.
*     -  Generate a unique temporary component name and create a new
*     empty ARRAY object in the parent structure with this name. Obtain
*     a locator to it.
*     -  Move the original primitive array into the DATA component of
*     the new array and store the array locator in the DCB.
*     -  Rename the new object so it has its original name.
*     -  Obtain a locator to the non-imaginary data component and store
*     it in the DCB.
*     -  Note the array form is now 'SIMPLE' and note whether data type
*     and form information are available in the DCB.

*  Implementation Deficiencies:
*     -  This routine requires the data type of the HDS object to
*     change. Therefore, it cannot be used if the object is a top level
*     object.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-OCT-1989 (RFWS):
*        Original version.
*     10-OCT-1990 (RFWS):
*        Changed to call ARY1_PAREN as a temporary work around for
*        problems with DAT_PAREN.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_DLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Non-imaginary data component locator.
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Write)
*           Data object storage form.
*        DCB_KFRM( ARY__MXDCB ) = LOGICAL (Write)
*           Whether storage form information is available in the DCB.
*        DCB_KTYP( ARY__MXDCB ) = LOGICAL (Write)
*           Whether data type information is available in the DCB.
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Data object locator.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! New object locator
      CHARACTER * ( DAT__SZLOC ) LOCP ! Parent structure locator
      CHARACTER * ( DAT__SZNAM ) NAME ! Data object name
      CHARACTER * ( DAT__SZNAM ) TNAME ! Temporary component name
      INTEGER DUMMY( 1 )         ! Dummy dimension array

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that data type information and component locators are
*  available for the data object in the DCB.
      CALL ARY1_DTYP( IDCB, STATUS )

*  Annul the non-imaginary data component locator (for primitive
*  objects, the main data object locator will point to the same
*  object).
      CALL DAT_ANNUL( DCB_DLOC( IDCB ), STATUS )
      DCB_DLOC( IDCB ) = ARY__NOLOC

*  Obtain a locator to the parent structure which contains the data
*  object.
      LOCP = ARY__NOLOC
      CALL DAT_PAREN( DCB_LOC( IDCB ), LOCP, STATUS )

*  Obtain the data object name within this structure.
      CALL DAT_NAME( DCB_LOC( IDCB ), NAME, STATUS )

*  Generate a temporary component name which will not clash with any
*  existing components in the parent structure and create an empty
*  ARRAY component with this name.
      CALL ARY1_TCNAM( LOCP, TNAME, STATUS )
      DUMMY( 1 ) = 0
      CALL DAT_NEW( LOCP, TNAME, 'ARRAY', 0, DUMMY, STATUS )

*  Obtain a locator to the new component.
      LOC = ARY__NOLOC
      CALL DAT_FIND( LOCP, TNAME, LOC, STATUS )

*  Annul the parent structure locator.
      CALL DAT_ANNUL( LOCP, STATUS )
      LOCP = ARY__NOLOC

*  Move the original primitive array into the DATA component within
*  the new ARRAY structure. Store the locator to the resulting new
*  "simple" array in the DCB.
      CALL DAT_MOVE( DCB_LOC( IDCB ), LOC, 'DATA', STATUS )
      DCB_LOC( IDCB ) = LOC

*  Rename the new object, back to its original name.
      CALL DAT_RENAM( DCB_LOC( IDCB ), NAME, STATUS )

*  Obtain a locator to the non-imaginary data component and store it in
*  the DCB.
      CALL DAT_FIND( DCB_LOC( IDCB ), 'DATA', DCB_DLOC( IDCB ),
     :               STATUS )

*  Note the array form is now 'SIMPLE'.
      DCB_FRM( IDCB ) = 'SIMPLE'

*  Note whether data type and form information are now available in the
*  DCB.
      DCB_KTYP( IDCB ) = STATUS .EQ. SAI__OK
      DCB_KFRM( IDCB ) = STATUS .EQ. SAI__OK
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DP2S', STATUS )

      END
