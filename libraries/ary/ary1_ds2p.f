      SUBROUTINE ARY1_DS2P( IDCB, STATUS )
*+
*  Name:
*     ARY1_DS2P

*  Purpose:
*     Convert a data object from simple to primitive form.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DS2P( IDCB, STATUS )

*  Description:
*     The routine converts a data object, identified by its DCB entry,
*     from simple to primitive form. The DCB entry is updated to
*     reflect the change.

*  Notes:
*     -  This routine will fail if (a) the array holds complex values,
*     or (b) its bad-pixel flag value is .FALSE., or (c) all its lower
*     bounds are not equal to 1.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the DCB entry for the data object to be converted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Ensure that data type information and component locators are
*     available for the data object in the DCB.
*     -  Ensure that bad-pixel flag and bounds information are also
*     available.
*     -  Loop to check that the lower bound of each array dimension is
*     equal to 1.
*     -  If any lower bound is not equal to 1, then report an error.
*     -  Similarly, report an error if the array's bad-pixel flag is
*     set to .FALSE..
*     -  Also report an error if the array holds complex values.
*     -  Determine the name of the array data object.
*     -  Obtain a locator to its parent structure.
*     -  Generate a temporary component name for use in the parent
*     structure and move the non-imaginary data component into the
*     structure under this name.
*     -  Annul the main data object locator and erase the remaining
*     simple array structure (minus its data array).
*     -  Obtain a locator for the new array.
*     -  Annul the parent structure locator.
*     -  Rename the data array (to have the original array name) and
*     clone a new non-imaginary component locator for storage in the
*     DCB.
*     -  Note the array form is now 'PRIMITIVE'.
*     -  Note whether data type and form information are now available
*     in the DCB.

*  Implementation Deficiencies:
*     -  This routine requires the data type of the HDS object to
*     change. Therefore, it cannot be used if the object is a top level
*     object.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-NOV-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

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
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_DLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
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
      CHARACTER * ( DAT__SZLOC ) LOCP ! Parent structure locator
      CHARACTER * ( DAT__SZNAM ) NAME ! Data object name
      CHARACTER * ( DAT__SZNAM ) TNAME ! Temporary component name
      INTEGER I                  ! Loop counter for dimensions
      LOGICAL OK                 ! Are array bounds OK?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that data type information and component locators are
*  available for the data object in the DCB.
      CALL ARY1_DTYP( IDCB, STATUS )

*  Ensure that bad-pixel flag and bounds information are also
*  available.
      CALL ARY1_DBAD( IDCB, STATUS )
      CALL ARY1_DBND( IDCB, STATUS )

*  Loop to check that the lower bound of each array dimension is equal
*  to 1.
      IF ( STATUS .EQ. SAI__OK ) THEN
         OK = .TRUE.
         DO 1 I = 1, DCB_NDIM( IDCB )
            IF ( DCB_LBND( I, IDCB ) .NE. 1 ) THEN
               OK = .FALSE.
               GO TO 2
            END IF
 1       CONTINUE
 2       CONTINUE

*  If any lower bound is not equal to 1, then report an error.
         IF ( .NOT. OK ) THEN
            STATUS = ARY__FRMCV
            CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
            CALL ERR_REP( 'ARY1_DS2P_BOUND',
     :                    'Unable to convert the array ^ARRAY from ' //
     :                    'simple to primitive storage form; the ' //
     :                    'lower bounds are not all equal to 1 ' //
     :                    '(possible programming error).', STATUS )

*  Similarly, report an error if the array's bad-pixel flag is set to
*  .FALSE..
         ELSE IF ( .NOT. DCB_BAD( IDCB ) ) THEN
            STATUS = ARY__FRMCV
            CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
            CALL ERR_REP( 'ARY1_DS2P_BAD',
     :                    'Unable to convert the array ^ARRAY from ' //
     :                    'simple to primitive storage form; the ' //
     :                    'current bad-pixel flag value is .FALSE. ' //
     :                    '(possible programming error).', STATUS )

*  Also report an error if the array holds complex values.
         ELSE IF ( DCB_CPX( IDCB ) ) THEN
            STATUS = ARY__FRMCV
            CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
            CALL ERR_REP( 'ARY1_DS2P_CPX',
     :                    'Unable to convert the array ^ARRAY from ' //
     :                    'simple to primitive storage form; the ' //
     :                    'array holds complex values (possible ' //
     :                    'programming error).', STATUS )

*  Determine the name of the array data object.
         ELSE
            CALL DAT_NAME( DCB_LOC( IDCB ), NAME, STATUS )

*  Obtain a locator to its parent structure.
            LOCP = ARY__NOLOC
            CALL DAT_PAREN( DCB_LOC( IDCB ), LOCP, STATUS )

*  Generate a temporary component name for use in the parent structure
*  and move the non-imaginary data component into the structure under
*  this name.
            CALL ARY1_TCNAM( LOCP, TNAME, STATUS )
            CALL DAT_MOVE( DCB_DLOC( IDCB ), LOCP, TNAME, STATUS )
            DCB_DLOC( IDCB ) = ARY__NOLOC

*  Annul the main data object locator and erase the remaining simple
*  array structure (minus its data array).
            CALL DAT_ANNUL( DCB_LOC( IDCB ), STATUS )
            DCB_LOC( IDCB ) = ARY__NOLOC
            CALL DAT_ERASE( LOCP, NAME, STATUS )

*  Obtain a locator for the new array.
            CALL DAT_FIND( LOCP, TNAME, DCB_LOC( IDCB ), STATUS )

*  Annul the parent structure locator.
            CALL DAT_ANNUL( LOCP, STATUS )
            LOCP = ARY__NOLOC

*  Rename the data array (to have the original array name) and clone a
*  new non-imaginary component locator for storage in the DCB.
            CALL DAT_RENAM( DCB_LOC( IDCB ), NAME, STATUS )
            CALL DAT_CLONE( DCB_LOC( IDCB ), DCB_DLOC( IDCB ), STATUS )

*  Note the array form is now 'PRIMITIVE'.
            DCB_FRM( IDCB ) = 'PRIMITIVE'

*  Note whether data type and form information are now available in the
*  DCB.
            DCB_KTYP( IDCB ) = STATUS .EQ. SAI__OK
            DCB_KFRM( IDCB ) = STATUS .EQ. SAI__OK
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DS2P', STATUS )

      END
