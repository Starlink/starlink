      SUBROUTINE ARY1_DBAD( IDCB, STATUS )
*+
*  Name:
*     ARY1_DBAD

*  Purpose:
*     Ensure that bad pixel information is available for a data object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DBAD( IDCB, STATUS )

*  Description:
*     The routine ensures that information about whether an array may
*     contain bad pixels is available. It does nothing if this
*     information is already available in the DCB. Otherwise, it obtains
*     the information by inspecting the data object itself and enters
*     the information into the DCB. Only those checks needed to obtain
*     the bad pixel information are performed on the data object.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Do nothing if bad pixel information is already available.
*     -  Ensure that form information is available for the data object.
*     -  Handle each form of array separately.
*     -  For primitive arrays, set the bad pixel flag value to .TRUE..
*     -  For simple arrays, search for the BAD_PIXEL component
*     (supplying a default value of .TRUE. if it is absent).
*     -  Ensure the BAD_PIXEL component is of type '_LOGICAL' and is
*     scalar. If OK, then obtain its value, otherwise report an
*     appropriate error.
*     -  Report an error if invalid form information is found in the
*     DCB.
*     -  Note whether bad pixel information is now available in the DCB.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1989 (RFWS):
*        Original version.
*     16-AUG-1989 (RFWS):
*        Changed initialisation of locators to use global constant.
*     12-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Form of the data object.
*        DCB_BAD( ARY__MXDCB ) = LOGICAL (Write)
*           Whether the array may contain bad pixels.
*        DCB_KBAD( ARY__MXDCB = LOGICAL (Read and Write)
*           Whether bad pixel information is available.
*        DCB_LOC( ARY__MXDCB = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to data object.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( DAT__SZLOC ) LOCBAD ! Locator to BAD_PIXEL component
      CHARACTER * ( DAT__SZTYP ) TYPE ! HDS data type
      INTEGER DIMB( DAT__MXDIM ) ! BAD_PIXEL component dimensions
      INTEGER NDIMB              ! Number of BAD_PIXEL dimensions
      LOGICAL THERE              ! Whether an HDS component is present

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do nothing if bad pixel information is already available.
      IF ( .NOT. DCB_KBAD( IDCB ) ) THEN

*  Ensure that form information is available for the data object.
         CALL ARY1_DFRM( IDCB, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  Primitive arrays.
*  ================
         IF ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN
            DCB_BAD( IDCB ) = .TRUE.

*  Simple arrays.
*  =============
         ELSE IF ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' ) THEN

*  See if the BAD_PIXEL component is present and supply a default value
*  of .TRUE. if it is not.
            CALL DAT_THERE( DCB_LOC( IDCB ), 'BAD_PIXEL', THERE,
     :                      STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 9999
            IF ( .NOT. THERE ) THEN
               DCB_BAD( IDCB ) = .TRUE.

*  If the BAD_PIXEL component is present, then obtain its type and
*  shape.
            ELSE
               LOCBAD = ARY__NOLOC
               CALL DAT_FIND( DCB_LOC( IDCB ), 'BAD_PIXEL', LOCBAD,
     :                        STATUS )
               CALL DAT_TYPE( LOCBAD, TYPE, STATUS )
               CALL DAT_SHAPE( LOCBAD, DAT__MXDIM, DIMB, NDIMB,
     :                         STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Check it is '_LOGICAL' and report an error if it is not.
                  IF ( TYPE .NE. '_LOGICAL' ) THEN
                     STATUS = ARY__TYPIN
                     CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                     CALL MSG_SETC( 'BADTYPE', TYPE )
                     CALL ERR_REP( 'ARY1_DBAD_TYPE',
     :               'The BAD_PIXEL component in the array ' //
     :               'structure ^ARRAY has an invalid HDS type of ' //
     :               '''^BADTYPE''; it should be of type ' //
     :               '''_LOGICAL''.', STATUS )

*  Check it is scalar and report an error if it is not.
                  ELSE IF ( NDIMB .NE. 0 ) THEN
                     STATUS = ARY__DIMIN
                     CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                     CALL MSG_SETI( 'BADNDIM', NDIMB )
                     CALL ERR_REP( 'ARY1_DBAD_BDIM',
     :               'The BAD_PIXEL component in the array ' //
     :               'structure ^ARRAY is ^BADNDIM-dimensional; ' //
     :               'it should be a scalar.', STATUS )

*  Obtain the BAD_PIXEL value.
                  ELSE
                     CALL DAT_GET0L( LOCBAD, DCB_BAD( IDCB ), STATUS )
                  END IF
               END IF

*  Annul the locator to the BAD_PIXEL component.
               CALL DAT_ANNUL( LOCBAD, STATUS )
               LOCBAD = ARY__NOLOC
            END IF

*  If the form entry in the Data Control Block was not recognised, then
*  report an error.
         ELSE
            STATUS = ARY__FATIN
            CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
            CALL ERR_REP( 'ARY1_DBAD_FRM',
     :      'Unsupported array form ''^BADFORM'' found in Data ' //
     :      'Control Block (internal programming error).', STATUS )
         END IF

*  Note if bad pixel information is now available.
9999     CONTINUE
         DCB_KBAD( IDCB ) = STATUS .EQ. SAI__OK
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DBAD', STATUS )

      END
