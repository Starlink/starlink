      SUBROUTINE ARY1_DSBD( BAD, IDCB, STATUS )
*+
*  Name:
*     ARY1_DSBD

*  Purpose:
*     Set the bad pixel flag for a data object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DSBD( BAD, IDCB, STATUS )

*  Description:
*     The routine sets the bad pixel flag to a specified value for a
*     data object which has an entry in the DCB.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Bad pixel flag value to be set.
*     IDCB = INTEGER (Given)
*        Index to the DCB entry for the data object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Ensure that the current value of the bad pixel flag is known in
*     the DCB.
*     -  Check whether the value to be set differs from the current
*     value. If not, then there is nothing to do.
*     -  Ensure that form information is available for the data object
*     in the DCB.
*     -  Handle each form of array in turn.
*     -  Process primitive and simple arrays together. If the array is
*     primitive, then it must first be converted to simple storage form
*     in order to set the bad pixel flag to .FALSE..  Check if the data
*     object is mapped for access. Report an error if it is.
*     -  Otherwise, perform the conversion.
*     -  If an error occurred during form conversion, then report
*     context information.
*     -  After conversion to simple form (if needed) ensure that a
*     logical BAD_PIXEL component is present and write the new value to
*     it. Then update the bad pixel flag information in the DCB.
*     -  If the form entry for the data object in the DCB is not
*     recognised, then report an error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-JUL-1989 (RFWS):
*        Original version.
*     12-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
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
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_BAD( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Bad pixel flag for the data object.
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Form of the data object.
*        DCB_KBAD( ARY__MXDCB ) = LOGICAL (Write)
*           Whether the bad pixel flag value in the DCB is valid.
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to data object.
*        DCB_NREAD( ARY__MXDCB ) = INTEGER (Read)
*           NUmber of current read mappings for the data object.
*        DCB_NWRIT( ARY__MXDCB ) = INTEGER (Read)
*           NUmber of current write mappings for the data object.

*  Arguments Given:
      LOGICAL BAD
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DUMMY( 1 )         ! Dummy dimension array

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that the current value of the data object's bad pixel flag is
*  known.
      CALL ARY1_DBAD( IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  There is nothing to do unless the current value differs from that
*  being set.
         IF ( DCB_BAD( IDCB ) .NEQV. BAD ) THEN

*  If a new value must be set, then ensure that form information is
*  available for the data object.
            CALL ARY1_DFRM( IDCB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Primitive and simple arrays.
*  ===========================
*  These are both processed here.
               IF ( ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) .OR.
     :              ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' ) ) THEN

*  If the array is primitive, then it must first be converted to
*  simple storage form in order to set the bad pixel flag to .FALSE..
*  Check if the data object is mapped for access. Report an error if it
*  is.
                  IF ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN
                     IF ( ( DCB_NWRIT( IDCB ) .NE. 0 ) .OR.
     :                    ( DCB_NREAD( IDCB ) .NE. 0 ) ) THEN
                        STATUS = ARY__ISMAP
                        CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                        CALL ERR_REP( 'ARY1_DSBD_MAP',
     :                  'The array ^ARRAY is mapped for access, ' //
     :                  'perhaps through another identifier ' //
     :                  '(possible programming error).', STATUS )

*  Otherwise, perform the conversion.
                     ELSE
                        CALL ARY1_DP2S( IDCB, STATUS )
                     END IF

*  If an error occurred during form conversion, then report context
*  information.
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_REP( 'ARY1_DSBD_CVT',
     :                  'Unable to perform implicit conversion from ' //
     :                  '''PRIMITIVE'' to ''SIMPLE'' array storage ' //
     :                  'form.', STATUS )
                     END IF
                  END IF

*  We can now deal with simple arrays alone. Ensure that a logical
*  BAD_PIXEL component is present.
                  DUMMY( 1 ) = 0
                  CALL CMP_MOD( DCB_LOC( IDCB ), 'BAD_PIXEL',
     :                          '_LOGICAL', 0, DUMMY, STATUS )

*  Enter the new value.
                  CALL CMP_PUT0L( DCB_LOC( IDCB ), 'BAD_PIXEL', BAD,
     :                            STATUS )

*  Note the new value (and whether it is valid) in the DCB.
                  DCB_BAD( IDCB ) = BAD
                  DCB_KBAD( IDCB ) = STATUS .EQ. SAI__OK

*  If the array form was not recognised, then report an error.
               ELSE
                  STATUS = ARY__FATIN
                  CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
                  CALL ERR_REP( 'ARY1_DSBD_FORM',
     :            'Unsupported array form ''^BADFORM'' found in ' //
     :            'Data Control Block (internal programming error).',
     :            STATUS )
               END IF
            END IF
         END IF
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DSBD', STATUS )

      END
