      SUBROUTINE ARY1_DDIAG( IDCB )
*+
*  Name:
*     ARY1_DDIAG

*  Purpose:
*     Diagnostic routine for DCB entries.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DDIAG( IDCB )

*  Description:
*     The routine displays diagnostic information about the specified
*     DCB entry through the ADAM parameter system.

*  Arguments:
*     IDCB = INTEGER (Given)
*        The DCB entry to be inspected.

*  Notes:
*     -  This routine does not perform any error checking or reporting.

*  Algorithm:
*     -  Format and display information about each DCB item in turn.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-AUG-1989 (RFWS):
*        Original version.
*     18-AUG-1989 (RFWS):
*        Removed duplicate bounds output and simplified its layout.
*        Added output of numeric data type.
*     22-AUG-1989 (RFWS):
*        Added EXTERNAL statement to ensure ARY1_INIT is linked.
*     23-AUG-1989 (RFWS):
*        Changed the layout of the dimension bounds and pixel shift
*        information.
*     1-SEP-1989 (RFWS):
*        Fixed bug in constructing messages about invalid locators.
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
*        This routine accesses all the global variables in the DCB using
*        READ access.

*  Arguments Given:
      INTEGER IDCB

*  External References:
      EXTERNAL ARY1_INIT         ! Initialise common blocks

*  Local Variables:
      CHARACTER * ( 60 ) BUF     ! Output buffer for dimension bounds
      INTEGER I                  ! Loop counter for dimensions
      INTEGER STATUS             ! Local status variable
      LOGICAL VALID              ! Whether locator is valid

*.

      STATUS = SAI__OK

*  Show which DCB entry the following information is for.
      CALL MSG_SETI( 'IDCB', IDCB )
      CALL MSG_OUT( ' ', 'Data Control Block entry number ^IDCB',
     :              STATUS )

*  Indicate if the entry number is not valid.
      IF ( ( IDCB .LT. 1 ) .OR. ( IDCB .GT. ARY__MXDCB ) ) THEN
         CALL MSG_OUT( ' ', 'This DCB entry number is invalid.',
     :                 STATUS )

*  Indicate if the DCB slot is not marked as in use. If this is so, then
*  there is nothing more to do.
      ELSE IF ( .NOT. DCB_USED( IDCB ) ) THEN
         CALL MSG_OUT( ' ', 'Entry is not in use.', STATUS )
      ELSE

*  Display the data object reference count and the number of read and
*  write mappings.
         CALL MSG_SETI( 'REFCT', DCB_REFCT( IDCB ) )
         CALL MSG_OUT( ' ', 'Reference count = ^REFCT', STATUS )
         CALL MSG_SETI( 'NREAD', DCB_NREAD( IDCB ) )
         CALL MSG_OUT( ' ', 'Number of READ mappings = ^NREAD', STATUS )
         CALL MSG_SETI( 'NWRIT', DCB_NWRIT( IDCB ) )
         CALL MSG_OUT( ' ', 'Number of WRITE mappings = ^NWRIT',
     :                 STATUS )

*  Display the array data object name, if obtainable. Otherwise show the
*  locator character value.
         CALL DAT_VALID( DCB_LOC( IDCB ), VALID, STATUS )
         IF ( VALID ) THEN
            CALL DAT_MSG( 'OBJ', DCB_LOC( IDCB ) )
            CALL MSG_OUT( ' ', 'Array name is ''^OBJ''.', STATUS )
         ELSE
            CALL MSG_SETC( 'LOC', DCB_LOC( IDCB ) )
            CALL MSG_OUT( ' ', 'Array locator is invalid: ''^LOC''.',
     :                    STATUS )
         END IF

*  Display the array form, if known.
         IF ( .NOT. DCB_KFRM( IDCB ) ) THEN
            CALL MSG_OUT( ' ', 'Array form is unknown.', STATUS )
         ELSE
            CALL MSG_SETC( 'FORM', DCB_FRM( IDCB ) )
            CALL MSG_OUT( ' ', 'Array form is ''^FORM''.', STATUS )
         END IF

*  Say if the data type information is not known.
         IF ( .NOT. DCB_KTYP( IDCB ) ) THEN
            CALL MSG_OUT( ' ', 'Data type and component ' //
     :                    'information is not available.', STATUS )
         ELSE

*  Disthay the numeric data type.
            CALL MSG_SETC( 'TYPE', DCB_TYP( IDCB ) )
            CALL MSG_OUT( ' ', 'Numeric data type is ''^TYPE''.',
     :                    STATUS )

*  Display the non-imaginary data component name, or its locator if not
*  valid.
            CALL DAT_VALID( DCB_DLOC( IDCB ), VALID, STATUS )
            IF ( VALID ) THEN
               CALL DAT_MSG( 'DATA', DCB_DLOC( IDCB ) )
               CALL MSG_OUT( ' ', 'Non-imaginary data component is ' //
     :                       '''^DATA''.', STATUS )
            ELSE
               CALL MSG_SETC( 'DLOC', DCB_DLOC( IDCB ) )
               CALL MSG_OUT( ' ', 'Non-imaginary data locator is ' //
     :                       'invalid: ''^DLOC''.', STATUS )
            END IF

*  Say if the array is non-complex, so it doesn't have an imaginary
*  component.
            IF ( .NOT. DCB_CPX( IDCB ) ) THEN
               CALL MSG_OUT( ' ', 'Array is not complex.', STATUS )
            ELSE

*  Display the imaginary component name or its locator if not valid.
               CALL DAT_VALID( DCB_ILOC( IDCB ), VALID, STATUS )
               IF ( VALID ) THEN
                  CALL DAT_MSG( 'I_DATA', DCB_ILOC( IDCB ) )
                  CALL MSG_OUT( ' ', 'Imaginary data component is ' //
     :                          '''^I_DATA''.', STATUS )
               ELSE
                  CALL MSG_SETC( 'ILOC', DCB_ILOC( IDCB ) )
                  CALL MSG_OUT( ' ', 'Imaginary data locator is ' //
     :                          'invalid: ''^ILOC''.', STATUS )
               END IF
            END IF
         END IF

*  Display the access mode, if known.
         IF ( .NOT. DCB_KMOD( IDCB ) ) THEN
            CALL MSG_OUT( ' ', 'Access mode is unknown.', STATUS )
         ELSE
            CALL MSG_SETC( 'MOD', DCB_MOD( IDCB ) )
            CALL MSG_OUT( ' ', 'Access mode is ''^MOD''.', STATUS )
         END IF

*  Display the array state, if known.
         IF ( .NOT. DCB_KSTA( IDCB ) ) THEN
            CALL MSG_OUT( ' ', 'Array state is unknown.', STATUS )
         ELSE
            CALL MSG_SETL( 'STA', DCB_STA( IDCB ) )
            CALL MSG_OUT( ' ', 'Array state is ''^STA''.', STATUS )
         END IF

*  Display the disposal mode.
         CALL MSG_SETC( 'DSP', DCB_DSP( IDCB ) )
         CALL MSG_OUT( ' ', 'Disposal mode is ''^DSP''.', STATUS )

*  Display the bad pixel flag value, if known.
         IF ( .NOT. DCB_KBAD( IDCB ) ) THEN
            CALL MSG_OUT( ' ', 'Bad pixel flag value is unknown.',
     :                    STATUS )
         ELSE
            CALL MSG_SETL( 'BAD', DCB_BAD( IDCB ) )
            CALL MSG_OUT( ' ', 'Bad pixel flag value is ''^BAD''.',
     :                    STATUS )
         END IF

*  Say if the array bounds information is not available.
         IF ( .NOT. DCB_KBND( IDCB ) ) THEN
            CALL MSG_OUT( ' ', 'Array bounds are unknown.', STATUS )
         ELSE

*  Display the number of array dimensions, followed by a list of the
*  dimension bounds and their accumulated pixel shifts. Those bounds
*  lying outside the array dimensionality are parenthesised.
            CALL MSG_SETI( 'NDIM', DCB_NDIM( IDCB ) )
            CALL MSG_OUT( ' ', 'Array has ^NDIM dimensions, ' //
     :                         'with bounds and pixel shifts:', STATUS )
            DO 1 I = 1, DCB_NDIM( IDCB )
               WRITE( BUF, 91 ) DCB_LBND( I, IDCB ),
     :                          DCB_UBND( I, IDCB ),
     :                          DCB_SFT( I, IDCB )
               CALL MSG_OUT( ' ', BUF, STATUS )
1           CONTINUE
            DO 2 I = DCB_NDIM( IDCB ) + 1, ARY__MXDIM
               WRITE( BUF, 92 ) DCB_LBND( I, IDCB ),
     :                          DCB_UBND( I, IDCB ),
     :                          DCB_SFT( I, IDCB )
               CALL MSG_OUT( ' ', BUF, STATUS )
2           CONTINUE

91    FORMAT( 10X, I10, ':', I10, 10X, I10 )
92    FORMAT( 9X, '(', I10, ':', I10, ')', 8X, '(', I10, ')' )

         END IF
      END IF

      END
