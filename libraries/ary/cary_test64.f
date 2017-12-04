#      PROGRAM CARY_TEST64
*+
*  Name:
*     CARY_TEST64

*  Purpose:
*     Test installation of 64 bit ARY Fortran API

*  Language:
*     Starlink Fortran 77

*  Description:
*     This program should be run after building and installing ARY in
*     order to test for correct installation. Note that this is not an
*     exhaustive test of ARY, but only of its installation.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB:  David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     16-JAN-1992 (RFWS):
*        Original version, derived from the equivalent HDS routine.
*     15-NOV-2017 (DSB):
*        - Lifted to tested the F77 interface to the C version of ARY.
*        - INTEGERs that holds bound, dimensions etc, changed to INTEGER*8.
*        - Added a call to EXIT(1) if an error occurs, so that the "make
*          check" command reports an error.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNV_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER FORM*30          ! Array form
      CHARACTER LOC*( DAT__SZLOC ) ! Top-level HDS locator
      CHARACTER TYPE*30          ! Array type
      INTEGER*8 DIM( 2 )         ! Array dimensions
      INTEGER*8 EL               ! Number of mapped elements
      INTEGER IARY               ! Array identifier
      INTEGER IARY2              ! Second array identifier
      INTEGER ISUM               ! Sum of array elements
      INTEGER*8 LBND( 2 )        ! Scaled array bounds
      INTEGER PLACE              ! Array placeholder
      INTEGER PNTR               ! Pointer to mapped array
      INTEGER*8 UBND( 2 )        ! Scaled array bounds
      INTEGER ZAXIS              ! Index of compression axis
      LOGICAL OK                 ! Was error correctly reported?
      REAL SCALE                 ! Scale factor
      REAL ZERO                  ! Zero offset
      REAL ZRAT                  ! Compression ratio

*  Local Data:
      DATA DIM / 10, 20 /

*.

*  Initialise the global status.
      STATUS = SAI__OK

*  Defer error reporting.
      CALL ERR_MARK()

*  Create a new container file.
      CALL HDS_NEW( 'ary_test', 'ARY_TEST', 'NDF', 0, DIM, LOC, STATUS )

*  Create an array inside it.
      CALL ARY_PLACE( LOC, 'ARRAY', PLACE, STATUS )
      CALL ARY_NEWPK( '_INTEGER', 2, DIM, PLACE, IARY, STATUS )

*  Map the array.
      CALL ARY_MAPK( IARY, '_REAL', 'WRITE', PNTR, EL, STATUS )

*  Initialise the array.
      CALL SETUP( EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )

*  Clean up and close the file.
      CALL ARY_ANNUL( IARY, STATUS )
      CALL HDS_CLOSE( LOC, STATUS )

*  Re-open the file.
      CALL HDS_OPEN( 'ary_test', 'UPDATE', LOC, STATUS )

*  Find and map the array.
      CALL ARY_FIND( LOC, 'ARRAY', IARY, STATUS )
      CALL ARY_MAPK( IARY, '_INTEGER', 'READ', PNTR, EL, STATUS )

*  Sum the data elements.
      CALL SUM( EL, %VAL( CNF_PVAL( PNTR ) ), ISUM, STATUS )

*  Clean up and erase the container file.
      CALL ARY_ANNUL( IARY, STATUS )
      CALL HDS_ERASE( LOC, STATUS )

*  Check if the test ran OK. If not, report an error.
      IF( STATUS .EQ. SAI__OK .AND. ISUM .NE. 20100 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ISUM', ISUM )
         CALL ERR_REP( 'ART_TEST_SER0', 'Bad data sum ^ISUM (should '//
     :                 'be 20100).', STATUS )
      END IF



*  Do similar tests for scaled arrays...

*  Create an HDS file and create a new _WORD 2D simple array in it. Fill the
*  array with integers equal to the element index.
      CALL HDS_NEW( 'ary_test2', 'TEST', 'TEST', 0, 0, LOC, STATUS )
      CALL ARY_PLACE( LOC, 'TEST', PLACE, STATUS )

      LBND( 1 ) = -100
      LBND( 2 ) = -200
      UBND( 1 ) = 100
      UBND( 2 ) = 0
      CALL ARY_NEWK( '_WORD', 2, LBND, UBND, PLACE, IARY, STATUS )
      CALL ARY_MAPK( IARY, '_INTEGER', 'WRITE', PNTR, EL, STATUS )
      CALL FILL( EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )
      CALL ARY_UNMAP( IARY, STATUS )

*  Set scale and zero terms for this simple array, and then check that
*  the storage form is now SCALED.
      CALL ARY_PTSZR( IARY, 2.0, -1.2, STATUS )
      CALL ARY_FORM( IARY, FORM, STATUS )

      IF( FORM .NE. 'SCALED' .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'F', FORM )
         CALL ERR_REP( 'ART_TEST_SER1', 'Bad form ^F for scaled array',
     :                  STATUS )
      END IF

      CALL ARY_GTSZR( IARY, SCALE, ZERO, STATUS )

      IF( SCALE .NE. 2.0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETR( 'S', SCALE )
         CALL ERR_REP( 'ART_TEST_SER1', 'Bad scale (^S) for scaled '//
     :                 'array', STATUS )
      END IF

      IF( ZERO .NE. -1.2 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETR( 'Z', ZERO )
         CALL ERR_REP( 'ART_TEST_SER1', 'Bad zero (^Z) for scaled '//
     :                 'array', STATUS )
      END IF

      CALL ARY_SCTYP( IARY, TYPE, STATUS )

      IF( TYPE .NE. '_WORD' .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( 'ART_TEST_SER1', 'Bad scaled type (^T) for '//
     :                 'scaled array', STATUS )
      END IF

      CALL ARY_TYPE( IARY, TYPE, STATUS )

      IF( TYPE .NE. '_REAL' .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( 'ART_TEST_SER1', 'Bad unscaled type (^T) for '//
     :                 'scaled array', STATUS )
      END IF


*  Check the data type is _REAL (because the scale and zero were set as
*  reals).
      CALL ARY_TYPE( IARY, TYPE, STATUS )
      IF( TYPE .NE. '_REAL' .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( 'ART_TEST_SER2', 'Bad type ^T for scaled array',
     :                  STATUS )
      END IF

*  Map the array and check that the mapped values include the effect of
*  the above scaling.
      CALL ARY_MAPK( IARY, '_REAL', 'READ', PNTR, EL, STATUS )
      CALL BACKR( EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )
      CALL ARY_UNMAP( IARY, STATUS )

*  Close the HDS file and then re-open it.
      CALL ARY_ANNUL( IARY, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )
      CALL HDS_OPEN( 'ary_test2', 'WRITE', LOC, STATUS )

*  Import the array and check that it has SCALED storage form, and check
*  the mapped array values are correct.
      CALL ARY_FIND( LOC, 'TEST', IARY, STATUS )
      CALL ARY_FORM( IARY, FORM, STATUS )
      IF( FORM .NE. 'SCALED' .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'F', FORM )
         CALL ERR_REP( 'ART_TEST_SER3', 'Bad form ^F for scaled array',
     :                  STATUS )
      END IF

      CALL ARY_MAPK( IARY, '_REAL', 'READ', PNTR, EL, STATUS )
      CALL BACKR( EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )
      CALL ARY_UNMAP( IARY, STATUS )

*  Check an "Access denied" error is reported if we try to map a scaled
*  array for update or write access.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL ERR_MARK
         OK = .TRUE.
         CALL ARY_MAPK( IARY, '_REAL', 'UPDATE', PNTR, EL, STATUS )
         IF( STATUS .NE. ARY__CMPAC ) THEN
            OK = .FALSE.
         ELSE
            CALL ERR_ANNUL( STATUS )
         END IF

         CALL ERR_END( STATUS )

         IF( .NOT. OK .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ART_TEST_SER4', 'No error on modifying a '//
     :                    'scaled array', STATUS )
         END IF
      END IF

*  Copy the scaled array to a new temporary array.
      CALL ARY_TEMP( PLACE, STATUS )
      CALL ARY_COPY( IARY, PLACE, IARY2, STATUS )

*  Check the values in the copied array are correct.
      CALL ARY_MAPK( IARY2, '_REAL', 'READ', PNTR, EL, STATUS )
      CALL BACKR( EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )
      CALL ARY_UNMAP( IARY2, STATUS )

*  Check the storage form of the copy.
      CALL ARY_FORM( IARY2, FORM, STATUS )
      IF( FORM .NE. 'SIMPLE' .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'F', FORM )
         CALL ERR_REP( 'ART_TEST_SER5', 'Bad form ^F for simple array',
     :                  STATUS )
      END IF

*  Delete the HDS file.
      CALL ARY_ANNUL( IARY, STATUS )
      CALL HDS_ERASE( LOC, STATUS )




*  Do similar tests for delta arrays...

*  Create _INTEGER 2D simple temporary array in it. Fill the array with
*  integers equal to the element index.
      CALL ARY_TEMP( PLACE, STATUS )
      LBND( 1 ) = -100
      LBND( 2 ) = -200
      UBND( 1 ) = 100
      UBND( 2 ) = 0
      CALL ARY_NEWK( '_INTEGER', 2, LBND, UBND, PLACE, IARY2, STATUS )
      CALL ARY_MAPK( IARY2, '_INTEGER', 'WRITE', PNTR, EL, STATUS )
      CALL FILL( EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )
      CALL ARY_UNMAP( IARY2, STATUS )

*  Create a delta compressed copy of the array, stroring it in an HDS file.
      CALL HDS_NEW( 'ary_test3', 'TEST', 'TEST', 0, 0, LOC, STATUS )
      CALL ARY_PLACE( LOC, 'TEST', PLACE, STATUS )
      CALL ARY_DELTA( IARY2, 0, ' ', 0.0, PLACE, ZRAT, IARY, STATUS )
      IF( ABS( ZRAT - 3.827846 ) .GT. 1.0E-4 .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETR( 'Z', ZRAT )
         CALL ERR_REP( ' ', 'Unexpected compression factor ^Z '//
     :                 '(should be 3.827846) for delta array', STATUS )
      END IF

*  Delete the temporary delta compressed array
      call ARY_ANNUL( IARY2, STATUS )

*  Check the storage form is DELTA
      CALL ARY_FORM( IARY, FORM, STATUS )
      IF( FORM .NE. 'DELTA' .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'F', FORM )
         CALL ERR_REP( 'ART_TEST_SER1', 'Bad form ^F for delta array',
     :                  STATUS )
      END IF

*  Check the compression axis and compressed data type
      CALL ARY_GTDLT( IARY, ZAXIS, TYPE, ZRAT, STATUS )

      IF( ABS( ZRAT - 3.827846 ) .GT. 1.0E-4 .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETR( 'Z', ZRAT )
         CALL ERR_REP( ' ', 'Unexpected compression factor ^Z '//
     :                 '(should be 3.827846) for a delta array',
     :                 STATUS )
      END IF

      IF( TYPE .NE. '_BYTE' .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( ' ', 'Unexpected compressed data type ^T '//
     :                 '(should be _BYTE) for a delta array', STATUS )
      END IF

      IF( ZAXIS .NE. 1 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'A', ZAXIS )
         CALL ERR_REP( ' ', 'Unexpected compression axis ^A '//
     :                 '(should be 1) for a delta array', STATUS )
      END IF

*  Check the uncompressed array data type
      CALL ARY_TYPE( IARY, TYPE, STATUS )
      IF( TYPE .NE. '_INTEGER' .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( 'ART_TEST_SER1', 'Bad uncompressed type (^T) '//
     :                 'for delta array', STATUS )
      END IF

*  Map the array and check that the mapped values are uncompressed. Do it
*  first as INTEGER then as REAL.
      CALL ARY_MAPK( IARY, '_INTEGER', 'READ', PNTR, EL, STATUS )
      CALL UNCOMPI( EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )
      CALL ARY_UNMAP( IARY, STATUS )

      CALL ARY_MAPK( IARY, '_REAL', 'READ', PNTR, EL, STATUS )
      CALL UNCOMPR( EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )
      CALL ARY_UNMAP( IARY, STATUS )

*  Close the HDS file and then re-open it.
      CALL ARY_ANNUL( IARY, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )
      CALL HDS_OPEN( 'ary_test3', 'WRITE', LOC, STATUS )

*  Import the array and check that it has DELTA storage form, and check
*  the mapped array values are correct.
      CALL ARY_FIND( LOC, 'TEST', IARY, STATUS )
      CALL ARY_FORM( IARY, FORM, STATUS )
      IF( FORM .NE. 'DELTA' .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'F', FORM )
         CALL ERR_REP( 'ART_TEST_SER3', 'Bad form ^F for delta array',
     :                  STATUS )
      END IF

      CALL ARY_MAPK( IARY, '_REAL', 'READ', PNTR, EL, STATUS )
      CALL UNCOMPR( EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )
      CALL ARY_UNMAP( IARY, STATUS )

*  Check an "Access denied" error is reported if we try to map a delta
*  array for update or write access.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL ERR_MARK
         OK = .TRUE.
         CALL ARY_MAPK( IARY, '_REAL', 'UPDATE', PNTR, EL, STATUS )
         IF( STATUS .NE. ARY__CMPAC ) THEN
            OK = .FALSE.
         ELSE
            CALL ERR_ANNUL( STATUS )
         END IF

         CALL ERR_END( STATUS )

         IF( .NOT. OK .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ART_TEST_SER4', 'No error on modifying a '//
     :                    'delta array', STATUS )
         END IF
      END IF

*  Copy the delta array to a new temporary array.
      CALL ARY_TEMP( PLACE, STATUS )
      CALL ARY_COPY( IARY, PLACE, IARY2, STATUS )

*  Check the values in the copied array are correct.
      CALL ARY_MAPK( IARY2, '_REAL', 'READ', PNTR, EL, STATUS )
      CALL UNCOMPR( EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )
      CALL ARY_UNMAP( IARY2, STATUS )

*  Check the storage form of the copy.
      CALL ARY_FORM( IARY2, FORM, STATUS )
      IF( FORM .NE. 'SIMPLE' .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'F', FORM )
         CALL ERR_REP( 'ART_TEST_SER5', 'Bad form ^F for simple array',
     :                  STATUS )
      END IF

*  Delete the HDS file.
      CALL ARY_ANNUL( IARY, STATUS )
      CALL HDS_ERASE( LOC, STATUS )

*  Report a context error.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_TEST_ERR', 'ARY_TEST: ARY installation '//
     :                 'test failed.', STATUS )
      END IF

*  Display any deferred error messages.
      CALL ERR_RLSE()

*  Abort with a non-zero status if an error has occurred, so that the
*  makefile can detect that the test has failed.
      IF( STATUS .NE. SAI__OK ) CALL EXIT(1)

      END




      SUBROUTINE SETUP( EL, ARRAY, STATUS )
*+
*  Name:
*     SETUP

*  Purpose:
*     Initialise an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SETUP( EL, ARRAY, STATUS )

*  Description:
*     Set each element of a 1-dimensional array equal to its element
*     number.

*  Arguments:
*     EL = INTEGER (Given)
*        Number of array elements.
*     ARRAY( EL ) = REAL (Returned)
*        Array to be initialised.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     19-AUG-1991 (RFWS):
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
      INTEGER*8 EL

*  Arguments Returned:
      REAL ARRAY( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER*8 I                ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the array.
      DO 1 I = 1, EL
         ARRAY( I ) = REAL( I )
 1    CONTINUE

      END

      SUBROUTINE SUM( EL, ARRAY, ISUM, STATUS )
*+
*  Name:
*     SUM

*  Purpose:
*     Sum the elements of an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUM( EL, ARRAY, ISUM, STATUS )

*  Description:
*     Return the sum of the elements of a 1-dimensional array.

*  Arguments:
*     EL = INTEGER (Given)
*        Number of array elements.
*     ARRAY( EL ) = INTEGER (Given)
*        Array whose elements are to be summed.
*     ISUM = INTEGER (Returned)
*        Sum of array elements.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     19-AUG-1991 (RFWS):
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
      INTEGER*8 EL
      INTEGER ARRAY( * )

*  Arguments Returned:
      INTEGER ISUM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER*8 I                ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      ISUM = 0

*  Sum the array elements.
      DO 1 I = 1, EL
         ISUM = ISUM + ARRAY( I )
 1    CONTINUE

      END




*  Fill the supplied array with integers equal to the element index.
      SUBROUTINE FILL( EL, ARRAY, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER*8 I, EL
      INTEGER STATUS
      INTEGER ARRAY( EL )

      IF( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, EL
         ARRAY( I ) = I
      END DO

      END



*  Check the supplied array has values equal to 2.0*element index - 1.2
      SUBROUTINE BACKR( EL, ARRAY, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER*8 I, EL
      INTEGER STATUS
      REAL ARRAY( EL ), VAL

      IF( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, 10
         VAL = 2.0*I - 1.2
         IF( ABS( ARRAY( I ) - VAL ) .GT. 1.0E-6 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETR( 'B', ARRAY( I ) )
            CALL MSG_SETR( 'SB', 2.0*I - 1.2 )
            CALL MSG_SETI( 'I', I )
            CALL ERR_REP( 'ARY_TEST_E10', 'Bad value (^B) at element '//
     :                    '^I. Should be ^SB.', STATUS )
            RETURN
         END IF
      END DO

      END




*  Check the supplied array has values equal to the element indices _ REAL
      SUBROUTINE UNCOMPR( EL, ARRAY, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS
      INTEGER*8 I, EL
      REAL ARRAY( EL )

      IF( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, 10
         IF( ABS( ARRAY( I ) - REAL( I ) ) .GT. 1.0E-6 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETR( 'B', ARRAY( I ) )
            CALL MSG_SETR( 'SB', REAL( I ) )
            CALL MSG_SETI( 'I', I )
            CALL ERR_REP( 'ARY_TEST_E11', 'Bad value (^B) at element '//
     :                    '^I. Should be ^SB.', STATUS )
            RETURN
         END IF
      END DO

      END

*  Check the supplied array has values equal to the element indices _ INTEGER
      SUBROUTINE UNCOMPI( EL, ARRAY, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS
      INTEGER*8 I, EL
      INTEGER ARRAY( EL )

      IF( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, 10
         IF( ARRAY( I ) .NE. I ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'B', ARRAY( I ) )
            CALL MSG_SETI( 'SB', I )
            CALL MSG_SETI( 'I', I )
            CALL ERR_REP( 'ARY_TEST_E11', 'Bad value (^B) at element '//
     :                    '^I. Should be ^SB.', STATUS )
            RETURN
         END IF
      END DO

      END



