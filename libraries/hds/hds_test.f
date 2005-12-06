      PROGRAM HDS_TEST
*+
*  Name:
*     HDS_TEST

*  Purpose:
*     Test installation of HDS.

*  Language:
*     Starlink Fortran 77

*  Description:
*     This program should be run after building and installing HDS in
*     order to test for correct installation. Note that this is not an
*     exhaustive test of HDS, but only of its installation.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     PWD: Peter W. Draper(JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     19-AUG-1991 (RFWS):
*        Original version.
*     25-AUG-1992 (RFWS):
*        Changed to use EMS instead of ERR.
*     26-AUG-1992 (RFWS):
*        Removed calls to HDS_START and HDS_STOP - no longer needed.
*     08-NOV-2005 (PWD):
*        Added call to HDS_TUNE to switch on 64 bit mode.
*     29-NOV-2005 (TIMJ):
*        Add test for DAT_PREC and DAT_CCTYP
*     02-DEC-2005 (TIMJ):
*        Add test for DAT_MSG / DAT_REF
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'DAT_ERR'          ! DAT_ error codes
      INCLUDE 'CMP_ERR'          ! CMP_ error codes
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      CHARACTER * ( 32 ) PATH    ! File name
      PARAMETER ( PATH = 'hds_test' )

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC1 ! Top-level locator
      CHARACTER * ( DAT__SZLOC ) LOC2 ! Locator for data array
      CHARACTER * ( DAT__SZLOC ) LOC3 ! Locator for cell array
      CHARACTER * ( DAT__SZLOC ) LOC4 ! Locator for cell array
      CHARACTER * ( DAT__SZLOC ) LOC5 ! Locator for cell array
      CHARACTER * ( DAT__SZLOC ) LOC6 ! Locator for cell array
      CHARACTER * ( DAT__SZLOC ) LOC7 ! Locator for cell array
      CHARACTER * ( 128 ) REF     ! Reference to HDS object
      INTEGER DIM( 2 )           ! Data array dimensions
      INTEGER EL                 ! Number of mapped elements
      INTEGER ISUM               ! Sum of array elements
      INTEGER PNTR               ! Pointer to mapped array
      INTEGER EXITSTATUS         ! Exit status to be reported at end
      INTEGER NCOMP              ! Number of top level HDS components
      INTEGER CDIM(2)            ! Cell dimensions
      REAL    ANSWER
      LOGICAL PRIM
      INTEGER NBYTES
      INTEGER CSIZE              ! Size to allocate _CHAR field
      INTEGER REFLEN             ! Actual size of REF
      INTEGER LSTAT
      CHARACTER * (DAT__SZTYP) CTYPE ! type field for _CHAR
      REAL RDIM(5), RDIM_OUT(5)
      INTEGER ACTVAL
      INTEGER I
      CHARACTER * (5) CHARARR(2)

*  Local Data:
      DATA DIM / 10, 20 /
      DATA RDIM / 1.0, 2.0, 3.0, 4.0, 5.0 /
      DATA CHARARR / 'TEST1', 'TEST2' /
*.

*  Initialise the global status.
      STATUS = SAI__OK

      CALL EMS_BEGIN( STATUS )

*  Make check work for version 4 files.
      CALL HDS_TUNE( '64BIT', 1, STATUS )

*  Create a new container file with a data array inside it.
      CALL HDS_NEW( PATH, 'HDS_TEST', 'NDF', 0, DIM, LOC1,
     :              STATUS )
      CALL DAT_NEW( LOC1, 'DATA_ARRAY', '_INTEGER', 2, DIM, STATUS )

      CSIZE = 20
      CALL DAT_CCTYP( 20, CTYPE )
      CALL DAT_NEW( LOC1, 'CHARTEST', CTYPE, 0, DIM, STATUS )
      CALL DAT_NEW0( LOC1, 'SCALAR', '_DOUBLE', STATUS )
      CALL DAT_NEW0L( LOC1, 'SCALARL', STATUS )
      CALL DAT_NEW0C( LOC1, 'SCALARC', 32, STATUS )

      CALL DAT_NEW1R( LOC1, 'ONEDR', 5, STATUS )
      CALL DAT_NEW1C( LOC1, 'ONEDCHAR', 5, 2, STATUS )

*  Create a test structure
      CALL DAT_NEW( LOC1, 'TSTRUCT', 'STRUCT', 0, DIM, STATUS )
      CALL DAT_FIND( LOC1, 'TSTRUCT', LOC3, STATUS )
      CALL DAT_NEW( LOC3, 'ARRAY', '_DOUBLE', 2, DIM, STATUS )

*  Store something in it
      CALL DAT_FIND( LOC1, 'ONEDR', LOC4, STATUS )
      CALL DAT_PUT1R( LOC4, 5, RDIM, STATUS )
      CALL DAT_ANNUL( LOC4, STATUS )

      CALL DAT_FIND( LOC1, 'ONEDCHAR', LOC4, STATUS )
      CALL DAT_PUTVC( LOC4, 2, CHARARR, STATUS )
      CALL DAT_ANNUL( LOC4, STATUS )

*  Find and map the data array.
      CALL DAT_FIND( LOC1, 'DATA_ARRAY', LOC2, STATUS )
      CALL DAT_MAPV( LOC2, '_REAL', 'WRITE', PNTR, EL, STATUS )

*  Initialise the array.
      CALL SETUP( EL, %VAL( CNF_PVAL( PNTR ) ), ANSWER, STATUS )
      CALL DAT_UNMAP( LOC2, STATUS )

*  And the one in the test struct
      CALL DAT_FIND( LOC3, 'ARRAY', LOC4, STATUS )
      CALL DAT_MAPV( LOC4, '_REAL', 'WRITE', PNTR, EL, STATUS )
      CALL SETUP( EL, %VAL( CNF_PVAL( PNTR ) ), ANSWER, STATUS )
      CALL DAT_UNMAP( LOC4, STATUS )


*  Clean up and close the file.
      CALL DAT_ANNUL( LOC2, STATUS )
      CALL DAT_ANNUL( LOC4, STATUS )
      CALL DAT_ANNUL( LOC3, STATUS )
      CALL HDS_CLOSE( LOC1, STATUS )

*  Re-open the file.
      CALL HDS_OPEN( PATH, 'UPDATE', LOC1, STATUS )

*  Count the number of components
      CALL DAT_NCOMP( LOC1, NCOMP, STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         IF (NCOMP .NE. 8) THEN
            STATUS = SAI__ERROR
            CALL EMS_REP( 'HDS_TEST_ERR',
     :           'HDS_TEST: Failed in NCOMP.',
     :           STATUS )
         END IF
      END IF

*  Find and map the data array.
      CALL DAT_FIND( LOC1, 'DATA_ARRAY', LOC2, STATUS )

      CDIM(1) = 200
      CDIM(2) = 3
      CALL DAT_VEC( LOC2, LOC3, STATUS )
      CALL DAT_SIZE( LOC3, CDIM(2), STATUS )

      CALL DAT_PREC( LOC3, NBYTES, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( NBYTES .NE. 4 ) THEN
            STATUS = SAI__ERROR
            CALL EMS_SETI( 'N', NBYTES)
            CALL EMS_REP('HDS_TEST_ERR_PREC', 
     :           'Precision of _REAL component != 4 (got ^N)', STATUS)
         END IF
      END IF

      CALL DAT_ANNUL( LOC3, STATUS )

*  Now look for the string component
      CALL DAT_FIND( LOC1, 'CHARTEST', LOC3, STATUS ) 
      CALL DAT_PREC( LOC3, NBYTES, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( NBYTES .NE. CSIZE ) THEN
            STATUS = SAI__ERROR
            CALL EMS_SETI( 'CMP1', CSIZE )
            CALL EMS_SETI( 'REF', CSIZE )
            CALL EMS_SETI( 'N', NBYTES )
            CALL EMS_REP('HDS_TEST_ERR_PREC', 
     :           'Precision of _CHAR*^CMP1 component != ^REF (got ^N)', 
     :           STATUS)
         END IF
      END IF

*  Take a section of LOC2
      CDIM(1) = 1
      CDIM(2) = 1
      CALL DAT_CELL( LOC2, 2, CDIM, LOC4, STATUS )

*  Get a locator to the .TSTRUCT.ARRAY
      CALL DAT_FIND( LOC1, 'TSTRUCT', LOC5, STATUS )
      CALL DAT_FIND( LOC5, 'ARRAY', LOC6, STATUS )
      CDIM(1) = 10
      CDIM(2) = 3
      CALL DAT_CELL( LOC6, 2, CDIM, LOC7, STATUS )

      IF (STATUS .EQ. SAI__OK) THEN
         CALL EMS_MARK()
         CALL DAT_MSG( 'LOCA', LOC3 )
         LSTAT = SAI__ERROR
         CALL EMS_REP( 'XXX', 
     :        'Not an error, test DAT_MSG: ^LOCA', LSTAT)
         CALL DAT_MSG( 'LOCB', LOC4 )
         CALL EMS_REP( 'XXX', 
     :        'Not an error, test DAT_MSG: ^LOCB', LSTAT)
         CALL DAT_MSG( 'LOCC', LOC7 )
         CALL EMS_REP( 'XXX', 
     :        'Not an error, test DAT_MSG: ^LOCC', LSTAT)
         CALL EMS_RLSE
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         CALL EMS_MARK
         LSTAT = SAI__OK
         CALL DAT_REF( LOC1, REF, REFLEN, LSTAT )
         PRINT *, 'REF: (',REFLEN,') ', REF(:REFLEN)
         IF ( LSTAT .EQ. DAT__TRUNC ) CALL EMS_ANNUL(LSTAT)
         CALL DAT_REF( LOC3, REF, REFLEN, LSTAT )
         PRINT *, 'REF: (',REFLEN,') ', REF(:REFLEN)
         IF ( LSTAT .EQ. DAT__TRUNC ) CALL EMS_ANNUL(LSTAT)
         CALL DAT_REF( LOC7, REF, REFLEN, LSTAT )
         PRINT *, 'REF: (',REFLEN,') ', REF(:REFLEN)
         IF ( LSTAT .EQ. DAT__TRUNC ) CALL EMS_ANNUL(LSTAT)
         CALL EMS_RLSE
      END IF

*  Clean up

      CALL DAT_ANNUL( LOC3, STATUS )
      CALL DAT_ANNUL( LOC4, STATUS )
      CALL DAT_ANNUL( LOC5, STATUS )
      CALL DAT_ANNUL( LOC6, STATUS )
      CALL DAT_ANNUL( LOC7, STATUS )

*  Check DAT_GET1R
      CALL DAT_FIND( LOC1, 'ONEDR', LOC3, STATUS )
      CALL DAT_GET1R( LOC3, 5, RDIM_OUT, ACTVAL, STATUS )
      CALL DAT_ANNUL( LOC3, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
         DO I = 1, 5
            IF (STATUS .EQ. SAI__OK) THEN
               IF ( RDIM(I) .NE. RDIM_OUT(I) ) THEN
                  STATUS = SAI__ERROR
                  CALL EMS_REP( 'GET',
     :                 'Did not get what we put in', STATUS)
               END IF
            END IF
         END DO
      END IF

*  Force primitive

      CALL DAT_PRIM( LOC2, PRIM, STATUS )

*  Quick check of MAPN
      CALL DAT_MAPN( LOC2, '_INTEGER', 'READ', 2, PNTR, CDIM, STATUS)
      CALL DAT_UNMAP( LOC2, STATUS )

*  Now map vectorized
      CALL DAT_MAPV( LOC2, '_INTEGER', 'READ', PNTR, EL, STATUS )



*  Sum the data elements.
      CALL SUM( EL, %VAL( CNF_PVAL( PNTR ) ), ISUM, STATUS )

*  Clean up and erase the container file.
      CALL DAT_UNMAP( LOC2, STATUS )
      CALL DAT_ANNUL( LOC2, STATUS )
      CALL HDS_ERASE( LOC1, STATUS )

*  Check if the test ran OK. If so, then report success.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( ISUM .EQ. INT(ANSWER) )) THEN
         WRITE( *, * ) '   HDS installation test succeeded.'
         EXITSTATUS = 0

*  Otherwise, report an error.
      ELSE
         IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
         PRINT *, 'Sum = ',ISUM, ' expected ',ANSWER
         CALL EMS_REP( 'HDS_TEST_ERR',
     :   'HDS_TEST: HDS installation test failed.', STATUS )
         EXITSTATUS = 1
      END IF

      CALL EMS_END( STATUS )
      
*   Use non-standard but common exit() intrinsic
      CALL EXIT(EXITSTATUS)

      END

      SUBROUTINE SETUP( EL, ARRAY, SUM, STATUS )
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
*     SUM = REAL (Returned)
*        Sum of all pixels written to ARRAY
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
      INTEGER EL

*  Arguments Returned:
      REAL ARRAY( * )
      REAL SUM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the array.
      SUM = 0.0
      DO 1 I = 1, EL
         ARRAY( I ) = REAL( I )
         SUM = SUM + REAL( I )
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
      INTEGER EL
      INTEGER ARRAY( * )

*  Arguments Returned:
      INTEGER ISUM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

*  Initialise.
      ISUM = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Sum the array elements.
      DO 1 I = 1, EL
         ISUM = ISUM + ARRAY( I )
 1    CONTINUE

      END
