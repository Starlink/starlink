      SUBROUTINE ARY1_CPY( IACB1, TEMP, LOC, IACB2, STATUS )
*+
*  Name:
*     ARY1_CPY

*  Purpose:
*     Make a copy of an ACB array entry in a new (or temporary) HDS
*     location.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_CPY( IACB1, TEMP, LOC, IACB2, STATUS )

*  Description:
*     The routine creates a new base array containing a copy of an
*     existing array (identified by its ACB entry) and creates a new
*     ACB entry to describe it. The HDS location for the new array is
*     passed by means of a locator to an array placeholder object. This
*     locator may later be annulled.

*  Arguments:
*     IACB1 = INTEGER (Given)
*        Index to the ACB entry of the array to be copied.
*     TEMP = LOGICAL (Given)
*        Whether the copy is to be a temporary array (this is used to
*        set its disposal mode in the DCB).
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to array placeholder object (a scalar data structure
*        of type ARRAY).
*     IACB2 = INTEGER (Returned)
*        Index to the ACB entry of the copy.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A value of zero will be returned for the IACB2 argument if the
*     routine is called with STATUS set, although no further processing
*     will occur.
*     -  A value of zero will also be returned for the IACB2 argument if
*     the routine fails for any reason.

*  Algorithm:
*     -  Set an initial value for the IACB2 argument before checking the
*     inherited status.
*     -  Get the DCB index for the (input) data object to be copied.
*     -  If the input array is a base array, then check it is not
*     already mapped for update or write access. Report an error if it
*     is.
*     -  Make a copy of the data object and create a new base array
*     entry in the ACB to describe it.
*     -  If the input array is not a base array, then ensure that form
*     information is available for it in the DCB.
*     -  Handle each form of array in turn.
*     -  For primitive arrays, see if the array bounds are consistent
*     with the creation of a new primitive array.
*     -  Ensure that data type information is available in the DCB.
*     -  Create a new data object (with an entry in the DCB) with the
*     correct type and bounds to accommodate the copied data. Create a
*     primitive array if possible. Otherwise, create a simple array.
*     -  Create a new base array entry in the ACB to describe it.
*     -  Ensure that state information for the input array is available
*     in the DCB.
*     -  If the input array is in the "defined" state, then its data
*     values must be copied to the output array. Produce a cloned copy
*     of its ACB entry, to allow it to be mapped even if the existing
*     ACB entry is already mapped for access.
*     -  Map the input array for reading through the cloned ACB entry
*     and the output array for writing.
*     -  Find the number of elements to be copied.
*     -  Test for each possible numeric data type and call the
*     appropriate routine to transfer data from the input array to the
*     output.
*     -  Unmap the mapped ACB entries and annul the cloned ACB entry.
*     -  For simple arrays, create a new data object with the correct
*     type and bounds to accommodate the copied data.
*     -  Ensure that state information is available in the DCB for the
*     input data object.
*     -  If the input array is in the "defined" state, then clone its
*     ACB entry so it can be mapped for reading even if the existing ACB
*     entry is already mapped for access.
*     -  Map the (cloned) input array ACB entry for reading and the new
*     output array entry for writing.
*     -  Find the number of data components to be copied and the number
*     of elements in each component.
*     -  Loop to transfer the data in each component.
*     -  Test the numeric data type against each possible value and call
*     the appropriate routine to transfer the input data to the new
*     array.
*     -  Unmap the ACB entries.
*     -  Transfer the bad pixel flag value to the new ACB entry.
*     -  Annul the cloned ACB entry.
*     -  If the array form entry in the DCB was not recognised, then
*     report an error.
*     -  If an error occurred, then reset the IACB2 argument.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-AUG-1989 (RFWS):
*        Original version.
*     9-OCT-1989 (RFWS):
*        Converted to use a placeholder object to indicate where the
*        output array should be located.
*     10-OCT-1989 (RFWS):
*        Changed to allow "undefined" arrays to be copied and to allow
*        array copying in the case where the input array is already
*        mapped for access through the ACB entry supplied. The routine
*        will still fail if the copying operation involves conflicting
*        mapped access (e.g. simultaneous READ and WRITE access) to the
*        data object, however.
*     17-OCT-1989 (RFWS):
*        Added checks on conflicting mapped access to base arrays. Also
*        added code to propagate the bad pixel flag to the new array.
*     18-OCT-1989 (RFWS):
*        Updated prologue and added code to clean up by annulling the
*        new ACB entry under error conditions.
*     20-OCT-1989 (RFWS):
*        Added code to use the DCB form entry.
*     22-NOV-1989 (RFWS):
*        Changed argument list to match new version of ARY1_BAD.
*     12-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
*     8-MAR-1990 (RFWS):
*        Added missing escape character in error message.
*     12-MAR-1990 (RFWS):
*        Changed the placeholder type to ARRAY.
*     22-OCT-1991 (RFWS):
*        Removed unused variable.
*     {enter_further_changes_here}

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
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_CPX( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the data object is complex.
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Data object storage form.
*        DCB_TYP( ARY__MXDCB ) = CHARACTER * ( ARY__SZTYP ) (Read)
*           Numeric type of data object.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_CUT( ARY__MXACB ) = LOGICAL (Read)
*           Whether the array is a "cut".
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_LBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Lower bounds of array.
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Read)
*           Number of array dimensions.
*        ACB_UBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Upper bounds of array.

*  Arguments Given:
      INTEGER IACB1
      LOGICAL TEMP
      CHARACTER * ( * ) LOC

*  Arguments Returned:
      INTEGER IACB2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EL                 ! Number of data elements per component
      INTEGER IACBC              ! Index to cloned ACB entry
      INTEGER ICOMP              ! Loop counter for array components
      INTEGER IDCB1              ! Index to input object entry in DCB
      INTEGER IDCB2              ! Index to output object entry in DCB
      INTEGER IERR               ! Position of conversion error (dummy)
      INTEGER NCOMP              ! Number of array components to copy
      INTEGER NERR               ! Number of conversion errors (dummy)
      INTEGER PNTR1( 2 )         ! Pointer to mapped input data
      INTEGER PNTR2( 2 )         ! Pointer to mapped output data
      LOGICAL BAD                ! Bad pixel flag
      LOGICAL PBND               ! Whether bounds are primitive

*.

*  Set an initial value for the IACB2 argument.
      IACB2 = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the DCB index for the data object to be copied.
      IDCB1 = ACB_IDCB( IACB1 )

*  If the input array is a base array, then check to see that there is
*  no conflicting mapped access for write or update in effect. Report an
*  error if there is.
      IF ( .NOT. ACB_CUT( IACB1 ) ) THEN
         IF ( DCB_NWRIT( IDCB1 ) .NE. 0 ) THEN
            STATUS = ARY__ISMAP
            CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB1 ) )
            CALL ERR_REP( 'ARY1_CPY_MAP',
     :      'The array structure ^ARRAY is already mapped for ' //
     :      'UPDATE or WRITE access, perhaps through another ' //
     :      'identifier (possible programming error).', STATUS )

*  Make a direct copy of the data object and then create a new base
*  array entry in the ACB to describe it.
         ELSE
            CALL ARY1_DCPY( IDCB1, TEMP, LOC, IDCB2, STATUS )
            CALL ARY1_CRNBA( IDCB2, IACB2, STATUS )
         END IF

*  If it is not a base array, then ensure that form information is
*  available for it in the DCB.
      ELSE
         CALL ARY1_DFRM( IDCB1, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Handle each form of array in turn...

*  Primitive arrays.
*  ================
            IF ( DCB_FRM( IDCB1 ) .EQ. 'PRIMITIVE' ) THEN

*  See if the array bounds are consistent with the creation of a new
*  primitive array.
               CALL ARY1_PBND( IACB1, PBND, STATUS )

*  Ensure that data type information is available in the DCB.
               CALL ARY1_DTYP( IDCB1, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Create a new data object (with an entry in the DCB) with the correct
*  type and bounds to accommodate the copied data. Create a primitive
*  array if possible. Otherwise, create a simple array.
                  IF ( PBND ) THEN
                     CALL ARY1_DCREP( DCB_TYP( IDCB1 ),
     :                                ACB_NDIM( IACB1 ),
     :                                ACB_UBND( 1, IACB1 ), TEMP, LOC,
     :                                IDCB2, STATUS )
                  ELSE
                     CALL ARY1_DCRE( DCB_TYP( IDCB1 ), .FALSE.,
     :                               ACB_NDIM( IACB1 ),
     :                               ACB_LBND( 1, IACB1 ),
     :                               ACB_UBND( 1, IACB1 ), TEMP, LOC,
     :                               IDCB2, STATUS )
                  END IF
               END IF

*  Create a new base array entry in the ACB to describe it.
               CALL ARY1_CRNBA( IDCB2, IACB2, STATUS )

*  Ensure that state information for the input array is available in the
*  DCB.
               CALL ARY1_DSTA( IDCB1, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If the input array is in the "defined" state, then its data values
*  must be copied to the output array. Produce a cloned copy of its ACB
*  entry, to allow it to be mapped even if the existing ACB entry is
*  already mapped for access.
                  IF ( DCB_STA( IDCB1 ) ) THEN
                     CALL ARY1_CLN( IACB1, IACBC, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN
      
*  Map the input array for reading through the cloned ACB entry and the
*  output array for writing.
                        CALL ARY1_MAPS( IACBC, DCB_TYP( IDCB1 ),
     :                                  .FALSE., 'READ',
     :                                  PNTR1( 1 ), PNTR1( 2 ),
     :                                  STATUS )
                        CALL ARY1_MAPS( IACB2, DCB_TYP( IDCB1 ),
     :                                  .FALSE., 'WRITE',
     :                                  PNTR2( 1 ), PNTR2( 2 ),
     :                                  STATUS )

*  Find the number of elements to be copied.
                        CALL ARY1_NEL( ACB_NDIM( IACB1 ),
     :                                 ACB_LBND( 1, IACB1 ),
     :                                 ACB_UBND( 1, IACB1 ), EL,
     :                                 STATUS )

*  Test for each possible numeric data type and call the appropriate
*  routine to transfer data from the input array to the output.

*  ...byte data.
                        IF ( DCB_TYP( IDCB1 ) .EQ. '_BYTE' ) THEN
                           CALL VEC_BTOB( .FALSE., EL,
     :                                   %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                                   %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                                   IERR, NERR, STATUS )

*  ...unsigned byte data.
                        ELSE IF ( DCB_TYP( IDCB1 ) .EQ. '_UBYTE' ) THEN
                           CALL VEC_UBTOUB( .FALSE., EL,
     :                                   %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                                   %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                                   IERR, NERR, STATUS )

*  ...double precision data.
                        ELSE IF ( DCB_TYP( IDCB1 ) .EQ. '_DOUBLE' ) THEN
                           CALL VEC_DTOD( .FALSE., EL,
     :                                   %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                                   %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                                   IERR, NERR, STATUS )

*  ...integer data.
                       ELSE IF ( DCB_TYP( IDCB1 ) .EQ. '_INTEGER' ) THEN
                           CALL VEC_ITOI( .FALSE., EL,
     :                                   %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                                   %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                                   IERR, NERR, STATUS )

*  ...real data.
                        ELSE IF ( DCB_TYP( IDCB1 ) .EQ. '_REAL' ) THEN
                           CALL VEC_RTOR( .FALSE., EL,
     :                                   %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                                   %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                                   IERR, NERR, STATUS )

*  ...word data.
                        ELSE IF ( DCB_TYP( IDCB1 ) .EQ. '_WORD' ) THEN
                           CALL VEC_WTOW( .FALSE., EL,
     :                                   %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                                   %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                                   IERR, NERR, STATUS )

*  ...unsigned word data.
                        ELSE IF ( DCB_TYP( IDCB1 ) .EQ. '_UWORD' ) THEN
                           CALL VEC_UWTOUW( .FALSE., EL,
     :                                   %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                                   %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                                   IERR, NERR, STATUS )
                        END IF

*  Unmap the (cloned) input and output arrays.
                        CALL ARY1_UMP( IACBC, STATUS )
                        CALL ARY1_UMP( IACB2, STATUS )
                     END IF

*  Annul the cloned ACB entry.
                     CALL ARY1_ANL( IACBC, STATUS )
                  END IF
               END IF

*  Simple arrays.
*  =============
            ELSE IF ( DCB_FRM( IDCB1 ) .EQ. 'SIMPLE' ) THEN

*  Ensure that data type information is available in the DCB.
               CALL ARY1_DTYP( IDCB1, STATUS )

*  Create a new data object (with an entry in the DCB) with the correct
*  type and bounds to accommodate the copied data.
               CALL ARY1_DCRE( DCB_TYP( IDCB1 ), DCB_CPX( IDCB1 ),
     :                         ACB_NDIM( IACB1 ), ACB_LBND( 1, IACB1 ),
     :                         ACB_UBND( 1, IACB1 ), TEMP, LOC, IDCB2,
     :                         STATUS )

*  Create a new base array entry in the ACB to describe it.
               CALL ARY1_CRNBA( IDCB2, IACB2, STATUS )

*  Ensure that state information for the input array is available in the
*  DCB.
               CALL ARY1_DSTA( IDCB1, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If the input array is in the "defined" state, then its data values
*  must be copied to the output array. Produce a cloned copy of its ACB
*  entry, to allow it to be mapped even if the existing ACB entry is
*  already mapped for access.
                  IF ( DCB_STA( IDCB1 ) ) THEN
                     CALL ARY1_CLN( IACB1, IACBC, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN
      
*  Map the input array for reading through the cloned ACB entry and the
*  output array for writing.
                        CALL ARY1_MAPS( IACBC, DCB_TYP( IDCB1 ),
     :                                  DCB_CPX( IDCB1 ), 'READ',
     :                                  PNTR1( 1 ), PNTR1( 2 ), STATUS )
                        CALL ARY1_MAPS( IACB2, DCB_TYP( IDCB1 ),
     :                                  DCB_CPX( IDCB1 ), 'WRITE',
     :                                  PNTR2( 1 ), PNTR2( 2 ), STATUS )

*  Find the number of data components to be copied and the number of
*  elements in each component.
                        NCOMP = 1
                        IF ( DCB_CPX( IDCB1 ) ) NCOMP = 2
                        CALL ARY1_NEL( ACB_NDIM( IACB1 ),
     :                                 ACB_LBND( 1, IACB1 ),
     :                                 ACB_UBND( 1, IACB1 ), EL,
     :                                 STATUS )

*  Loop to copy each component.
                        DO 1 ICOMP = 1, NCOMP

*  Test for each possible numeric data type and call the appropriate
*  routine to transfer data from the input array component to the
*  output.

*  ...byte data.
                           IF ( DCB_TYP( IDCB1 ) .EQ. '_BYTE' ) THEN
                              CALL VEC_BTOB( .FALSE., EL,
     :                               %VAL( CNF_PVAL( PNTR1( ICOMP ) ) ),
     :                               %VAL( CNF_PVAL( PNTR2( ICOMP ) ) ),
     :                               IERR, NERR, STATUS )

*  ...unsigned byte data.
                           ELSE IF ( DCB_TYP( IDCB1 ) .EQ. '_UBYTE' )
     :                     THEN
                              CALL VEC_UBTOUB( .FALSE., EL,
     :                               %VAL( CNF_PVAL( PNTR1( ICOMP ) ) ),
     :                               %VAL( CNF_PVAL( PNTR2( ICOMP ) ) ),
     :                               IERR, NERR, STATUS )

*  ...double precision data.
                           ELSE IF ( DCB_TYP( IDCB1 ) .EQ. '_DOUBLE' )
     :                     THEN
                              CALL VEC_DTOD( .FALSE., EL,
     :                               %VAL( CNF_PVAL( PNTR1( ICOMP ) ) ),
     :                               %VAL( CNF_PVAL( PNTR2( ICOMP ) ) ),
     :                               IERR, NERR, STATUS )

*  ...integer data.
                          ELSE IF ( DCB_TYP( IDCB1 ) .EQ. '_INTEGER' )
     :                    THEN
                              CALL VEC_ITOI( .FALSE., EL,
     :                               %VAL( CNF_PVAL( PNTR1( ICOMP ) ) ),
     :                               %VAL( CNF_PVAL( PNTR2( ICOMP ) ) ),
     :                               IERR, NERR, STATUS )

*  ...real data.
                           ELSE IF ( DCB_TYP( IDCB1 ) .EQ. '_REAL' )
     :                     THEN
                              CALL VEC_RTOR( .FALSE., EL,
     :                               %VAL( CNF_PVAL( PNTR1( ICOMP ) ) ),
     :                               %VAL( CNF_PVAL( PNTR2( ICOMP ) ) ),
     :                               IERR, NERR, STATUS )

*  ...word data.
                           ELSE IF ( DCB_TYP( IDCB1 ) .EQ. '_WORD' )
     :                     THEN
                              CALL VEC_WTOW( .FALSE., EL,
     :                               %VAL( CNF_PVAL( PNTR1( ICOMP ) ) ),
     :                               %VAL( CNF_PVAL( PNTR2( ICOMP ) ) ),
     :                               IERR, NERR, STATUS )

*  ...unsigned word data.
                           ELSE IF ( DCB_TYP( IDCB1 ) .EQ. '_UWORD' )
     :                     THEN
                              CALL VEC_UWTOUW( .FALSE., EL,
     :                               %VAL( CNF_PVAL( PNTR1( ICOMP ) ) ),
     :                               %VAL( CNF_PVAL( PNTR2( ICOMP ) ) ),
     :                               IERR, NERR, STATUS )
                           END IF
1                       CONTINUE

*  Unmap the (cloned) input and output arrays.
                        CALL ARY1_UMP( IACBC, STATUS )
                        CALL ARY1_UMP( IACB2, STATUS )

*  Transfer the bad pixel flag to the new array.
                        CALL ARY1_BAD( IACBC, .FALSE., BAD, STATUS )
                        CALL ARY1_SBD( BAD, IACB2, STATUS )
                     END IF

*  Annul the cloned ACB entry.
                     CALL ARY1_ANL( IACBC, STATUS )
                  END IF
               END IF

*  If the DCB form entry was not recognised, then report an error.
            ELSE
               STATUS = ARY__FATIN
               CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB1 ) )
               CALL ERR_REP( 'ARY1_CPY_FORM',
     :         'Invalid array form ''^BADFORM'' found in Data ' //
     :         'Control Block (internal programming error).', STATUS )
            END IF
         END IF
      END IF

*  If an error occurred, then annul the new ACB entry and reset the
*  IACB2 argument to zero.
      IF ( ( STATUS .NE. SAI__OK ) .AND. ( IACB2 .NE. 0 ) ) THEN
         CALL ARY1_ANL( IACB2, STATUS )
         IACB2 = 0
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CPY', STATUS )

      END
