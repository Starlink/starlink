      SUBROUTINE ARY1_CHBPP( IACB, BAD, STATUS )
*+
*  Name:
*     ARY1_CHBPP

*  Purpose:
*     Perform an explicit check that bad pixels are present in an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_CHBPP( IACB, BAD, STATUS )

*  Description:
*     The routine checks explicitly that bad pixels are present in an
*     array identified by its ACB entry. It is intended to be called
*     once checks on the array's bad pixel flag have indicated that bad
*     pixels may be present, but a further check is required that bad
*     values actually appear in the data (they may not be present, for
*     instance, if the array is a section taken from a larger array and
*     it happens to avoid all the bad pixels).

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the array entry in the ACB.
*     BAD = LOGICAL (Returned)
*        A .TRUE. value is returned only if bad values are actually
*        present in the array's data. Otherwise, .FALSE. is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the array is mapped for access when this routine is called,
*     then checking will be performed on the actual mapped data values
*     (i.e. on the region of memory containing the data passed to the
*     application). An imaginary array component may therefore be
*     omitted from this check if mapped access to non-complex data is
*     in effect.
*     -  If the array is not mapped for access, then it will first be
*     mapped using 'READ' access and the data type of the associated
*     data object (to avoid type conversion) before performing the
*     check for bad pixels. In this case, any imaginary component
*     present will also be checked.
*     -  A BAD value of .TRUE. will be returned if the array is not
*     mapped for access and is in the "undefined" state.

*  Algorithm:
*     -  Calculate the number of data elements in the array.
*     -  Obtain an index to the array's mapping entry in the MCB.
*     -  If the array is mapped for access, then check the non-imaginary
*     mapped component for the presence of bad pixels.
*     -  If mapped access to complex data is in effect and no bad pixels
*     have so far been found, then check the mapped imaginary component
*     in the same way.
*     -  If the array is not mapped for access, then obtain an index to
*     the data object entry in the DCB.
*     -  Ensure that data type and state information is available for
*     the data object in the DCB.
*     -  If the array is in the undefined state, then return a value of
*     BAD=.TRUE..
*     -  Otherwise, map the array for read access, using the intrinsic
*     numeric type of the data object to avoid data type conversion.
*     -  If mapping failed, then make an additional context error
*     report.
*     -  Examine the non-imaginary component of the mapped data for bad
*     pixels.
*     -  If the data object holds complex data and no bad pixels have so
*     far been found, then examine the imaginary component of the mapped
*     data in the same way.
*     -  Unmap the array.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-NOV-1989 (RFWS):
*        Original version.
*     7-MAR-1990 (RFWS):
*        Added context error report if access to the array's values
*        cannot be obtained.
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
*        DCB_CPX( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the data object holds complex data.
*        DCB_STA( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the object's data values are defined.
*        DCB_TYP( ARY__MXDCB ) = CHARACTER * ( ARY__SZTYP ) (Read)
*           Numeric type of data object.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to the data object entry in the DCB.
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Read)
*           Index to the mapping entry in the MCB.
*        ACB_LBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Lower bounds of array access region.
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Read)
*           Number of array dimensions.
*        ACB_UBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Upper bounds of array access region.

      INCLUDE 'ARY_MCB'          ! ARY_ Mapping Control Block
*        MCB_CPX( ARY__MXMCB ) = LOGICAL (Read)
*           Whether mapped access is to complex data.
*        MCB_DPNTR( ARY__MXMCB ) = INTEGER (Read)
*           Pointer to non-imaginary component of mapped data.
*        MCB_IPNTR( ARY__MXMCB ) = INTEGER (Read)
*           Pointer to imaginary component of mapped data.
*        MCB_TYP( ARY__MXMCB ) = CHARACTER * ( ARY__SZTYP ) (Read)
*           Numeric data type for mapped access.

*  Arguments Given:
      INTEGER IACB

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DPNTR              ! Pointer to mapped non-imaginary data
      INTEGER EL                 ! Number of data elements in the array
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IMCB               ! Index to mapping entry in the MCB
      INTEGER IPNTR              ! Pointer to mapped imaginary data

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the number of data elements in the array.
      CALL ARY1_NEL( ACB_NDIM( IACB ), ACB_LBND( 1, IACB ),
     :               ACB_UBND( 1, IACB ), EL, STATUS )

*  Obtain an index to the array's mapping entry in the MCB.
      IMCB = ACB_IMCB( IACB )

*  If this index is positive, then the array is currently mapped for
*  access. Check the non-imaginary component of the mapped data to see
*  if bad pixels are present.
      IF ( IMCB .GT. 0 ) THEN
         CALL ARY1_BPP( MCB_TYP( IMCB ), EL, MCB_DPNTR( IMCB ), BAD,
     :                  STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the array is mapped for access to complex data, and no bad pixels
*  have yet been found, then the imaginary component of the mapped data
*  must be checked in the same way.
            IF ( MCB_CPX( IMCB ) .AND. ( .NOT. BAD ) ) THEN
               CALL ARY1_BPP( MCB_TYP( IMCB ), EL, MCB_IPNTR( IMCB ),
     :                        BAD, STATUS )
            END IF
         END IF

*  If the array is not mapped for access, then obtain an index to the
*  data object entry in the DCB.
      ELSE
         IDCB = ACB_IDCB( IACB )

*  Ensure that data type and state information is available for the data
*  object in the DCB.
         CALL ARY1_DTYP( IDCB, STATUS )
         CALL ARY1_DSTA( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the data object is in the "undefined" state, then BAD is
*  automatically set .TRUE..
            IF ( .NOT. DCB_STA( IDCB ) ) THEN
               BAD = .TRUE.

*  Otherwise, map the array for read access.
            ELSE
               CALL ARY1_MAPS( IACB, DCB_TYP( IDCB ), DCB_CPX( IDCB ),
     :                         'READ', DPNTR, IPNTR, STATUS )

*  If access could not be obtained, then add context information to the
*  error report.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( 'ARY1_CHBPP_ACC',
     :            'Unable to access array values to check for bad ' //
     :            'pixels.', STATUS )
               END IF

*  Examine the non-imaginary mapped data for bad pixels.
               CALL ARY1_BPP( DCB_TYP( IDCB ), EL, DPNTR, BAD, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If the array is complex, and no bad pixels have yet been found, then
*  the imaginary component of the mapped data must be examined in the
*  same way.
                  IF ( DCB_CPX( IDCB ) .AND. ( .NOT. BAD ) ) THEN
                     CALL ARY1_BPP( DCB_TYP( IDCB ), EL, IPNTR, BAD,
     :                              STATUS )
                  END IF
               END IF

*  Unmap the array.
               CALL ARY1_UMPS( IACB, STATUS )
            END IF
         END IF
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CHBPP', STATUS )

      END
