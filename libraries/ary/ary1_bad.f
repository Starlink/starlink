      SUBROUTINE ARY1_BAD( IACB, CHECK, BAD, STATUS )
*+
*  Name:
*     ARY1_BAD

*  Purpose:
*     Determine whether bad pixels may be present for an ACB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_BAD( IACB, CHECK, BAD, STATUS )

*  Description:
*     The routine obtains the value of the bad pixel flag for an array
*     with an entry in the ACB, taking account of the possibility that
*     the array may be mapped for access. If requested, an explicit
*     check on the presence of bad pixels will be made.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the array entry in the ACB.
*     CHECK = LOGICAL (Given)
*        If CHECK is set .FALSE., then the routine will simply return
*        the value of the bad pixel flag (indicating whether bad pixels
*        may be present in the array). If CHECK is set .TRUE., then an
*        explicit check will be performed if necessary, to see if bad
*        values are actually present in the data.
*     BAD = LOGICAL (Returned)
*        Bad pixel flag value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Determine if the array is mapped for access.
*     -  If it is, then determine the bad pixel flag value from the MCB
*     entry, taking account of whether the mapping transfer region
*     exists and whether it completely fills the mapping region.
*     -  If bad pixels may be present and an explicit check is required,
*     then perform the check.
*     -  If the array is not mapped, then obtain an index to the data
*     object entry in the DCB and ensure that state information is
*     available for it.
*     -  If the array is undefined, then bad pixels are definitely
*     present. Otherwise, obtain an initial value of the bad pixel flag
*     from the ACB.
*     -  If this value is not .TRUE., then check to see if a data
*     transfer window exists; set the value to .TRUE. if it does not.
*     -  If the value is still not .TRUE., then obtain mapping region
*     bounds information for the array, as if it were going to be
*     mapped.
*     -  Set the bad pixel flag to .TRUE. if the mapping transfer
*     region does not completely fill the mapping region (because
*     accessing the array would cause it to be padded with "bad"
*     values).
*     -  If bad pixels may be present, but this is not certain, and an
*     explicit check is required, then check to see if a data transfer
*     window exists. If not, then the presence of bad pixels is certain,
*     so no further check is needed.
*     -  If a check is still needed, then obtain mapping region bounds
*     information as if the array is to be mapped and see if the
*     mapping transfer region completely fills the mapping region.
*     -  If not, then the presence of bad pixels is certain. If it
*     does, however, then perform an explicit check on the presence of
*     bad pixels.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JUL-1989 (RFWS):
*        Original version.
*     7-SEP-1989 (RFWS):
*        Changed to use new argument list for ARY1_GMRB.
*     21-NOV-1989 (RFWS):
*        Implemented the CHECK option.
*     5-MAR-1990 (RFWS):
*        Added extra code to cater for the case where the array is in an
*        undefined state.
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
*        DCB_STA( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the array's data values are defined

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_BAD( ARY__MXACB ) = LOGICAL (Read)
*           Whether there may be "bad" values in the data transfer
*           window (if it exists).
*        ACB_DTWEX( ARY__MXACB ) = LOGICAL (Read)
*           Whether a data transfer window exists.
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Read)
*           Index to associated MCB entry.

      INCLUDE 'ARY_MCB'          ! ARY_ Mapping Control Block
*        MCB_BAD( ARY__MXMCB ) = LOGICAL (Read)
*           Whether there may be "bad" values in the mapped data lying
*           within the mapping transfer region (if it exists).
*        MCB_MRFUL( ARY__MXMCB ) = LOGICAL (Read)
*           Whether the mapping transfer region completely fills the
*           mapping region.
*        MCB_PBAD( ARY__MXMCB ) = LOGICAL (Read)
*           Whether there may be "bad" values in the mapped data lying
*           within the padding region (if it exists) which surrounds
*           the mapping transfer region.

*  Arguments Given:
      INTEGER IACB
      LOGICAL CHECK

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IMCB               ! Index to entry in MCB
      INTEGER LMRB( ARY__MXDIM ) ! Lower mapping region bounds
      INTEGER LMTR( ARY__MXDIM ) ! Lower mapping transfer region bounds
      INTEGER UMRB( ARY__MXDIM ) ! Upper mapping region bounds
      INTEGER UMTR( ARY__MXDIM ) ! Upper mapping transfer region bounds
      LOGICAL MRFULL             ! Mapping region full of data?
      LOGICAL MTREX              ! Mapping transfer region exists?
      LOGICAL SURE               ! Whether bad pixel presence is certain
      LOGICAL WHOLE              ! Mapping region is whole object?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the index to the associated MCB entry. A positive value
*  indicates that the array is currently mapped for access.
      IMCB = ACB_IMCB( IACB )
      IF ( IMCB .GT. 0 ) THEN

*  If it is mapped and the mapping transfer region completely fills the
*  mapping region, then the value of the bad pixel flag is determined
*  by whether there may be "bad" values in the mapping transfer region.
         IF ( MCB_MRFUL( IMCB ) ) THEN
            BAD = MCB_BAD( IMCB )

*  If the mapping transfer region does not completely fill the mapping
*  region, but a mapping transfer region does exist, then account must
*  also be taken of whether there may be "bad" values in the padding
*  region which surrounds the mapping transfer region.
         ELSE IF ( MCB_MTREX( IMCB ) ) THEN
            BAD = MCB_BAD( IMCB ) .OR. MCB_PBAD( IMCB )

*  If no mapping transfer region exists, then only the padding region
*  need be considered.
         ELSE
            BAD = MCB_PBAD( IMCB )
         END IF

*  If the mapped data may contain bad pixels and an explicit check is
*  required, then perform this check.
         IF ( BAD .AND. CHECK ) THEN
            CALL ARY1_CHBPP( IACB, BAD, STATUS )
         END IF

*  If the array is not mapped for access, then obtain an index to the
*  data object entry in the DCB and ensure that state information is
*  available for it.
      ELSE
         IDCB = ACB_IDCB( IACB )
         CALL ARY1_DSTA( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the array is in an undefined state, then there are definitely bad
*  pixels present.
            IF ( .NOT. DCB_STA( IDCB ) ) THEN
               BAD = .TRUE.
               SURE = .TRUE.

*  Otherwise, obtain an initial value of the bad pixel flag from the
*  ACB entry. This indicates whether there may be bad values in the
*  data transfer window (if it exists). Bad pixels are not certain to
*  be present, however.
            ELSE
               BAD = ACB_BAD( IACB )
               SURE = .FALSE.
            END IF

*  If BAD is not .TRUE., then check whether a data transfer window
*  exists; the bad pixel flag is .TRUE. if it does not. In this case,
*  the presence of bad pixels is certain.
            IF ( .NOT. BAD ) THEN
               BAD = .NOT. ACB_DTWEX( IACB )
               SURE = .TRUE.

*  If the bad pixel flag is still not .TRUE., then a further check must
*  be made to see whether the data would be padded (with "bad" values)
*  if it were accessed. Obtain mapping region bounds information for
*  the ACB entry, as if the array were going to be mapped.
               IF ( .NOT. BAD ) THEN
                  CALL ARY1_GMRB( IACB, MTREX, MRFULL, WHOLE,
     :                            LMRB, UMRB, LMTR, UMTR, STATUS )

*  The bad pixel flag is .TRUE. if the mapping region is not completely
*  filled by the mapping transfer region. In this case, the presence of
*  bad pixels is certain.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     BAD = .NOT. MRFULL
                     SURE = .TRUE.
                  END IF
               END IF
            END IF
         END IF

*  If bad pixels may be present, but not for certain, and an explicit
*  check is required, then check to see if a data transfer window
*  exists. If so, then the presence of bad pixels is certain, so no
*  check need be performed.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( BAD .AND. ( .NOT. SURE ) .AND. CHECK ) THEN
               IF ( ACB_DTWEX( IACB ) ) THEN

*  Obtain mapping region bounds information as if the array were going
*  to be mapped.
                  CALL ARY1_GMRB( IACB, MTREX, MRFULL, WHOLE,
     :                            LMRB, UMRB, LMTR, UMTR, STATUS )

*  If the mapping transfer region does not completely fill the mapping
*  region, then the presence of bad pixels is certain, so no check need
*  be made.  However, if there is still uncertainty, then perform an
*  explicit check for bad pixels.
                  IF ( MRFULL ) THEN
                     CALL ARY1_CHBPP( IACB, BAD, STATUS )
                  END IF
               END IF
            END IF
         END IF
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_BAD', STATUS )

      END
