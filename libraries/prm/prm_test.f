      PROGRAM PRM_TEST
*+
*  Name:
*     PRM_TEST

*  Purpose:
*     Test installation of the PRIMDAT system.

*  Language:
*     Starlink Fortran 77

*  Description:
*     This program may be used to test for correct installation of the
*     PRIMDAT system. Note that it is not an exhaustive test of the
*     system, but only of its installation.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     AJC: A.J. Chipperfield (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     7-NOV-1991 (RFWS):
*        Original version.
*     27-FEB-1995 (AJC):
*        Use INCLUDE file links
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INCLUDE 'PRM_PAR'          ! PRIMDAT constants (test existence)

      INCLUDE 'PRM_ERR'          ! PRIMDAT error codes (test existence)


*  Global Variables:
      INCLUDE 'NUM_CMN'          ! Define NUM_ERROR flag


*  External References:
      EXTERNAL NUM_TRAP          ! Numerical error handler
      INTEGER VAL_DTOI           ! Convert D to I

*  Local Variables:
      DOUBLE PRECISION D2        ! The value 2.0D0
      DOUBLE PRECISION D2A( 1 )  ! D2 as an array
      INTEGER I0                 ! The value 0
      INTEGER I1                 ! The value 1
      INTEGER I2                 ! The value 2?
      INTEGER I2A( 1 )           ! I2 as an array
      INTEGER IERR               ! Error pointer (junk)
      INTEGER NERR               ! Error count (junk)
      INTEGER STATUS             ! Status variable
      LOGICAL OK                 ! Test succeeded?

*  Internal References:
      INCLUDE 'NUM_DEC'          ! Declare NUM_ functions

      INCLUDE 'NUM_DEF'          ! Define NUM_ functions


*  Local Data:
      DATA I0, I1, D2 / 0, 1, 2.0D0 /

*.

*  Initialise status and success flag.
      STATUS = SAI__OK
      OK = .TRUE.

*  Perform a type conversion using NUM_DTOI and check that it works.
      WRITE( *, * )
      IF ( NUM_DTOI( D2 ) .EQ. 2 ) THEN
         WRITE( *, * ) ' NUM_ conversion routines OK.'
      ELSE
         WRITE( *, * )
     :    ' Error: NUM_ conversion routines not functioning correctly.'
         OK = .FALSE.
      END IF

*  Perform the same conversion using VAL_DTOI and check that it works.
      IF ( VAL_DTOI( .FALSE., D2, STATUS ) .EQ. 2 ) THEN
         WRITE( *, * ) ' VAL_ conversion routines OK.'
      ELSE
         WRITE( *, * )
     :    ' Error: VAL_ conversion routines not functioning correctly.'
         OK = .FALSE.
      END IF

*  Perform the same conversion using VEC_DTOI and check that it works.
      D2A( 1 ) = D2
      I2A( 1 ) = 0
      CALL VEC_DTOI( .FALSE., 1, D2A, I2A, IERR, NERR, STATUS )
      IF ( I2A( 1 ) .EQ. 2 ) THEN
         WRITE( *, * ) ' VEC_ conversion routines OK.'
      ELSE
         WRITE( *, * )
     :    ' Error: VEC_ conversion routines not functioning correctly.'
         OK = .FALSE.
      END IF

*  Establish the error handler and perform a divide by zero. Check that
*  the error flag gets set correctly.
      CALL NUM_HANDL( NUM_TRAP )
      NUM_ERROR = SAI__OK
      WRITE( *, * ) ' Testing divide by zero: 1.0/0.0 = ',
     :              REAL( I1 ) / REAL( I0 )
      IF ( NUM_ERROR .NE. SAI__OK ) THEN
         WRITE( *, * ) ' NUM_TRAP error handler responding OK.'
         WRITE( *, * ) ' Returns error number ', NUM_ERROR
      ELSE
         WRITE( *, * )
     :    ' Error: NUM_TRAP error handler not responding.'
         OK = .FALSE.
      END IF
      CALL NUM_REVRT

*  Say whether the test program succeeded or failed.
      WRITE( *, * )
      IF ( OK ) THEN
         WRITE( *, * )
     :    ' PRIMDAT installation test completed successfully.'
      ELSE
         WRITE( *, * )
     :    ' PRIMDAT installation test ***FAILED***'
      END IF
      WRITE( *, * )

      END      
