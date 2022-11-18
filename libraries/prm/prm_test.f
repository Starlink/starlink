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

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995, 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     AJC: A.J. Chipperfield (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     7-NOV-1991 (RFWS):
*        Original version.
*     27-FEB-1995 (AJC):
*        Use INCLUDE file links
*     6-OCT-2004 (TIMJ):
*        Use new interface to NUM_ERROR
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

*  External References:
      EXTERNAL NUM_TRAP          ! Numerical error handler
      EXTERNAL NUM_WASOK
      INTEGER VAL_DTOI           ! Convert D to I
      LOGICAL NUM_WASOK

*  Local Variables:
      DOUBLE PRECISION D2        ! The value 2.0D0
      DOUBLE PRECISION D2A( 1 )  ! D2 as an array
      INTEGER I0                 ! The value 0
      INTEGER I1                 ! The value 1
      INTEGER I2                 ! The value 2?
      INTEGER I2A( 1 )           ! I2 as an array
      INTEGER IERR               ! Error pointer (junk)
      INTEGER NERR               ! Error count (junk)
      INTEGER NUM_ERR            ! Numerical error status
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
      CALL NUM_CLEARERR()

      WRITE( *, * ) ' Testing divide by zero: 1.0/0.0 = ',
     :              REAL( I1 ) / REAL( I0 )
      IF ( .NOT. NUM_WASOK() ) THEN
         NUM_ERR = SAI__OK
         CALL NUM_GETERR( NUM_ERR )
         WRITE( *, * ) ' NUM_TRAP error handler responding OK.'
         WRITE( *, * ) ' Returns error number ', NUM_ERR
      ELSE
         WRITE( *, * )
     :    ' Warning: NUM_TRAP error handler not responding (not impl?).'
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

*   Use non-standard but common EXIT intrinsic
      IF ( OK ) THEN
         CALL EXIT(0)
      ELSE
         CALL EXIT(1)
      ENDIF

      END
