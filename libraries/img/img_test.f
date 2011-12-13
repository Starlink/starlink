      SUBROUTINE IMG_TEST( STATUS )
*+
*  Name:
*     IMG_TEST

*  Purpose:
*     Test installation of IMG.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IMG_TEST( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This program should be run after building and installing IMG in
*     order to test for correct installation. Note that this is not an
*     exhaustive test of IMG, but only of its installation.

*  Usage:
*     IMG_TEST [IMAGE]

*  ADAM Parameters:
*     IMAGE = NDF (Update)
*        Name of the scratch image to be created by the test program
*        [img_test].

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-MAR-1993 (RFWS):
*        Original version.
*     7-JUL-1994 (PDRAPER):
*        Changed IMG_DELET call to IMG_CANCL.
*     7-DEC_1995 (PDRAPER):
*        Changed to delete the NDF.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL
      INCLUDE 'IMG_ERR'          ! IMG_ error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ISUM               ! Sum of array elements
      INTEGER NX                 ! First dimensions size of image
      INTEGER NY                 ! Second dimension size of image
      INTEGER PNTR( 1 )          ! Pointer to mapped data
      LOGICAL OLDFLG             ! NDF error tracing status

*  Local Data:
      DATA NX, NY / 10, 20 /

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Enable NDF error-tracing in case something goes wrong.
      CALL NDF_TRACE( .TRUE., OLDFLG )

*  Create a new _REAL image.
      CALL IMG_NEW( 'IMAGE', NX, NY, PNTR( 1 ), STATUS )

*  Initialise the array.
      CALL SETUP( NX * NY, %VAL( CNF_PVAL(PNTR( 1 )) ), STATUS )

*  Free the image.
      CALL IMG_FREE( 'IMAGE', STATUS )

*  Re-open the image as _INTEGER.
      CALL IMG_INI( 'IMAGE', NX, NY, PNTR( 1 ), STATUS )

*  Sum the data elements.
      CALL SUM( NX * NY, %VAL( CNF_PVAL(PNTR( 1 ) )), ISUM, STATUS )

*   Release the image.
      CALL IMG_FREE( 'IMAGE', STATUS )

*  Now arrange to delete the NDF (need to re-open with MOD access).
      CALL IMG_MOD( 'IMAGE', NX, NY, PNTR( 1 ), STATUS )
      CALL IMG_DELET( 'IMAGE', STATUS )

*  Check if the test ran OK. If so, then report success.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( ISUM .EQ. 20100 ) ) THEN
         WRITE( *, * ) '   IMG installation test succeeded.'

*  Otherwise, report an error.
      ELSE
         IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
         CALL ERR_REP( 'IMG_TEST_ERR',
     :   'IMG_TEST: IMG installation test failed.', STATUS )
      END IF

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

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

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

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      ISUM = 0

*  Sum the array elements.
      DO 1 I = 1, EL
         ISUM = ISUM + ARRAY( I )
 1    CONTINUE

      END
* $Id$
