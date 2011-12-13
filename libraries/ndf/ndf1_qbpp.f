      SUBROUTINE NDF1_QBPP( BADBIT, EL, QUAL, BAD, STATUS )
*+
*  Name:
*     NDF1_QBPP

*  Purpose:
*     Determine if a vectorised quality array contains bad pixels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_QBPP( BADBIT, EL, QUAL, BAD, STATUS )

*  Description:
*     The routine examines a vectorised array of unsigned byte quality
*     values using a bad-bits mask and determines if any of the quality
*     values give a non-zero result when a bit-wise "AND" is performed
*     with the mask.

*  Arguments:
*     BADBIT = BYTE (Given)
*        Unsigned byte bad-bits mask.
*     EL = INTEGER (Given)
*        Number of quality array elements to examine.
*     QUAL( EL ) = BYTE (Given)
*        Array of unsigned byte quality values.
*     BAD = LOGICAL (Given)
*        Whether any non-zero result is obtained when performing a
*        bit-wise "AND" of the quality values with the bad-bits mask.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise.
*     -  If the bad-bits value is non-zero, loop to check each array
*     element.
*     -  Evaluate the quality masking function and set BAD if
*     appropriate.
*     -  Quit checking once BAD is set.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-MAR-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      BYTE BADBIT
      INTEGER EL
      BYTE QUAL( EL )

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      BYTE ZEROUB                ! Zero as an unsigned byte value
      PARAMETER ( ZEROUB = 0 )

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements

*  Internal References:
      INCLUDE 'NDF_FUNC_DEC'     ! Declare NDF_ statement functions
      INCLUDE 'NDF_FUNC_DEF'     ! Define NDF_ statement functions

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      BAD = .FALSE.

*  If the bad-bits value is non-zero, loop to check each array element.
      IF ( BADBIT .NE. ZEROUB ) THEN
         DO 1 I = 1, EL

*  Evaluate the quality masking function and set BAD if appropriate.
*  Quit checking once BAD is set.
            IF ( .NOT. NDF_QMASK( QUAL( I ), BADBIT ) ) THEN
               BAD = .TRUE.
               GO TO 2
            END IF
1        CONTINUE
2        CONTINUE
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_QBPP', STATUS )

      END
