      SUBROUTINE NDF1_QMLOG( BADBIT, EL, QUAL, LARRAY, BAD, STATUS )
*+
*  Name:
*     NDF1_QMLOG

*  Purpose:
*     Convert a vectorised quality mask into a logical array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_QMLOG( BADBIT, EL, QUAL, LARRAY, BAD, STATUS )

*  Description:
*     The routine converts a vectorised array holding an unsigned byte
*     quality mask into a logical array. The logical values are derived
*     by performing a bit-wise "AND" operation between each quality
*     value and an unsigned byte bad-bits mask and then testing if the
*     result is equal to zero. The resulting logical values are
*     assigned to the output array; .TRUE. means that the corresponding
*     NDF pixel is to be accepted for processing by subsequent
*     algorithms and .FALSE. means that it should be rejected.

*  Arguments:
*     BADBIT = BYTE (Given)
*        The unsigned byte bad-bits mask.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     QUAL( EL ) = BYTE (Given)
*        Array of quality values.
*     LARRAY( EL ) = LOGICAL (Returned)
*        Array of logical values.
*     BAD = LOGICAL (Returned)
*        Whether the quality mask resulted in the rejection of any
*        pixels. This value is set to .TRUE. if any of the LARRAY
*        values returned are set to .FALSE.. Otherwise it is set to
*        .FALSE..
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine loops through the arrays in a backward direction.
*     This is to minimise paging on a virtual memory machine, since
*     this routine will usually be followed by a processing loop which
*     passes through the same arrays in the forward direction.

*  Algorithm:
*     -  Initialise the BAD value.
*     -  If the badbits value is zero, then fill the logical array with
*     .TRUE.  values.
*     -  Otherwise, loop to process each array element.
*     -  Evaluate the quality masking function and assign the result to
*     the logical array.
*     -  Note if any .FALSE. values are generated.
*     -  Having detected a .FALSE. value, further assignments to the
*     BAD argument can be eliminated, so quit the loop.
*     -  If a .FALSE. value has been detected, then process the
*     remaining array elements without making further assignments to
*     the BAD argument.

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
*     1-FEB-1990 (RFWS):
*        Original version.
*     1-MAR-1990 (RFWS):
*        Changed to use the non-generic functions IIAND and IZEXT.
*     20-MAR-1990 (RFWS):
*        Changed to loop through the arrays backwards.
*     20-MAR-1990 (RFWS):
*        Changed to define the quality masking operation through a
*        statement function defined in an include file.
*     {enter_further_changes_here}

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
      LOGICAL LARRAY( EL )
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      BYTE ZEROUB                ! Zero as an unsigned byte value
      PARAMETER ( ZEROUB = 0 )

*  Local Variables:
      INTEGER I                  ! 1st loop counter for array elements
      INTEGER II                 ! 2nd loop counter for array elements

*  Internal References:
      INCLUDE 'NDF_FUNC_DEC'     ! Declare NDF_ statement functions
      INCLUDE 'NDF_FUNC_DEF'     ! Define NDF_ statement functions

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      BAD = .FALSE.

*  If the mask is zero, then fill the logical array with .TRUE. values.
      IF ( BADBIT .EQ. ZEROUB ) THEN
         CALL NDF1_TRUE( EL, LARRAY, STATUS )

*  Loop to process each array element.
      ELSE
         DO 1 I = EL, 1, -1

*  Evaluate the quality masking function and assign the result to the
*  logical array.
            IF ( NDF_QMASK( QUAL( I ), BADBIT ) ) THEN
               LARRAY( I ) = .TRUE.
            ELSE
               LARRAY( I ) = .FALSE.

*  Note if any .FALSE. values are generated.
               BAD = .TRUE.

*  Having detected a .FALSE. value, further assignments to the BAD
*  argument can be eliminated, so quit this loop to process the
*  remaining array elements without further assignments.
               GO TO 2
            END IF
1        CONTINUE
2        CONTINUE

*  If a .FALSE. value has been produced, then process any remaining
*  array elements without making further assignments to the BAD
*  argument.
         IF ( BAD ) THEN
            DO 3 II = I - 1, 1, -1
               IF ( NDF_QMASK( QUAL( II ), BADBIT ) ) THEN
                  LARRAY( II ) = .TRUE.
               ELSE
                  LARRAY( II ) = .FALSE.
               ENDIF
3           CONTINUE
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_QMLOG', STATUS )

      END
