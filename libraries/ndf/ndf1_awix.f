      SUBROUTINE NDF1_AWIB( LBND, UBND, DATA, WIDTH, STATUS )
*+
*  Name:
*     NDF1_AWIB

*  Purpose:
*     Initialise a BYTE axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWIB( LBND, UBND, DATA, WIDTH, STATUS )

*  Description:
*     The routine initialises a BYTE axis width array.  The values
*     assigned are calculated from an associated axis data array giving
*     the positions of the pixel centres by forming differences between
*     the centre positions of neighbouring pixels.

*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     DATA( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of axis data values (containing pixel centre coordinates)
*        from which the axis width values should be derived.
*     WIDTH( LBND : UBND ) = BYTE (Returned)
*        The BYTE axis width array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the width values cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Determine the maximum axis width value that can be stored
*     without overflow.
*     -  Loop to assign values to the array elements.
*     -  If there is a pixel on either side of the current pixel, then
*     use half the separation of their centres to calculate the current
*     pixel width.
*     -  If there is a pixel on only one side, then use the distance
*     from the current pixel to calculate the width.
*     -  If there is only one pixel, then use a width of unity.
*     -  Ensure the result is positive.  If it can be stored without
*     overflow, then assign it to the width array.
*     -  Otherwise, assign a bad result.
*     -  If this is the first such error, then report it.
*     -  Note that overflow has occurred.

*  Copyright:
*     Copyright (C) 1990, 1992 Science & Engineering Research Council.
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
*     18-OCT-1990 (RFWS):
*        Original version.
*     19-OCT-1990 (RFWS):
*        Fixed bug in checking array index against bounds.
*     9-NOV-1990 (RFWS):
*        Changed the routine name and altered to perform initialisation,
*        rather than extrapolation.
*     17-JAN-1992 (RFWS):
*        Changed to make explicit checks on width values to avoid
*        overflow.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER LBND
      INTEGER UBND
      DOUBLE PRECISION DATA( LBND : UBND )

*  Arguments Returned:
      BYTE WIDTH( LBND : UBND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION HI        ! Maximum value which can be stored
      DOUBLE PRECISION W         ! Width value
      INTEGER I                  ! Loop counter for array elements
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the maximum axis width value that can be stored without
*  overflow.
      HI = NUM_BTOD( NUM__MAXB )

*  Loop to assign values to the array elements.
      DO 1 I = LBND, UBND

*  If there is a pixel on either side of the current pixel, then use
*  half the separation of their centres to calculate the current pixel
*  width.
         IF ( ( I .GT. LBND ) .AND. ( I .LT. UBND ) ) THEN
            W = 0.5D0 * ( DATA( I + 1 ) - DATA( I - 1 ) )

*  If there is a pixel on only one side, then use the distance from the
*  current pixel to calculate the width.
         ELSE IF ( I .LT. UBND ) THEN
            W = DATA( I + 1 ) - DATA( I )
         ELSE IF ( I .GT. LBND ) THEN
            W = DATA( I ) - DATA( I - 1 )

*  If there is only one pixel, then use a width of unity.
         ELSE
            W = 1.0D0
         END IF

*  Ensure the result is positive.  If it can be stored without
*  overflow, then assign it to the width array.
         IF ( W .LT. 0.0D0 ) W = - W
         IF ( W .LE. HI ) THEN
            WIDTH( I ) = NUM_DTOB( W )

*  Otherwise, assign a bad result.
         ELSE
            WIDTH( I ) = VAL__BADB

*  If this is the first such error, then report it.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETI( 'PIXEL', I )
               CALL ERR_REP( 'NDF1_AWIB_AXVAL',
     :                       'Overflow occurred while calculating ' //
     :                       'a value for pixel ^PIXEL of an axis ' //
     :                       'width array.', STATUS )

*  Note that overflow has occurred.
               OVFLOW = .TRUE.
            END IF
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWIB',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AWIUB( LBND, UBND, DATA, WIDTH, STATUS )
*+
*  Name:
*     NDF1_AWIUB

*  Purpose:
*     Initialise a UNSIGNED BYTE axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWIUB( LBND, UBND, DATA, WIDTH, STATUS )

*  Description:
*     The routine initialises a UNSIGNED BYTE axis width array.  The values
*     assigned are calculated from an associated axis data array giving
*     the positions of the pixel centres by forming differences between
*     the centre positions of neighbouring pixels.

*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     DATA( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of axis data values (containing pixel centre coordinates)
*        from which the axis width values should be derived.
*     WIDTH( LBND : UBND ) = BYTE (Returned)
*        The UNSIGNED BYTE axis width array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the width values cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Determine the maximum axis width value that can be stored
*     without overflow.
*     -  Loop to assign values to the array elements.
*     -  If there is a pixel on either side of the current pixel, then
*     use half the separation of their centres to calculate the current
*     pixel width.
*     -  If there is a pixel on only one side, then use the distance
*     from the current pixel to calculate the width.
*     -  If there is only one pixel, then use a width of unity.
*     -  Ensure the result is positive.  If it can be stored without
*     overflow, then assign it to the width array.
*     -  Otherwise, assign a bad result.
*     -  If this is the first such error, then report it.
*     -  Note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-OCT-1990 (RFWS):
*        Original version.
*     19-OCT-1990 (RFWS):
*        Fixed bug in checking array index against bounds.
*     9-NOV-1990 (RFWS):
*        Changed the routine name and altered to perform initialisation,
*        rather than extrapolation.
*     17-JAN-1992 (RFWS):
*        Changed to make explicit checks on width values to avoid
*        overflow.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER LBND
      INTEGER UBND
      DOUBLE PRECISION DATA( LBND : UBND )

*  Arguments Returned:
      BYTE WIDTH( LBND : UBND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION HI        ! Maximum value which can be stored
      DOUBLE PRECISION W         ! Width value
      INTEGER I                  ! Loop counter for array elements
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the maximum axis width value that can be stored without
*  overflow.
      HI = NUM_UBTOD( NUM__MAXUB )

*  Loop to assign values to the array elements.
      DO 1 I = LBND, UBND

*  If there is a pixel on either side of the current pixel, then use
*  half the separation of their centres to calculate the current pixel
*  width.
         IF ( ( I .GT. LBND ) .AND. ( I .LT. UBND ) ) THEN
            W = 0.5D0 * ( DATA( I + 1 ) - DATA( I - 1 ) )

*  If there is a pixel on only one side, then use the distance from the
*  current pixel to calculate the width.
         ELSE IF ( I .LT. UBND ) THEN
            W = DATA( I + 1 ) - DATA( I )
         ELSE IF ( I .GT. LBND ) THEN
            W = DATA( I ) - DATA( I - 1 )

*  If there is only one pixel, then use a width of unity.
         ELSE
            W = 1.0D0
         END IF

*  Ensure the result is positive.  If it can be stored without
*  overflow, then assign it to the width array.
         IF ( W .LT. 0.0D0 ) W = - W
         IF ( W .LE. HI ) THEN
            WIDTH( I ) = NUM_DTOUB( W )

*  Otherwise, assign a bad result.
         ELSE
            WIDTH( I ) = VAL__BADUB

*  If this is the first such error, then report it.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETI( 'PIXEL', I )
               CALL ERR_REP( 'NDF1_AWIUB_AXVAL',
     :                       'Overflow occurred while calculating ' //
     :                       'a value for pixel ^PIXEL of an axis ' //
     :                       'width array.', STATUS )

*  Note that overflow has occurred.
               OVFLOW = .TRUE.
            END IF
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWIUB',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AWID( LBND, UBND, DATA, WIDTH, STATUS )
*+
*  Name:
*     NDF1_AWID

*  Purpose:
*     Initialise a DOUBLE PRECISION axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWID( LBND, UBND, DATA, WIDTH, STATUS )

*  Description:
*     The routine initialises a DOUBLE PRECISION axis width array.  The values
*     assigned are calculated from an associated axis data array giving
*     the positions of the pixel centres by forming differences between
*     the centre positions of neighbouring pixels.

*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     DATA( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of axis data values (containing pixel centre coordinates)
*        from which the axis width values should be derived.
*     WIDTH( LBND : UBND ) = DOUBLE PRECISION (Returned)
*        The DOUBLE PRECISION axis width array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the width values cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Determine the maximum axis width value that can be stored
*     without overflow.
*     -  Loop to assign values to the array elements.
*     -  If there is a pixel on either side of the current pixel, then
*     use half the separation of their centres to calculate the current
*     pixel width.
*     -  If there is a pixel on only one side, then use the distance
*     from the current pixel to calculate the width.
*     -  If there is only one pixel, then use a width of unity.
*     -  Ensure the result is positive.  If it can be stored without
*     overflow, then assign it to the width array.
*     -  Otherwise, assign a bad result.
*     -  If this is the first such error, then report it.
*     -  Note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-OCT-1990 (RFWS):
*        Original version.
*     19-OCT-1990 (RFWS):
*        Fixed bug in checking array index against bounds.
*     9-NOV-1990 (RFWS):
*        Changed the routine name and altered to perform initialisation,
*        rather than extrapolation.
*     17-JAN-1992 (RFWS):
*        Changed to make explicit checks on width values to avoid
*        overflow.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER LBND
      INTEGER UBND
      DOUBLE PRECISION DATA( LBND : UBND )

*  Arguments Returned:
      DOUBLE PRECISION WIDTH( LBND : UBND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION HI        ! Maximum value which can be stored
      DOUBLE PRECISION W         ! Width value
      INTEGER I                  ! Loop counter for array elements
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the maximum axis width value that can be stored without
*  overflow.
      HI = NUM_DTOD( NUM__MAXD )

*  Loop to assign values to the array elements.
      DO 1 I = LBND, UBND

*  If there is a pixel on either side of the current pixel, then use
*  half the separation of their centres to calculate the current pixel
*  width.
         IF ( ( I .GT. LBND ) .AND. ( I .LT. UBND ) ) THEN
            W = 0.5D0 * ( DATA( I + 1 ) - DATA( I - 1 ) )

*  If there is a pixel on only one side, then use the distance from the
*  current pixel to calculate the width.
         ELSE IF ( I .LT. UBND ) THEN
            W = DATA( I + 1 ) - DATA( I )
         ELSE IF ( I .GT. LBND ) THEN
            W = DATA( I ) - DATA( I - 1 )

*  If there is only one pixel, then use a width of unity.
         ELSE
            W = 1.0D0
         END IF

*  Ensure the result is positive.  If it can be stored without
*  overflow, then assign it to the width array.
         IF ( W .LT. 0.0D0 ) W = - W
         IF ( W .LE. HI ) THEN
            WIDTH( I ) = NUM_DTOD( W )

*  Otherwise, assign a bad result.
         ELSE
            WIDTH( I ) = VAL__BADD

*  If this is the first such error, then report it.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETI( 'PIXEL', I )
               CALL ERR_REP( 'NDF1_AWID_AXVAL',
     :                       'Overflow occurred while calculating ' //
     :                       'a value for pixel ^PIXEL of an axis ' //
     :                       'width array.', STATUS )

*  Note that overflow has occurred.
               OVFLOW = .TRUE.
            END IF
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWID',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AWII( LBND, UBND, DATA, WIDTH, STATUS )
*+
*  Name:
*     NDF1_AWII

*  Purpose:
*     Initialise a INTEGER axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWII( LBND, UBND, DATA, WIDTH, STATUS )

*  Description:
*     The routine initialises a INTEGER axis width array.  The values
*     assigned are calculated from an associated axis data array giving
*     the positions of the pixel centres by forming differences between
*     the centre positions of neighbouring pixels.

*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     DATA( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of axis data values (containing pixel centre coordinates)
*        from which the axis width values should be derived.
*     WIDTH( LBND : UBND ) = INTEGER (Returned)
*        The INTEGER axis width array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the width values cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Determine the maximum axis width value that can be stored
*     without overflow.
*     -  Loop to assign values to the array elements.
*     -  If there is a pixel on either side of the current pixel, then
*     use half the separation of their centres to calculate the current
*     pixel width.
*     -  If there is a pixel on only one side, then use the distance
*     from the current pixel to calculate the width.
*     -  If there is only one pixel, then use a width of unity.
*     -  Ensure the result is positive.  If it can be stored without
*     overflow, then assign it to the width array.
*     -  Otherwise, assign a bad result.
*     -  If this is the first such error, then report it.
*     -  Note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-OCT-1990 (RFWS):
*        Original version.
*     19-OCT-1990 (RFWS):
*        Fixed bug in checking array index against bounds.
*     9-NOV-1990 (RFWS):
*        Changed the routine name and altered to perform initialisation,
*        rather than extrapolation.
*     17-JAN-1992 (RFWS):
*        Changed to make explicit checks on width values to avoid
*        overflow.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER LBND
      INTEGER UBND
      DOUBLE PRECISION DATA( LBND : UBND )

*  Arguments Returned:
      INTEGER WIDTH( LBND : UBND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION HI        ! Maximum value which can be stored
      DOUBLE PRECISION W         ! Width value
      INTEGER I                  ! Loop counter for array elements
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the maximum axis width value that can be stored without
*  overflow.
      HI = NUM_ITOD( NUM__MAXI )

*  Loop to assign values to the array elements.
      DO 1 I = LBND, UBND

*  If there is a pixel on either side of the current pixel, then use
*  half the separation of their centres to calculate the current pixel
*  width.
         IF ( ( I .GT. LBND ) .AND. ( I .LT. UBND ) ) THEN
            W = 0.5D0 * ( DATA( I + 1 ) - DATA( I - 1 ) )

*  If there is a pixel on only one side, then use the distance from the
*  current pixel to calculate the width.
         ELSE IF ( I .LT. UBND ) THEN
            W = DATA( I + 1 ) - DATA( I )
         ELSE IF ( I .GT. LBND ) THEN
            W = DATA( I ) - DATA( I - 1 )

*  If there is only one pixel, then use a width of unity.
         ELSE
            W = 1.0D0
         END IF

*  Ensure the result is positive.  If it can be stored without
*  overflow, then assign it to the width array.
         IF ( W .LT. 0.0D0 ) W = - W
         IF ( W .LE. HI ) THEN
            WIDTH( I ) = NUM_DTOI( W )

*  Otherwise, assign a bad result.
         ELSE
            WIDTH( I ) = VAL__BADI

*  If this is the first such error, then report it.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETI( 'PIXEL', I )
               CALL ERR_REP( 'NDF1_AWII_AXVAL',
     :                       'Overflow occurred while calculating ' //
     :                       'a value for pixel ^PIXEL of an axis ' //
     :                       'width array.', STATUS )

*  Note that overflow has occurred.
               OVFLOW = .TRUE.
            END IF
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWII',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AWIR( LBND, UBND, DATA, WIDTH, STATUS )
*+
*  Name:
*     NDF1_AWIR

*  Purpose:
*     Initialise a REAL axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWIR( LBND, UBND, DATA, WIDTH, STATUS )

*  Description:
*     The routine initialises a REAL axis width array.  The values
*     assigned are calculated from an associated axis data array giving
*     the positions of the pixel centres by forming differences between
*     the centre positions of neighbouring pixels.

*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     DATA( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of axis data values (containing pixel centre coordinates)
*        from which the axis width values should be derived.
*     WIDTH( LBND : UBND ) = REAL (Returned)
*        The REAL axis width array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the width values cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Determine the maximum axis width value that can be stored
*     without overflow.
*     -  Loop to assign values to the array elements.
*     -  If there is a pixel on either side of the current pixel, then
*     use half the separation of their centres to calculate the current
*     pixel width.
*     -  If there is a pixel on only one side, then use the distance
*     from the current pixel to calculate the width.
*     -  If there is only one pixel, then use a width of unity.
*     -  Ensure the result is positive.  If it can be stored without
*     overflow, then assign it to the width array.
*     -  Otherwise, assign a bad result.
*     -  If this is the first such error, then report it.
*     -  Note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-OCT-1990 (RFWS):
*        Original version.
*     19-OCT-1990 (RFWS):
*        Fixed bug in checking array index against bounds.
*     9-NOV-1990 (RFWS):
*        Changed the routine name and altered to perform initialisation,
*        rather than extrapolation.
*     17-JAN-1992 (RFWS):
*        Changed to make explicit checks on width values to avoid
*        overflow.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER LBND
      INTEGER UBND
      DOUBLE PRECISION DATA( LBND : UBND )

*  Arguments Returned:
      REAL WIDTH( LBND : UBND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION HI        ! Maximum value which can be stored
      DOUBLE PRECISION W         ! Width value
      INTEGER I                  ! Loop counter for array elements
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the maximum axis width value that can be stored without
*  overflow.
      HI = NUM_RTOD( NUM__MAXR )

*  Loop to assign values to the array elements.
      DO 1 I = LBND, UBND

*  If there is a pixel on either side of the current pixel, then use
*  half the separation of their centres to calculate the current pixel
*  width.
         IF ( ( I .GT. LBND ) .AND. ( I .LT. UBND ) ) THEN
            W = 0.5D0 * ( DATA( I + 1 ) - DATA( I - 1 ) )

*  If there is a pixel on only one side, then use the distance from the
*  current pixel to calculate the width.
         ELSE IF ( I .LT. UBND ) THEN
            W = DATA( I + 1 ) - DATA( I )
         ELSE IF ( I .GT. LBND ) THEN
            W = DATA( I ) - DATA( I - 1 )

*  If there is only one pixel, then use a width of unity.
         ELSE
            W = 1.0D0
         END IF

*  Ensure the result is positive.  If it can be stored without
*  overflow, then assign it to the width array.
         IF ( W .LT. 0.0D0 ) W = - W
         IF ( W .LE. HI ) THEN
            WIDTH( I ) = NUM_DTOR( W )

*  Otherwise, assign a bad result.
         ELSE
            WIDTH( I ) = VAL__BADR

*  If this is the first such error, then report it.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETI( 'PIXEL', I )
               CALL ERR_REP( 'NDF1_AWIR_AXVAL',
     :                       'Overflow occurred while calculating ' //
     :                       'a value for pixel ^PIXEL of an axis ' //
     :                       'width array.', STATUS )

*  Note that overflow has occurred.
               OVFLOW = .TRUE.
            END IF
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWIR',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AWIW( LBND, UBND, DATA, WIDTH, STATUS )
*+
*  Name:
*     NDF1_AWIW

*  Purpose:
*     Initialise a WORD axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWIW( LBND, UBND, DATA, WIDTH, STATUS )

*  Description:
*     The routine initialises a WORD axis width array.  The values
*     assigned are calculated from an associated axis data array giving
*     the positions of the pixel centres by forming differences between
*     the centre positions of neighbouring pixels.

*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     DATA( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of axis data values (containing pixel centre coordinates)
*        from which the axis width values should be derived.
*     WIDTH( LBND : UBND ) = INTEGER*2 (Returned)
*        The WORD axis width array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the width values cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Determine the maximum axis width value that can be stored
*     without overflow.
*     -  Loop to assign values to the array elements.
*     -  If there is a pixel on either side of the current pixel, then
*     use half the separation of their centres to calculate the current
*     pixel width.
*     -  If there is a pixel on only one side, then use the distance
*     from the current pixel to calculate the width.
*     -  If there is only one pixel, then use a width of unity.
*     -  Ensure the result is positive.  If it can be stored without
*     overflow, then assign it to the width array.
*     -  Otherwise, assign a bad result.
*     -  If this is the first such error, then report it.
*     -  Note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-OCT-1990 (RFWS):
*        Original version.
*     19-OCT-1990 (RFWS):
*        Fixed bug in checking array index against bounds.
*     9-NOV-1990 (RFWS):
*        Changed the routine name and altered to perform initialisation,
*        rather than extrapolation.
*     17-JAN-1992 (RFWS):
*        Changed to make explicit checks on width values to avoid
*        overflow.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER LBND
      INTEGER UBND
      DOUBLE PRECISION DATA( LBND : UBND )

*  Arguments Returned:
      INTEGER*2 WIDTH( LBND : UBND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION HI        ! Maximum value which can be stored
      DOUBLE PRECISION W         ! Width value
      INTEGER I                  ! Loop counter for array elements
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the maximum axis width value that can be stored without
*  overflow.
      HI = NUM_WTOD( NUM__MAXW )

*  Loop to assign values to the array elements.
      DO 1 I = LBND, UBND

*  If there is a pixel on either side of the current pixel, then use
*  half the separation of their centres to calculate the current pixel
*  width.
         IF ( ( I .GT. LBND ) .AND. ( I .LT. UBND ) ) THEN
            W = 0.5D0 * ( DATA( I + 1 ) - DATA( I - 1 ) )

*  If there is a pixel on only one side, then use the distance from the
*  current pixel to calculate the width.
         ELSE IF ( I .LT. UBND ) THEN
            W = DATA( I + 1 ) - DATA( I )
         ELSE IF ( I .GT. LBND ) THEN
            W = DATA( I ) - DATA( I - 1 )

*  If there is only one pixel, then use a width of unity.
         ELSE
            W = 1.0D0
         END IF

*  Ensure the result is positive.  If it can be stored without
*  overflow, then assign it to the width array.
         IF ( W .LT. 0.0D0 ) W = - W
         IF ( W .LE. HI ) THEN
            WIDTH( I ) = NUM_DTOW( W )

*  Otherwise, assign a bad result.
         ELSE
            WIDTH( I ) = VAL__BADW

*  If this is the first such error, then report it.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETI( 'PIXEL', I )
               CALL ERR_REP( 'NDF1_AWIW_AXVAL',
     :                       'Overflow occurred while calculating ' //
     :                       'a value for pixel ^PIXEL of an axis ' //
     :                       'width array.', STATUS )

*  Note that overflow has occurred.
               OVFLOW = .TRUE.
            END IF
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWIW',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AWIUW( LBND, UBND, DATA, WIDTH, STATUS )
*+
*  Name:
*     NDF1_AWIUW

*  Purpose:
*     Initialise a UNSIGNED WORD axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWIUW( LBND, UBND, DATA, WIDTH, STATUS )

*  Description:
*     The routine initialises a UNSIGNED WORD axis width array.  The values
*     assigned are calculated from an associated axis data array giving
*     the positions of the pixel centres by forming differences between
*     the centre positions of neighbouring pixels.

*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     DATA( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of axis data values (containing pixel centre coordinates)
*        from which the axis width values should be derived.
*     WIDTH( LBND : UBND ) = INTEGER*2 (Returned)
*        The UNSIGNED WORD axis width array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the width values cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Determine the maximum axis width value that can be stored
*     without overflow.
*     -  Loop to assign values to the array elements.
*     -  If there is a pixel on either side of the current pixel, then
*     use half the separation of their centres to calculate the current
*     pixel width.
*     -  If there is a pixel on only one side, then use the distance
*     from the current pixel to calculate the width.
*     -  If there is only one pixel, then use a width of unity.
*     -  Ensure the result is positive.  If it can be stored without
*     overflow, then assign it to the width array.
*     -  Otherwise, assign a bad result.
*     -  If this is the first such error, then report it.
*     -  Note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-OCT-1990 (RFWS):
*        Original version.
*     19-OCT-1990 (RFWS):
*        Fixed bug in checking array index against bounds.
*     9-NOV-1990 (RFWS):
*        Changed the routine name and altered to perform initialisation,
*        rather than extrapolation.
*     17-JAN-1992 (RFWS):
*        Changed to make explicit checks on width values to avoid
*        overflow.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER LBND
      INTEGER UBND
      DOUBLE PRECISION DATA( LBND : UBND )

*  Arguments Returned:
      INTEGER*2 WIDTH( LBND : UBND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION HI        ! Maximum value which can be stored
      DOUBLE PRECISION W         ! Width value
      INTEGER I                  ! Loop counter for array elements
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the maximum axis width value that can be stored without
*  overflow.
      HI = NUM_UWTOD( NUM__MAXUW )

*  Loop to assign values to the array elements.
      DO 1 I = LBND, UBND

*  If there is a pixel on either side of the current pixel, then use
*  half the separation of their centres to calculate the current pixel
*  width.
         IF ( ( I .GT. LBND ) .AND. ( I .LT. UBND ) ) THEN
            W = 0.5D0 * ( DATA( I + 1 ) - DATA( I - 1 ) )

*  If there is a pixel on only one side, then use the distance from the
*  current pixel to calculate the width.
         ELSE IF ( I .LT. UBND ) THEN
            W = DATA( I + 1 ) - DATA( I )
         ELSE IF ( I .GT. LBND ) THEN
            W = DATA( I ) - DATA( I - 1 )

*  If there is only one pixel, then use a width of unity.
         ELSE
            W = 1.0D0
         END IF

*  Ensure the result is positive.  If it can be stored without
*  overflow, then assign it to the width array.
         IF ( W .LT. 0.0D0 ) W = - W
         IF ( W .LE. HI ) THEN
            WIDTH( I ) = NUM_DTOUW( W )

*  Otherwise, assign a bad result.
         ELSE
            WIDTH( I ) = VAL__BADUW

*  If this is the first such error, then report it.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETI( 'PIXEL', I )
               CALL ERR_REP( 'NDF1_AWIUW_AXVAL',
     :                       'Overflow occurred while calculating ' //
     :                       'a value for pixel ^PIXEL of an axis ' //
     :                       'width array.', STATUS )

*  Note that overflow has occurred.
               OVFLOW = .TRUE.
            END IF
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWIUW',
     :                                            STATUS )

      END
