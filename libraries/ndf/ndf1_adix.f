      SUBROUTINE NDF1_ADIB( LBNDA, UBNDA, ADATA, STATUS )
*+
*  Name:
*     NDF1_ADIB

*  Purpose:
*     Initialise a BYTE axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADIB( LBNDA, UBNDA, ADATA, STATUS )

*  Description:
*     The routine assigns initial values to a BYTE axis data array.
*     The values are chosen so as to define the default axis coordinate
*     system, in which, for each axis, the pixel with index (I) has a
*     central coordinate of (I-0.5).

*  Arguments:
*     LBNDA = INTEGER (Given)
*        Index of the first pixel on the axis.
*     UBNDA = INTEGER (Given)
*        Index of the last pixel on the axis.
*     ADATA( LBNDA : UBNDA ) = BYTE (Returned)
*        BYTE axis data array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Successive elements of the array are set to the values
*     LBNDA-0.5, LBNDA+0.5, LBNDA+1.5, etc. (these values are rounded
*     up in the case of non-floating point types, so the values LBNDA,
*     LBNDA+1, LBNDA+2, etc. will result).
*     -  If overflow occurs because any of the values to be assigned
*     cannot be represented using the array's numeric type, then an
*     error will be reported and STATUS set.

*  Algorithm:
*     -  Note no overflow has occurred yet.
*     -  Depending on the numeric type of the array, determine the
*     lower and upper bounds on the values which may be assigned.
*     -  Loop to assign a value to each array element, checking that
*     the values are in range.
*     -  If the value is out of range, then set a "bad" value for the
*     affected array element.
*     -  If this is the first time overflow has occurred, then report
*     an error and note that overflow has occurred.

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
*     13-JUN-1990 (RFWS):
*        Original version.
*     8-OCT-1990 (RFWS):
*        Added bounds checking on array values.
*     10-OCT-1990 (RFWS):
*        Fixed sign error in asignment of array values.
*     17-JUL-1992 (RFWS):
*        Changed limits LO and HI to be LBNDA and UBNDA if overflow
*        cannot occur (previous use of constants here seems to confuse
*        the mips compiler).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      BYTE ADATA( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER HI                 ! Upper bound on array values
      INTEGER I                  ! Loop counter for array elements
      INTEGER LO                 ! Lower bound on array values
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Note no overflow has occurred yet.
      OVFLOW = .FALSE.

*  Depending on the numeric type of the array, determine the lower and
*  upper bounds on the values which may be assigned.
      IF ( ( 'B' .EQ. 'D' ) .OR. ( 'B' .EQ. 'R' ) .OR.
     :     ( 'B' .EQ. 'I' ) ) THEN
         LO = LBNDA
         HI = UBNDA
      ELSE
         LO = NUM_BTOI( NUM__MINB )
         HI = NUM_BTOI( NUM__MAXB )
      END IF

*  Loop to assign a value to each array element, checking that the
*  values are in range.
      DO 1 I = LBNDA, UBNDA
         IF ( ( I .GE. LO ) .AND. ( I .LE. HI ) ) THEN
            IF ( ( 'B' .EQ. 'D' ) .OR. ( 'B' .EQ. 'R' ) ) THEN
               ADATA( I ) = NUM_ITOB( I ) - NUM_DTOB( 0.5D0 )
            ELSE
               ADATA( I ) = NUM_ITOB( I )
            END IF

*  If the value is out of range, then set a "bad" value for the
*  affected array element.
         ELSE
            ADATA( I ) = VAL__BADB

*  If this is the first time overflow has occurred, then report an
*  error.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETR( 'AXVAL', REAL( I ) + 0.5 )
               CALL ERR_REP( 'NDF1_ADIB_AXVAL',
     :         'Unable to assign a value of ^AXVAL to an axis ' //
     :         'centre array with a numeric type of ''_BYTE''. ',
     :         STATUS )
            END IF

*  Note that overflow has occurred.
            OVFLOW = .TRUE.
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADIB',
     :                                             STATUS )

      END
      SUBROUTINE NDF1_ADIUB( LBNDA, UBNDA, ADATA, STATUS )
*+
*  Name:
*     NDF1_ADIUB

*  Purpose:
*     Initialise a UNSIGNED BYTE axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADIUB( LBNDA, UBNDA, ADATA, STATUS )

*  Description:
*     The routine assigns initial values to a UNSIGNED BYTE axis data array.
*     The values are chosen so as to define the default axis coordinate
*     system, in which, for each axis, the pixel with index (I) has a
*     central coordinate of (I-0.5).

*  Arguments:
*     LBNDA = INTEGER (Given)
*        Index of the first pixel on the axis.
*     UBNDA = INTEGER (Given)
*        Index of the last pixel on the axis.
*     ADATA( LBNDA : UBNDA ) = BYTE (Returned)
*        UNSIGNED BYTE axis data array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Successive elements of the array are set to the values
*     LBNDA-0.5, LBNDA+0.5, LBNDA+1.5, etc. (these values are rounded
*     up in the case of non-floating point types, so the values LBNDA,
*     LBNDA+1, LBNDA+2, etc. will result).
*     -  If overflow occurs because any of the values to be assigned
*     cannot be represented using the array's numeric type, then an
*     error will be reported and STATUS set.

*  Algorithm:
*     -  Note no overflow has occurred yet.
*     -  Depending on the numeric type of the array, determine the
*     lower and upper bounds on the values which may be assigned.
*     -  Loop to assign a value to each array element, checking that
*     the values are in range.
*     -  If the value is out of range, then set a "bad" value for the
*     affected array element.
*     -  If this is the first time overflow has occurred, then report
*     an error and note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1990 (RFWS):
*        Original version.
*     8-OCT-1990 (RFWS):
*        Added bounds checking on array values.
*     10-OCT-1990 (RFWS):
*        Fixed sign error in asignment of array values.
*     17-JUL-1992 (RFWS):
*        Changed limits LO and HI to be LBNDA and UBNDA if overflow
*        cannot occur (previous use of constants here seems to confuse
*        the mips compiler).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      BYTE ADATA( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER HI                 ! Upper bound on array values
      INTEGER I                  ! Loop counter for array elements
      INTEGER LO                 ! Lower bound on array values
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Note no overflow has occurred yet.
      OVFLOW = .FALSE.

*  Depending on the numeric type of the array, determine the lower and
*  upper bounds on the values which may be assigned.
      IF ( ( 'UB' .EQ. 'D' ) .OR. ( 'UB' .EQ. 'R' ) .OR.
     :     ( 'UB' .EQ. 'I' ) ) THEN
         LO = LBNDA
         HI = UBNDA
      ELSE
         LO = NUM_UBTOI( NUM__MINUB )
         HI = NUM_UBTOI( NUM__MAXUB )
      END IF

*  Loop to assign a value to each array element, checking that the
*  values are in range.
      DO 1 I = LBNDA, UBNDA
         IF ( ( I .GE. LO ) .AND. ( I .LE. HI ) ) THEN
            IF ( ( 'UB' .EQ. 'D' ) .OR. ( 'UB' .EQ. 'R' ) ) THEN
               ADATA( I ) = NUM_ITOUB( I ) - NUM_DTOUB( 0.5D0 )
            ELSE
               ADATA( I ) = NUM_ITOUB( I )
            END IF

*  If the value is out of range, then set a "bad" value for the
*  affected array element.
         ELSE
            ADATA( I ) = VAL__BADUB

*  If this is the first time overflow has occurred, then report an
*  error.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETR( 'AXVAL', REAL( I ) + 0.5 )
               CALL ERR_REP( 'NDF1_ADIUB_AXVAL',
     :         'Unable to assign a value of ^AXVAL to an axis ' //
     :         'centre array with a numeric type of ''_UBYTE''. ',
     :         STATUS )
            END IF

*  Note that overflow has occurred.
            OVFLOW = .TRUE.
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADIUB',
     :                                             STATUS )

      END
      SUBROUTINE NDF1_ADID( LBNDA, UBNDA, ADATA, STATUS )
*+
*  Name:
*     NDF1_ADID

*  Purpose:
*     Initialise a DOUBLE PRECISION axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADID( LBNDA, UBNDA, ADATA, STATUS )

*  Description:
*     The routine assigns initial values to a DOUBLE PRECISION axis data array.
*     The values are chosen so as to define the default axis coordinate
*     system, in which, for each axis, the pixel with index (I) has a
*     central coordinate of (I-0.5).

*  Arguments:
*     LBNDA = INTEGER (Given)
*        Index of the first pixel on the axis.
*     UBNDA = INTEGER (Given)
*        Index of the last pixel on the axis.
*     ADATA( LBNDA : UBNDA ) = DOUBLE PRECISION (Returned)
*        DOUBLE PRECISION axis data array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Successive elements of the array are set to the values
*     LBNDA-0.5, LBNDA+0.5, LBNDA+1.5, etc. (these values are rounded
*     up in the case of non-floating point types, so the values LBNDA,
*     LBNDA+1, LBNDA+2, etc. will result).
*     -  If overflow occurs because any of the values to be assigned
*     cannot be represented using the array's numeric type, then an
*     error will be reported and STATUS set.

*  Algorithm:
*     -  Note no overflow has occurred yet.
*     -  Depending on the numeric type of the array, determine the
*     lower and upper bounds on the values which may be assigned.
*     -  Loop to assign a value to each array element, checking that
*     the values are in range.
*     -  If the value is out of range, then set a "bad" value for the
*     affected array element.
*     -  If this is the first time overflow has occurred, then report
*     an error and note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1990 (RFWS):
*        Original version.
*     8-OCT-1990 (RFWS):
*        Added bounds checking on array values.
*     10-OCT-1990 (RFWS):
*        Fixed sign error in asignment of array values.
*     17-JUL-1992 (RFWS):
*        Changed limits LO and HI to be LBNDA and UBNDA if overflow
*        cannot occur (previous use of constants here seems to confuse
*        the mips compiler).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      DOUBLE PRECISION ADATA( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER HI                 ! Upper bound on array values
      INTEGER I                  ! Loop counter for array elements
      INTEGER LO                 ! Lower bound on array values
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Note no overflow has occurred yet.
      OVFLOW = .FALSE.

*  Depending on the numeric type of the array, determine the lower and
*  upper bounds on the values which may be assigned.
      IF ( ( 'D' .EQ. 'D' ) .OR. ( 'D' .EQ. 'R' ) .OR.
     :     ( 'D' .EQ. 'I' ) ) THEN
         LO = LBNDA
         HI = UBNDA
      ELSE
         LO = NUM_DTOI( NUM__MIND )
         HI = NUM_DTOI( NUM__MAXD )
      END IF

*  Loop to assign a value to each array element, checking that the
*  values are in range.
      DO 1 I = LBNDA, UBNDA
         IF ( ( I .GE. LO ) .AND. ( I .LE. HI ) ) THEN
            IF ( ( 'D' .EQ. 'D' ) .OR. ( 'D' .EQ. 'R' ) ) THEN
               ADATA( I ) = NUM_ITOD( I ) - NUM_DTOD( 0.5D0 )
            ELSE
               ADATA( I ) = NUM_ITOD( I )
            END IF

*  If the value is out of range, then set a "bad" value for the
*  affected array element.
         ELSE
            ADATA( I ) = VAL__BADD

*  If this is the first time overflow has occurred, then report an
*  error.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETR( 'AXVAL', REAL( I ) + 0.5 )
               CALL ERR_REP( 'NDF1_ADID_AXVAL',
     :         'Unable to assign a value of ^AXVAL to an axis ' //
     :         'centre array with a numeric type of ''_DOUBLE''. ',
     :         STATUS )
            END IF

*  Note that overflow has occurred.
            OVFLOW = .TRUE.
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADID',
     :                                             STATUS )

      END
      SUBROUTINE NDF1_ADII( LBNDA, UBNDA, ADATA, STATUS )
*+
*  Name:
*     NDF1_ADII

*  Purpose:
*     Initialise a INTEGER axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADII( LBNDA, UBNDA, ADATA, STATUS )

*  Description:
*     The routine assigns initial values to a INTEGER axis data array.
*     The values are chosen so as to define the default axis coordinate
*     system, in which, for each axis, the pixel with index (I) has a
*     central coordinate of (I-0.5).

*  Arguments:
*     LBNDA = INTEGER (Given)
*        Index of the first pixel on the axis.
*     UBNDA = INTEGER (Given)
*        Index of the last pixel on the axis.
*     ADATA( LBNDA : UBNDA ) = INTEGER (Returned)
*        INTEGER axis data array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Successive elements of the array are set to the values
*     LBNDA-0.5, LBNDA+0.5, LBNDA+1.5, etc. (these values are rounded
*     up in the case of non-floating point types, so the values LBNDA,
*     LBNDA+1, LBNDA+2, etc. will result).
*     -  If overflow occurs because any of the values to be assigned
*     cannot be represented using the array's numeric type, then an
*     error will be reported and STATUS set.

*  Algorithm:
*     -  Note no overflow has occurred yet.
*     -  Depending on the numeric type of the array, determine the
*     lower and upper bounds on the values which may be assigned.
*     -  Loop to assign a value to each array element, checking that
*     the values are in range.
*     -  If the value is out of range, then set a "bad" value for the
*     affected array element.
*     -  If this is the first time overflow has occurred, then report
*     an error and note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1990 (RFWS):
*        Original version.
*     8-OCT-1990 (RFWS):
*        Added bounds checking on array values.
*     10-OCT-1990 (RFWS):
*        Fixed sign error in asignment of array values.
*     17-JUL-1992 (RFWS):
*        Changed limits LO and HI to be LBNDA and UBNDA if overflow
*        cannot occur (previous use of constants here seems to confuse
*        the mips compiler).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      INTEGER ADATA( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER HI                 ! Upper bound on array values
      INTEGER I                  ! Loop counter for array elements
      INTEGER LO                 ! Lower bound on array values
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Note no overflow has occurred yet.
      OVFLOW = .FALSE.

*  Depending on the numeric type of the array, determine the lower and
*  upper bounds on the values which may be assigned.
      IF ( ( 'I' .EQ. 'D' ) .OR. ( 'I' .EQ. 'R' ) .OR.
     :     ( 'I' .EQ. 'I' ) ) THEN
         LO = LBNDA
         HI = UBNDA
      ELSE
         LO = NUM_ITOI( NUM__MINI )
         HI = NUM_ITOI( NUM__MAXI )
      END IF

*  Loop to assign a value to each array element, checking that the
*  values are in range.
      DO 1 I = LBNDA, UBNDA
         IF ( ( I .GE. LO ) .AND. ( I .LE. HI ) ) THEN
            IF ( ( 'I' .EQ. 'D' ) .OR. ( 'I' .EQ. 'R' ) ) THEN
               ADATA( I ) = NUM_ITOI( I ) - NUM_DTOI( 0.5D0 )
            ELSE
               ADATA( I ) = NUM_ITOI( I )
            END IF

*  If the value is out of range, then set a "bad" value for the
*  affected array element.
         ELSE
            ADATA( I ) = VAL__BADI

*  If this is the first time overflow has occurred, then report an
*  error.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETR( 'AXVAL', REAL( I ) + 0.5 )
               CALL ERR_REP( 'NDF1_ADII_AXVAL',
     :         'Unable to assign a value of ^AXVAL to an axis ' //
     :         'centre array with a numeric type of ''_INTEGER''. ',
     :         STATUS )
            END IF

*  Note that overflow has occurred.
            OVFLOW = .TRUE.
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADII',
     :                                             STATUS )

      END
      SUBROUTINE NDF1_ADIR( LBNDA, UBNDA, ADATA, STATUS )
*+
*  Name:
*     NDF1_ADIR

*  Purpose:
*     Initialise a REAL axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADIR( LBNDA, UBNDA, ADATA, STATUS )

*  Description:
*     The routine assigns initial values to a REAL axis data array.
*     The values are chosen so as to define the default axis coordinate
*     system, in which, for each axis, the pixel with index (I) has a
*     central coordinate of (I-0.5).

*  Arguments:
*     LBNDA = INTEGER (Given)
*        Index of the first pixel on the axis.
*     UBNDA = INTEGER (Given)
*        Index of the last pixel on the axis.
*     ADATA( LBNDA : UBNDA ) = REAL (Returned)
*        REAL axis data array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Successive elements of the array are set to the values
*     LBNDA-0.5, LBNDA+0.5, LBNDA+1.5, etc. (these values are rounded
*     up in the case of non-floating point types, so the values LBNDA,
*     LBNDA+1, LBNDA+2, etc. will result).
*     -  If overflow occurs because any of the values to be assigned
*     cannot be represented using the array's numeric type, then an
*     error will be reported and STATUS set.

*  Algorithm:
*     -  Note no overflow has occurred yet.
*     -  Depending on the numeric type of the array, determine the
*     lower and upper bounds on the values which may be assigned.
*     -  Loop to assign a value to each array element, checking that
*     the values are in range.
*     -  If the value is out of range, then set a "bad" value for the
*     affected array element.
*     -  If this is the first time overflow has occurred, then report
*     an error and note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1990 (RFWS):
*        Original version.
*     8-OCT-1990 (RFWS):
*        Added bounds checking on array values.
*     10-OCT-1990 (RFWS):
*        Fixed sign error in asignment of array values.
*     17-JUL-1992 (RFWS):
*        Changed limits LO and HI to be LBNDA and UBNDA if overflow
*        cannot occur (previous use of constants here seems to confuse
*        the mips compiler).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      REAL ADATA( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER HI                 ! Upper bound on array values
      INTEGER I                  ! Loop counter for array elements
      INTEGER LO                 ! Lower bound on array values
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Note no overflow has occurred yet.
      OVFLOW = .FALSE.

*  Depending on the numeric type of the array, determine the lower and
*  upper bounds on the values which may be assigned.
      IF ( ( 'R' .EQ. 'D' ) .OR. ( 'R' .EQ. 'R' ) .OR.
     :     ( 'R' .EQ. 'I' ) ) THEN
         LO = LBNDA
         HI = UBNDA
      ELSE
         LO = NUM_RTOI( NUM__MINR )
         HI = NUM_RTOI( NUM__MAXR )
      END IF

*  Loop to assign a value to each array element, checking that the
*  values are in range.
      DO 1 I = LBNDA, UBNDA
         IF ( ( I .GE. LO ) .AND. ( I .LE. HI ) ) THEN
            IF ( ( 'R' .EQ. 'D' ) .OR. ( 'R' .EQ. 'R' ) ) THEN
               ADATA( I ) = NUM_ITOR( I ) - NUM_DTOR( 0.5D0 )
            ELSE
               ADATA( I ) = NUM_ITOR( I )
            END IF

*  If the value is out of range, then set a "bad" value for the
*  affected array element.
         ELSE
            ADATA( I ) = VAL__BADR

*  If this is the first time overflow has occurred, then report an
*  error.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETR( 'AXVAL', REAL( I ) + 0.5 )
               CALL ERR_REP( 'NDF1_ADIR_AXVAL',
     :         'Unable to assign a value of ^AXVAL to an axis ' //
     :         'centre array with a numeric type of ''_REAL''. ',
     :         STATUS )
            END IF

*  Note that overflow has occurred.
            OVFLOW = .TRUE.
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADIR',
     :                                             STATUS )

      END
      SUBROUTINE NDF1_ADIW( LBNDA, UBNDA, ADATA, STATUS )
*+
*  Name:
*     NDF1_ADIW

*  Purpose:
*     Initialise a WORD axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADIW( LBNDA, UBNDA, ADATA, STATUS )

*  Description:
*     The routine assigns initial values to a WORD axis data array.
*     The values are chosen so as to define the default axis coordinate
*     system, in which, for each axis, the pixel with index (I) has a
*     central coordinate of (I-0.5).

*  Arguments:
*     LBNDA = INTEGER (Given)
*        Index of the first pixel on the axis.
*     UBNDA = INTEGER (Given)
*        Index of the last pixel on the axis.
*     ADATA( LBNDA : UBNDA ) = INTEGER*2 (Returned)
*        WORD axis data array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Successive elements of the array are set to the values
*     LBNDA-0.5, LBNDA+0.5, LBNDA+1.5, etc. (these values are rounded
*     up in the case of non-floating point types, so the values LBNDA,
*     LBNDA+1, LBNDA+2, etc. will result).
*     -  If overflow occurs because any of the values to be assigned
*     cannot be represented using the array's numeric type, then an
*     error will be reported and STATUS set.

*  Algorithm:
*     -  Note no overflow has occurred yet.
*     -  Depending on the numeric type of the array, determine the
*     lower and upper bounds on the values which may be assigned.
*     -  Loop to assign a value to each array element, checking that
*     the values are in range.
*     -  If the value is out of range, then set a "bad" value for the
*     affected array element.
*     -  If this is the first time overflow has occurred, then report
*     an error and note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1990 (RFWS):
*        Original version.
*     8-OCT-1990 (RFWS):
*        Added bounds checking on array values.
*     10-OCT-1990 (RFWS):
*        Fixed sign error in asignment of array values.
*     17-JUL-1992 (RFWS):
*        Changed limits LO and HI to be LBNDA and UBNDA if overflow
*        cannot occur (previous use of constants here seems to confuse
*        the mips compiler).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      INTEGER*2 ADATA( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER HI                 ! Upper bound on array values
      INTEGER I                  ! Loop counter for array elements
      INTEGER LO                 ! Lower bound on array values
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Note no overflow has occurred yet.
      OVFLOW = .FALSE.

*  Depending on the numeric type of the array, determine the lower and
*  upper bounds on the values which may be assigned.
      IF ( ( 'W' .EQ. 'D' ) .OR. ( 'W' .EQ. 'R' ) .OR.
     :     ( 'W' .EQ. 'I' ) ) THEN
         LO = LBNDA
         HI = UBNDA
      ELSE
         LO = NUM_WTOI( NUM__MINW )
         HI = NUM_WTOI( NUM__MAXW )
      END IF

*  Loop to assign a value to each array element, checking that the
*  values are in range.
      DO 1 I = LBNDA, UBNDA
         IF ( ( I .GE. LO ) .AND. ( I .LE. HI ) ) THEN
            IF ( ( 'W' .EQ. 'D' ) .OR. ( 'W' .EQ. 'R' ) ) THEN
               ADATA( I ) = NUM_ITOW( I ) - NUM_DTOW( 0.5D0 )
            ELSE
               ADATA( I ) = NUM_ITOW( I )
            END IF

*  If the value is out of range, then set a "bad" value for the
*  affected array element.
         ELSE
            ADATA( I ) = VAL__BADW

*  If this is the first time overflow has occurred, then report an
*  error.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETR( 'AXVAL', REAL( I ) + 0.5 )
               CALL ERR_REP( 'NDF1_ADIW_AXVAL',
     :         'Unable to assign a value of ^AXVAL to an axis ' //
     :         'centre array with a numeric type of ''_WORD''. ',
     :         STATUS )
            END IF

*  Note that overflow has occurred.
            OVFLOW = .TRUE.
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADIW',
     :                                             STATUS )

      END
      SUBROUTINE NDF1_ADIUW( LBNDA, UBNDA, ADATA, STATUS )
*+
*  Name:
*     NDF1_ADIUW

*  Purpose:
*     Initialise a UNSIGNED WORD axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADIUW( LBNDA, UBNDA, ADATA, STATUS )

*  Description:
*     The routine assigns initial values to a UNSIGNED WORD axis data array.
*     The values are chosen so as to define the default axis coordinate
*     system, in which, for each axis, the pixel with index (I) has a
*     central coordinate of (I-0.5).

*  Arguments:
*     LBNDA = INTEGER (Given)
*        Index of the first pixel on the axis.
*     UBNDA = INTEGER (Given)
*        Index of the last pixel on the axis.
*     ADATA( LBNDA : UBNDA ) = INTEGER*2 (Returned)
*        UNSIGNED WORD axis data array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Successive elements of the array are set to the values
*     LBNDA-0.5, LBNDA+0.5, LBNDA+1.5, etc. (these values are rounded
*     up in the case of non-floating point types, so the values LBNDA,
*     LBNDA+1, LBNDA+2, etc. will result).
*     -  If overflow occurs because any of the values to be assigned
*     cannot be represented using the array's numeric type, then an
*     error will be reported and STATUS set.

*  Algorithm:
*     -  Note no overflow has occurred yet.
*     -  Depending on the numeric type of the array, determine the
*     lower and upper bounds on the values which may be assigned.
*     -  Loop to assign a value to each array element, checking that
*     the values are in range.
*     -  If the value is out of range, then set a "bad" value for the
*     affected array element.
*     -  If this is the first time overflow has occurred, then report
*     an error and note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1990 (RFWS):
*        Original version.
*     8-OCT-1990 (RFWS):
*        Added bounds checking on array values.
*     10-OCT-1990 (RFWS):
*        Fixed sign error in asignment of array values.
*     17-JUL-1992 (RFWS):
*        Changed limits LO and HI to be LBNDA and UBNDA if overflow
*        cannot occur (previous use of constants here seems to confuse
*        the mips compiler).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      INTEGER*2 ADATA( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER HI                 ! Upper bound on array values
      INTEGER I                  ! Loop counter for array elements
      INTEGER LO                 ! Lower bound on array values
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Note no overflow has occurred yet.
      OVFLOW = .FALSE.

*  Depending on the numeric type of the array, determine the lower and
*  upper bounds on the values which may be assigned.
      IF ( ( 'UW' .EQ. 'D' ) .OR. ( 'UW' .EQ. 'R' ) .OR.
     :     ( 'UW' .EQ. 'I' ) ) THEN
         LO = LBNDA
         HI = UBNDA
      ELSE
         LO = NUM_UWTOI( NUM__MINUW )
         HI = NUM_UWTOI( NUM__MAXUW )
      END IF

*  Loop to assign a value to each array element, checking that the
*  values are in range.
      DO 1 I = LBNDA, UBNDA
         IF ( ( I .GE. LO ) .AND. ( I .LE. HI ) ) THEN
            IF ( ( 'UW' .EQ. 'D' ) .OR. ( 'UW' .EQ. 'R' ) ) THEN
               ADATA( I ) = NUM_ITOUW( I ) - NUM_DTOUW( 0.5D0 )
            ELSE
               ADATA( I ) = NUM_ITOUW( I )
            END IF

*  If the value is out of range, then set a "bad" value for the
*  affected array element.
         ELSE
            ADATA( I ) = VAL__BADUW

*  If this is the first time overflow has occurred, then report an
*  error.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETR( 'AXVAL', REAL( I ) + 0.5 )
               CALL ERR_REP( 'NDF1_ADIUW_AXVAL',
     :         'Unable to assign a value of ^AXVAL to an axis ' //
     :         'centre array with a numeric type of ''_UWORD''. ',
     :         STATUS )
            END IF

*  Note that overflow has occurred.
            OVFLOW = .TRUE.
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADIUW',
     :                                             STATUS )

      END
