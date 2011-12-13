      SUBROUTINE NDF1_ADEB( SCALE, ZERO, UPPER, PIX0, LBNDA, UBNDA,
     :                        ADATA, STATUS )
*+
*  Name:
*     NDF1_ADEB

*  Purpose:
*     Assign extrapolated values to a BYTE axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADEB( SCALE, ZERO, UPPER, PIX0, LBNDA, UBNDA, ADATA,
*                       STATUS )

*  Description:
*     The routine assigns extrapolated values to a BYTE axis data
*     array. It is intended for assigning values to those axis data
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.
*     Parameters relating the array element values to the array index
*     are provided as input arguments.

*  Arguments:
*     SCALE = DOUBLE PRECISION (Given)
*        The scale factor relating the axis array index to the array
*        values according to the formula ADATA( I ) = I * SCALE + ZERO.
*     ZERO = DOUBLE PRECISION (Given)
*        The zero point of the extrapolation formula.
*     UPPER = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then
*        extrapolation will be performed towards higher array index
*        values. Otherwise extrapolation will be towards lower array
*        index values.
*     PIX0 = INTEGER (Given)
*        The index of the first "unknown" pixel to be assigned a value.
*        If UPPER is .TRUE., this will be the index of the pixel
*        following the last one whose value is known. If UPPER is
*        .FALSE., it will be the index of the pixel before the first
*        one whose value is known.
*     LBNDA = INTEGER (Given)
*        The lower bound of the axis data array.
*     UBNDA = INTEGER (Given)
*        The upper bound of the axis data array.
*     ADATA( LBNDA : UBNDA ) = BYTE (Given and Returned)
*        The BYTE axis data array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the extrapolated values
*     cannot be represented using the array's numeric type, then an
*     error will be reported and STATUS set.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Also determine the lower and upper extrapolated array values
*     which can be assigned without overflow occurring.
*     -  Note no overflow has yet occurred.
*     -  Loop to assign extrapolated values to the array elements.
*     -  Calculate the extrapolated value.
*     -  If the result can be stored without overflow, then convert to
*     the required data type and assign the result.
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
*     8-OCT-1990 (RFWS):
*        Original version.
*     17-JAN-1992 (RFWS):
*        Changed to make explicit bounds checks to prevent overflow.
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
      DOUBLE PRECISION SCALE
      DOUBLE PRECISION ZERO
      LOGICAL UPPER
      INTEGER PIX0
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      BYTE ADATA( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION AVAL      ! Extrapolated array value
      DOUBLE PRECISION HI        ! Highest value which can be stored
      DOUBLE PRECISION LO        ! Lowest value which can be stored
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBNDA
      ELSE
         L = LBNDA
         U = PIX0
      END IF

*  Also determine the lower and upper extrapolated array values which
*  can be assigned without overflow occurring.
      LO = NUM_BTOD( NUM__MINB )
      HI = NUM_BTOD( NUM__MAXB )

*  Note no overflow has yet occurred.
      OVFLOW = .FALSE.

*  Loop to assign extrapolated values to the array elements.
      DO 1 I = L, U

*  Calculate the extrapolated value.
         AVAL = SCALE * DBLE( I ) + ZERO

*  If the result can be stored without overflow, then convert to the
*  required data type and assign the result.
         IF ( ( AVAL .GE. LO ) .AND. ( AVAL .LE. HI ) ) THEN
            ADATA( I ) = NUM_DTOB( AVAL )

*  Otherwise, assign a bad result.
         ELSE
            ADATA( I ) = VAL__BADB

*  If this is the first such error, then report it.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETI( 'PIXEL', I )
               CALL ERR_REP( 'NDF1_ADEB_AXVAL',
     :                       'Overflow occurred while calculating ' //
     :                       'an extrapolated value for pixel ' //
     :                       '^PIXEL of an axis centre array.',
     :                       STATUS )

*  Note that overflow has occurred.
               OVFLOW = .TRUE.
            END IF
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADEB',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_ADEUB( SCALE, ZERO, UPPER, PIX0, LBNDA, UBNDA,
     :                        ADATA, STATUS )
*+
*  Name:
*     NDF1_ADEUB

*  Purpose:
*     Assign extrapolated values to a UNSIGNED BYTE axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADEUB( SCALE, ZERO, UPPER, PIX0, LBNDA, UBNDA, ADATA,
*                       STATUS )

*  Description:
*     The routine assigns extrapolated values to a UNSIGNED BYTE axis data
*     array. It is intended for assigning values to those axis data
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.
*     Parameters relating the array element values to the array index
*     are provided as input arguments.

*  Arguments:
*     SCALE = DOUBLE PRECISION (Given)
*        The scale factor relating the axis array index to the array
*        values according to the formula ADATA( I ) = I * SCALE + ZERO.
*     ZERO = DOUBLE PRECISION (Given)
*        The zero point of the extrapolation formula.
*     UPPER = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then
*        extrapolation will be performed towards higher array index
*        values. Otherwise extrapolation will be towards lower array
*        index values.
*     PIX0 = INTEGER (Given)
*        The index of the first "unknown" pixel to be assigned a value.
*        If UPPER is .TRUE., this will be the index of the pixel
*        following the last one whose value is known. If UPPER is
*        .FALSE., it will be the index of the pixel before the first
*        one whose value is known.
*     LBNDA = INTEGER (Given)
*        The lower bound of the axis data array.
*     UBNDA = INTEGER (Given)
*        The upper bound of the axis data array.
*     ADATA( LBNDA : UBNDA ) = BYTE (Given and Returned)
*        The UNSIGNED BYTE axis data array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the extrapolated values
*     cannot be represented using the array's numeric type, then an
*     error will be reported and STATUS set.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Also determine the lower and upper extrapolated array values
*     which can be assigned without overflow occurring.
*     -  Note no overflow has yet occurred.
*     -  Loop to assign extrapolated values to the array elements.
*     -  Calculate the extrapolated value.
*     -  If the result can be stored without overflow, then convert to
*     the required data type and assign the result.
*     -  Otherwise, assign a bad result.
*     -  If this is the first such error, then report it.
*     -  Note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-OCT-1990 (RFWS):
*        Original version.
*     17-JAN-1992 (RFWS):
*        Changed to make explicit bounds checks to prevent overflow.
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
      DOUBLE PRECISION SCALE
      DOUBLE PRECISION ZERO
      LOGICAL UPPER
      INTEGER PIX0
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      BYTE ADATA( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION AVAL      ! Extrapolated array value
      DOUBLE PRECISION HI        ! Highest value which can be stored
      DOUBLE PRECISION LO        ! Lowest value which can be stored
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBNDA
      ELSE
         L = LBNDA
         U = PIX0
      END IF

*  Also determine the lower and upper extrapolated array values which
*  can be assigned without overflow occurring.
      LO = NUM_UBTOD( NUM__MINUB )
      HI = NUM_UBTOD( NUM__MAXUB )

*  Note no overflow has yet occurred.
      OVFLOW = .FALSE.

*  Loop to assign extrapolated values to the array elements.
      DO 1 I = L, U

*  Calculate the extrapolated value.
         AVAL = SCALE * DBLE( I ) + ZERO

*  If the result can be stored without overflow, then convert to the
*  required data type and assign the result.
         IF ( ( AVAL .GE. LO ) .AND. ( AVAL .LE. HI ) ) THEN
            ADATA( I ) = NUM_DTOUB( AVAL )

*  Otherwise, assign a bad result.
         ELSE
            ADATA( I ) = VAL__BADUB

*  If this is the first such error, then report it.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETI( 'PIXEL', I )
               CALL ERR_REP( 'NDF1_ADEUB_AXVAL',
     :                       'Overflow occurred while calculating ' //
     :                       'an extrapolated value for pixel ' //
     :                       '^PIXEL of an axis centre array.',
     :                       STATUS )

*  Note that overflow has occurred.
               OVFLOW = .TRUE.
            END IF
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADEUB',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_ADED( SCALE, ZERO, UPPER, PIX0, LBNDA, UBNDA,
     :                        ADATA, STATUS )
*+
*  Name:
*     NDF1_ADED

*  Purpose:
*     Assign extrapolated values to a DOUBLE PRECISION axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADED( SCALE, ZERO, UPPER, PIX0, LBNDA, UBNDA, ADATA,
*                       STATUS )

*  Description:
*     The routine assigns extrapolated values to a DOUBLE PRECISION axis data
*     array. It is intended for assigning values to those axis data
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.
*     Parameters relating the array element values to the array index
*     are provided as input arguments.

*  Arguments:
*     SCALE = DOUBLE PRECISION (Given)
*        The scale factor relating the axis array index to the array
*        values according to the formula ADATA( I ) = I * SCALE + ZERO.
*     ZERO = DOUBLE PRECISION (Given)
*        The zero point of the extrapolation formula.
*     UPPER = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then
*        extrapolation will be performed towards higher array index
*        values. Otherwise extrapolation will be towards lower array
*        index values.
*     PIX0 = INTEGER (Given)
*        The index of the first "unknown" pixel to be assigned a value.
*        If UPPER is .TRUE., this will be the index of the pixel
*        following the last one whose value is known. If UPPER is
*        .FALSE., it will be the index of the pixel before the first
*        one whose value is known.
*     LBNDA = INTEGER (Given)
*        The lower bound of the axis data array.
*     UBNDA = INTEGER (Given)
*        The upper bound of the axis data array.
*     ADATA( LBNDA : UBNDA ) = DOUBLE PRECISION (Given and Returned)
*        The DOUBLE PRECISION axis data array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the extrapolated values
*     cannot be represented using the array's numeric type, then an
*     error will be reported and STATUS set.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Also determine the lower and upper extrapolated array values
*     which can be assigned without overflow occurring.
*     -  Note no overflow has yet occurred.
*     -  Loop to assign extrapolated values to the array elements.
*     -  Calculate the extrapolated value.
*     -  If the result can be stored without overflow, then convert to
*     the required data type and assign the result.
*     -  Otherwise, assign a bad result.
*     -  If this is the first such error, then report it.
*     -  Note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-OCT-1990 (RFWS):
*        Original version.
*     17-JAN-1992 (RFWS):
*        Changed to make explicit bounds checks to prevent overflow.
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
      DOUBLE PRECISION SCALE
      DOUBLE PRECISION ZERO
      LOGICAL UPPER
      INTEGER PIX0
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      DOUBLE PRECISION ADATA( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION AVAL      ! Extrapolated array value
      DOUBLE PRECISION HI        ! Highest value which can be stored
      DOUBLE PRECISION LO        ! Lowest value which can be stored
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBNDA
      ELSE
         L = LBNDA
         U = PIX0
      END IF

*  Also determine the lower and upper extrapolated array values which
*  can be assigned without overflow occurring.
      LO = NUM_DTOD( NUM__MIND )
      HI = NUM_DTOD( NUM__MAXD )

*  Note no overflow has yet occurred.
      OVFLOW = .FALSE.

*  Loop to assign extrapolated values to the array elements.
      DO 1 I = L, U

*  Calculate the extrapolated value.
         AVAL = SCALE * DBLE( I ) + ZERO

*  If the result can be stored without overflow, then convert to the
*  required data type and assign the result.
         IF ( ( AVAL .GE. LO ) .AND. ( AVAL .LE. HI ) ) THEN
            ADATA( I ) = NUM_DTOD( AVAL )

*  Otherwise, assign a bad result.
         ELSE
            ADATA( I ) = VAL__BADD

*  If this is the first such error, then report it.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETI( 'PIXEL', I )
               CALL ERR_REP( 'NDF1_ADED_AXVAL',
     :                       'Overflow occurred while calculating ' //
     :                       'an extrapolated value for pixel ' //
     :                       '^PIXEL of an axis centre array.',
     :                       STATUS )

*  Note that overflow has occurred.
               OVFLOW = .TRUE.
            END IF
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADED',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_ADEI( SCALE, ZERO, UPPER, PIX0, LBNDA, UBNDA,
     :                        ADATA, STATUS )
*+
*  Name:
*     NDF1_ADEI

*  Purpose:
*     Assign extrapolated values to a INTEGER axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADEI( SCALE, ZERO, UPPER, PIX0, LBNDA, UBNDA, ADATA,
*                       STATUS )

*  Description:
*     The routine assigns extrapolated values to a INTEGER axis data
*     array. It is intended for assigning values to those axis data
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.
*     Parameters relating the array element values to the array index
*     are provided as input arguments.

*  Arguments:
*     SCALE = DOUBLE PRECISION (Given)
*        The scale factor relating the axis array index to the array
*        values according to the formula ADATA( I ) = I * SCALE + ZERO.
*     ZERO = DOUBLE PRECISION (Given)
*        The zero point of the extrapolation formula.
*     UPPER = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then
*        extrapolation will be performed towards higher array index
*        values. Otherwise extrapolation will be towards lower array
*        index values.
*     PIX0 = INTEGER (Given)
*        The index of the first "unknown" pixel to be assigned a value.
*        If UPPER is .TRUE., this will be the index of the pixel
*        following the last one whose value is known. If UPPER is
*        .FALSE., it will be the index of the pixel before the first
*        one whose value is known.
*     LBNDA = INTEGER (Given)
*        The lower bound of the axis data array.
*     UBNDA = INTEGER (Given)
*        The upper bound of the axis data array.
*     ADATA( LBNDA : UBNDA ) = INTEGER (Given and Returned)
*        The INTEGER axis data array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the extrapolated values
*     cannot be represented using the array's numeric type, then an
*     error will be reported and STATUS set.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Also determine the lower and upper extrapolated array values
*     which can be assigned without overflow occurring.
*     -  Note no overflow has yet occurred.
*     -  Loop to assign extrapolated values to the array elements.
*     -  Calculate the extrapolated value.
*     -  If the result can be stored without overflow, then convert to
*     the required data type and assign the result.
*     -  Otherwise, assign a bad result.
*     -  If this is the first such error, then report it.
*     -  Note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-OCT-1990 (RFWS):
*        Original version.
*     17-JAN-1992 (RFWS):
*        Changed to make explicit bounds checks to prevent overflow.
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
      DOUBLE PRECISION SCALE
      DOUBLE PRECISION ZERO
      LOGICAL UPPER
      INTEGER PIX0
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      INTEGER ADATA( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION AVAL      ! Extrapolated array value
      DOUBLE PRECISION HI        ! Highest value which can be stored
      DOUBLE PRECISION LO        ! Lowest value which can be stored
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBNDA
      ELSE
         L = LBNDA
         U = PIX0
      END IF

*  Also determine the lower and upper extrapolated array values which
*  can be assigned without overflow occurring.
      LO = NUM_ITOD( NUM__MINI )
      HI = NUM_ITOD( NUM__MAXI )

*  Note no overflow has yet occurred.
      OVFLOW = .FALSE.

*  Loop to assign extrapolated values to the array elements.
      DO 1 I = L, U

*  Calculate the extrapolated value.
         AVAL = SCALE * DBLE( I ) + ZERO

*  If the result can be stored without overflow, then convert to the
*  required data type and assign the result.
         IF ( ( AVAL .GE. LO ) .AND. ( AVAL .LE. HI ) ) THEN
            ADATA( I ) = NUM_DTOI( AVAL )

*  Otherwise, assign a bad result.
         ELSE
            ADATA( I ) = VAL__BADI

*  If this is the first such error, then report it.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETI( 'PIXEL', I )
               CALL ERR_REP( 'NDF1_ADEI_AXVAL',
     :                       'Overflow occurred while calculating ' //
     :                       'an extrapolated value for pixel ' //
     :                       '^PIXEL of an axis centre array.',
     :                       STATUS )

*  Note that overflow has occurred.
               OVFLOW = .TRUE.
            END IF
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADEI',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_ADER( SCALE, ZERO, UPPER, PIX0, LBNDA, UBNDA,
     :                        ADATA, STATUS )
*+
*  Name:
*     NDF1_ADER

*  Purpose:
*     Assign extrapolated values to a REAL axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADER( SCALE, ZERO, UPPER, PIX0, LBNDA, UBNDA, ADATA,
*                       STATUS )

*  Description:
*     The routine assigns extrapolated values to a REAL axis data
*     array. It is intended for assigning values to those axis data
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.
*     Parameters relating the array element values to the array index
*     are provided as input arguments.

*  Arguments:
*     SCALE = DOUBLE PRECISION (Given)
*        The scale factor relating the axis array index to the array
*        values according to the formula ADATA( I ) = I * SCALE + ZERO.
*     ZERO = DOUBLE PRECISION (Given)
*        The zero point of the extrapolation formula.
*     UPPER = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then
*        extrapolation will be performed towards higher array index
*        values. Otherwise extrapolation will be towards lower array
*        index values.
*     PIX0 = INTEGER (Given)
*        The index of the first "unknown" pixel to be assigned a value.
*        If UPPER is .TRUE., this will be the index of the pixel
*        following the last one whose value is known. If UPPER is
*        .FALSE., it will be the index of the pixel before the first
*        one whose value is known.
*     LBNDA = INTEGER (Given)
*        The lower bound of the axis data array.
*     UBNDA = INTEGER (Given)
*        The upper bound of the axis data array.
*     ADATA( LBNDA : UBNDA ) = REAL (Given and Returned)
*        The REAL axis data array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the extrapolated values
*     cannot be represented using the array's numeric type, then an
*     error will be reported and STATUS set.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Also determine the lower and upper extrapolated array values
*     which can be assigned without overflow occurring.
*     -  Note no overflow has yet occurred.
*     -  Loop to assign extrapolated values to the array elements.
*     -  Calculate the extrapolated value.
*     -  If the result can be stored without overflow, then convert to
*     the required data type and assign the result.
*     -  Otherwise, assign a bad result.
*     -  If this is the first such error, then report it.
*     -  Note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-OCT-1990 (RFWS):
*        Original version.
*     17-JAN-1992 (RFWS):
*        Changed to make explicit bounds checks to prevent overflow.
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
      DOUBLE PRECISION SCALE
      DOUBLE PRECISION ZERO
      LOGICAL UPPER
      INTEGER PIX0
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      REAL ADATA( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION AVAL      ! Extrapolated array value
      DOUBLE PRECISION HI        ! Highest value which can be stored
      DOUBLE PRECISION LO        ! Lowest value which can be stored
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBNDA
      ELSE
         L = LBNDA
         U = PIX0
      END IF

*  Also determine the lower and upper extrapolated array values which
*  can be assigned without overflow occurring.
      LO = NUM_RTOD( NUM__MINR )
      HI = NUM_RTOD( NUM__MAXR )

*  Note no overflow has yet occurred.
      OVFLOW = .FALSE.

*  Loop to assign extrapolated values to the array elements.
      DO 1 I = L, U

*  Calculate the extrapolated value.
         AVAL = SCALE * DBLE( I ) + ZERO

*  If the result can be stored without overflow, then convert to the
*  required data type and assign the result.
         IF ( ( AVAL .GE. LO ) .AND. ( AVAL .LE. HI ) ) THEN
            ADATA( I ) = NUM_DTOR( AVAL )

*  Otherwise, assign a bad result.
         ELSE
            ADATA( I ) = VAL__BADR

*  If this is the first such error, then report it.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETI( 'PIXEL', I )
               CALL ERR_REP( 'NDF1_ADER_AXVAL',
     :                       'Overflow occurred while calculating ' //
     :                       'an extrapolated value for pixel ' //
     :                       '^PIXEL of an axis centre array.',
     :                       STATUS )

*  Note that overflow has occurred.
               OVFLOW = .TRUE.
            END IF
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADER',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_ADEW( SCALE, ZERO, UPPER, PIX0, LBNDA, UBNDA,
     :                        ADATA, STATUS )
*+
*  Name:
*     NDF1_ADEW

*  Purpose:
*     Assign extrapolated values to a WORD axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADEW( SCALE, ZERO, UPPER, PIX0, LBNDA, UBNDA, ADATA,
*                       STATUS )

*  Description:
*     The routine assigns extrapolated values to a WORD axis data
*     array. It is intended for assigning values to those axis data
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.
*     Parameters relating the array element values to the array index
*     are provided as input arguments.

*  Arguments:
*     SCALE = DOUBLE PRECISION (Given)
*        The scale factor relating the axis array index to the array
*        values according to the formula ADATA( I ) = I * SCALE + ZERO.
*     ZERO = DOUBLE PRECISION (Given)
*        The zero point of the extrapolation formula.
*     UPPER = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then
*        extrapolation will be performed towards higher array index
*        values. Otherwise extrapolation will be towards lower array
*        index values.
*     PIX0 = INTEGER (Given)
*        The index of the first "unknown" pixel to be assigned a value.
*        If UPPER is .TRUE., this will be the index of the pixel
*        following the last one whose value is known. If UPPER is
*        .FALSE., it will be the index of the pixel before the first
*        one whose value is known.
*     LBNDA = INTEGER (Given)
*        The lower bound of the axis data array.
*     UBNDA = INTEGER (Given)
*        The upper bound of the axis data array.
*     ADATA( LBNDA : UBNDA ) = INTEGER*2 (Given and Returned)
*        The WORD axis data array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the extrapolated values
*     cannot be represented using the array's numeric type, then an
*     error will be reported and STATUS set.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Also determine the lower and upper extrapolated array values
*     which can be assigned without overflow occurring.
*     -  Note no overflow has yet occurred.
*     -  Loop to assign extrapolated values to the array elements.
*     -  Calculate the extrapolated value.
*     -  If the result can be stored without overflow, then convert to
*     the required data type and assign the result.
*     -  Otherwise, assign a bad result.
*     -  If this is the first such error, then report it.
*     -  Note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-OCT-1990 (RFWS):
*        Original version.
*     17-JAN-1992 (RFWS):
*        Changed to make explicit bounds checks to prevent overflow.
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
      DOUBLE PRECISION SCALE
      DOUBLE PRECISION ZERO
      LOGICAL UPPER
      INTEGER PIX0
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      INTEGER*2 ADATA( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION AVAL      ! Extrapolated array value
      DOUBLE PRECISION HI        ! Highest value which can be stored
      DOUBLE PRECISION LO        ! Lowest value which can be stored
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBNDA
      ELSE
         L = LBNDA
         U = PIX0
      END IF

*  Also determine the lower and upper extrapolated array values which
*  can be assigned without overflow occurring.
      LO = NUM_WTOD( NUM__MINW )
      HI = NUM_WTOD( NUM__MAXW )

*  Note no overflow has yet occurred.
      OVFLOW = .FALSE.

*  Loop to assign extrapolated values to the array elements.
      DO 1 I = L, U

*  Calculate the extrapolated value.
         AVAL = SCALE * DBLE( I ) + ZERO

*  If the result can be stored without overflow, then convert to the
*  required data type and assign the result.
         IF ( ( AVAL .GE. LO ) .AND. ( AVAL .LE. HI ) ) THEN
            ADATA( I ) = NUM_DTOW( AVAL )

*  Otherwise, assign a bad result.
         ELSE
            ADATA( I ) = VAL__BADW

*  If this is the first such error, then report it.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETI( 'PIXEL', I )
               CALL ERR_REP( 'NDF1_ADEW_AXVAL',
     :                       'Overflow occurred while calculating ' //
     :                       'an extrapolated value for pixel ' //
     :                       '^PIXEL of an axis centre array.',
     :                       STATUS )

*  Note that overflow has occurred.
               OVFLOW = .TRUE.
            END IF
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADEW',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_ADEUW( SCALE, ZERO, UPPER, PIX0, LBNDA, UBNDA,
     :                        ADATA, STATUS )
*+
*  Name:
*     NDF1_ADEUW

*  Purpose:
*     Assign extrapolated values to a UNSIGNED WORD axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADEUW( SCALE, ZERO, UPPER, PIX0, LBNDA, UBNDA, ADATA,
*                       STATUS )

*  Description:
*     The routine assigns extrapolated values to a UNSIGNED WORD axis data
*     array. It is intended for assigning values to those axis data
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.
*     Parameters relating the array element values to the array index
*     are provided as input arguments.

*  Arguments:
*     SCALE = DOUBLE PRECISION (Given)
*        The scale factor relating the axis array index to the array
*        values according to the formula ADATA( I ) = I * SCALE + ZERO.
*     ZERO = DOUBLE PRECISION (Given)
*        The zero point of the extrapolation formula.
*     UPPER = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then
*        extrapolation will be performed towards higher array index
*        values. Otherwise extrapolation will be towards lower array
*        index values.
*     PIX0 = INTEGER (Given)
*        The index of the first "unknown" pixel to be assigned a value.
*        If UPPER is .TRUE., this will be the index of the pixel
*        following the last one whose value is known. If UPPER is
*        .FALSE., it will be the index of the pixel before the first
*        one whose value is known.
*     LBNDA = INTEGER (Given)
*        The lower bound of the axis data array.
*     UBNDA = INTEGER (Given)
*        The upper bound of the axis data array.
*     ADATA( LBNDA : UBNDA ) = INTEGER*2 (Given and Returned)
*        The UNSIGNED WORD axis data array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the extrapolated values
*     cannot be represented using the array's numeric type, then an
*     error will be reported and STATUS set.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Also determine the lower and upper extrapolated array values
*     which can be assigned without overflow occurring.
*     -  Note no overflow has yet occurred.
*     -  Loop to assign extrapolated values to the array elements.
*     -  Calculate the extrapolated value.
*     -  If the result can be stored without overflow, then convert to
*     the required data type and assign the result.
*     -  Otherwise, assign a bad result.
*     -  If this is the first such error, then report it.
*     -  Note that overflow has occurred.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-OCT-1990 (RFWS):
*        Original version.
*     17-JAN-1992 (RFWS):
*        Changed to make explicit bounds checks to prevent overflow.
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
      DOUBLE PRECISION SCALE
      DOUBLE PRECISION ZERO
      LOGICAL UPPER
      INTEGER PIX0
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      INTEGER*2 ADATA( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION AVAL      ! Extrapolated array value
      DOUBLE PRECISION HI        ! Highest value which can be stored
      DOUBLE PRECISION LO        ! Lowest value which can be stored
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider
      LOGICAL OVFLOW             ! Whether overflow has occurred

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBNDA
      ELSE
         L = LBNDA
         U = PIX0
      END IF

*  Also determine the lower and upper extrapolated array values which
*  can be assigned without overflow occurring.
      LO = NUM_UWTOD( NUM__MINUW )
      HI = NUM_UWTOD( NUM__MAXUW )

*  Note no overflow has yet occurred.
      OVFLOW = .FALSE.

*  Loop to assign extrapolated values to the array elements.
      DO 1 I = L, U

*  Calculate the extrapolated value.
         AVAL = SCALE * DBLE( I ) + ZERO

*  If the result can be stored without overflow, then convert to the
*  required data type and assign the result.
         IF ( ( AVAL .GE. LO ) .AND. ( AVAL .LE. HI ) ) THEN
            ADATA( I ) = NUM_DTOUW( AVAL )

*  Otherwise, assign a bad result.
         ELSE
            ADATA( I ) = VAL__BADUW

*  If this is the first such error, then report it.
            IF ( .NOT. OVFLOW ) THEN
               STATUS = NDF__AXOVF
               CALL MSG_SETI( 'PIXEL', I )
               CALL ERR_REP( 'NDF1_ADEUW_AXVAL',
     :                       'Overflow occurred while calculating ' //
     :                       'an extrapolated value for pixel ' //
     :                       '^PIXEL of an axis centre array.',
     :                       STATUS )

*  Note that overflow has occurred.
               OVFLOW = .TRUE.
            END IF
         END IF
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADEUW',
     :                                            STATUS )

      END
