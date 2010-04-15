      SUBROUTINE ARY1_VSCL( LOC, STATUS )
*+
*  Name:
*     ARY1_VSCL

*  Purpose:
*     Verify the scale and zero values associated with a scaled array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_VSCL( LOC, STATUS )

*  Description:
*     The routine ensures that the supplied HDS locator contains two
*     scalar components named SCALE and ZERO, which have the same data
*     type. The SCALE value must be larger than zero. Both SCALE and ZERO
*     must not be bad. An error is reported if any problem is encoutered
*     with the supplied LOCATOR.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Locator for the HDS object containing the SCALE and ZERO values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     15-MAY-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'ARY_ERR'          ! ARY error constants
      INCLUDE 'ARY_PAR'          ! ARY constants
      INCLUDE 'ARY_CONST'        ! Private ARY constants

*  Arguments Given:
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( DAT__SZLOC ) LOC2 ! Locator to SCALE or ZERO component
      CHARACTER * ( DAT__SZTYP ) STYP ! Data type of the SCALE value
      CHARACTER * ( DAT__SZTYP ) ZTYP ! Data type of the ZERO value
      DOUBLE PRECISION SCALE     ! The scale value
      DOUBLE PRECISION ZERO      ! The zero value
      INTEGER DIM( ARY__MXDIM )  ! Dimensions of SCALE or ZERO component
      INTEGER NDIM               ! No. of dimensions
      LOGICAL THERE              ! Whether a component exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if there is a SCALE component present in the data object
      CALL DAT_THERE( LOC, 'SCALE', THERE, STATUS )

*  If not, report an error.
      IF ( .NOT. THERE ) THEN
         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = ARY__SCLIN
            CALL ERR_REP( 'ARY1_VSCL_BSIN',
     :                 'The SCALE component in missing (internal ARY '//
     :                 'programming error).', STATUS )
         END IF

*  If there is, then obtain its shape and type.
      ELSE
         LOC2 = ARY__NOLOC
         CALL DAT_FIND( LOC, 'SCALE', LOC2, STATUS )
         CALL DAT_SHAPE( LOC2, 1, DIM, NDIM, STATUS )
         CALL DAT_TYPE( LOC2, STYP, STATUS )

*  Check that it is scalar and report an error if it is not.
         IF ( NDIM .NE. 0 ) THEN
            IF ( STATUS .EQ. SAI__OK ) THEN
               STATUS = ARY__NDMIN
               CALL MSG_SETI( 'BADNDIM', NDIM )
               CALL ERR_REP( 'ARY1_VSCL_BSC1',
     :                  'The SCALE component is ^BADNDIM-dimensional;'//
     :                  ' it should be a scalar.', STATUS )
            END IF
         END IF

*  Report an error if it is zero or negative or bad.
         CALL DAT_GET0D( LOC2, SCALE, STATUS )
         IF( SCALE .LE. 0.0 .OR. SCALE .EQ. VAL__BADD ) THEN
            IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = ARY__SCLIN
               CALL MSG_SETD( 'SCALE', SCALE )
               CALL ERR_REP( 'ARY1_VSCL_BSIN',
     :                 'The SCALE component has an invalid zero, '//
     :                 'negative or <bad> value: ^SCALE.', STATUS )
            END IF
         END IF

*  Annul the locator to the scale component.
         CALL DAT_ANNUL( LOC2, STATUS )
      END IF

*  See if there is a ZERO component present in the data object
      CALL DAT_THERE( LOC, 'ZERO', THERE, STATUS )

*  If not, report an error.
      IF ( .NOT. THERE ) THEN
         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = ARY__SCLIN
            CALL ERR_REP( 'ARY1_VSCL_BSIN',
     :                 'The ZERO component in missing (internal ARY '//
     :                 'programming error).', STATUS )
         END IF

*  If there is, then obtain its shape and data type.
      ELSE
         LOC2 = ARY__NOLOC
         CALL DAT_FIND( LOC, 'ZERO', LOC2, STATUS )
         CALL DAT_SHAPE( LOC2, 1, DIM, NDIM, STATUS )
         CALL DAT_TYPE( LOC2, ZTYP, STATUS )

*  Check that it is scalar and report an error if it is not.
         IF ( NDIM .NE. 0 ) THEN
            IF ( STATUS .EQ. SAI__OK ) THEN
               STATUS = ARY__NDMIN
               CALL MSG_SETI( 'BADNDIM', NDIM )
               CALL ERR_REP( 'ARY1_VSCL_BSC1',
     :                  'The ZERO component is ^BADNDIM-dimensional;'//
     :                  ' it should be a scalar.', STATUS )
            END IF
         END IF

*  Report an error if it is bad.
         CALL DAT_GET0D( LOC2, ZERO, STATUS )
         IF( ZERO .EQ. VAL__BADD ) THEN
            IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = ARY__SCLIN
               CALL ERR_REP( 'ARY1_VSCL_BSIN',
     :                 'The ZERO component has an invalid <bad> '//
     :                 'value.', STATUS )
            END IF
         END IF

*  Report an error if they have different data types.
         IF( STYP .NE. ZTYP ) THEN
            IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = ARY__SCLIN
               CALL ERR_REP( 'ARY1_VSCL_BSIN',
     :                 'The ZERO and SCALE components have different '//
     :                 'data types.', STATUS )
            END IF
         END IF

*  Annul the locator to the zero component.
         CALL DAT_ANNUL( LOC2, STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_VSCL', STATUS )

      END
