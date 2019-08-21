      SUBROUTINE NDF1_ZPSCA( IACB, TYPE, SCALE, ZERO, STATUS )
*+
*  Name:
*     NDF1_ZPSCA

*  Purpose:
*     Store the parameters defining the values in a SCALED NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ZPSCA( IACB, TYPE, SCALE, ZERO, STATUS )

*  Description:
*     The routine stores values in an NDF that indicate that the NDF is
*     stored in SCALED form. These values are the scales and offsets that
*     related the internal values stored in the NDF array components into
*     the external values of interest to the user:
*
*        external_value = SCALE*internal_value + ZERO

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the ACB entry identifying the NDF.
*     TYPE = INTEGER (Given)
*        The data type in which the parameters are to be stored in the NDF.
*     SCALE( 2 ) = DOUBLE PRECISION (Given)
*        The scale factors for DATA and VARIANCE arrays.
*     ZERO( 2 ) = DOUBLE PRECISION (Given)
*        The zero offsets for DATA and VARIANCE arrays.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - An error will be reported if the DATA or VARIANCE array is mapped
*     on entry to this routine.
*     - The second element of the SCALE and ZERO arrays will be ignored
*     if the VARIANCE component of the NDF is undefined.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-OCT-2010 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NUM_DEC_CVT'      ! Type conversion functions
      INCLUDE 'NUM_DEF_CVT'

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_DMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's data array is mapped for access.
*        ACB_VID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's variance array.
*        ACB_VMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's variance array is mapped for access.

*  Arguments Given:
      INTEGER IACB
      INTEGER TYPE
      DOUBLE PRECISION SCALE( 2 )
      DOUBLE PRECISION ZERO( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER COMP*8           ! Component name
      INTEGER IARY               ! The ARY identifier for the array
      INTEGER ICOMP              ! Component index
      INTEGER NCOMP              ! The number of array components to scale
      LOGICAL MAPPED             ! Is the array component currently mapped?
      LOGICAL THERE              ! Does the VARIANCE component exist?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the variance component does not exist, we only need to loop over
*  one array component (DATA).
      CALL NDF1_VSTA( IACB, THERE, STATUS )
      IF( THERE ) THEN
         NCOMP = 2
      ELSE
         NCOMP = 1
      END IF

*  Loop over the required array components.
      DO ICOMP = 1, NCOMP

*  Get the identifier for the ARY array holding the current NDF
*  component, get the component name, and see if it is mapped.
         IF( ICOMP .EQ. 1 ) THEN
            IARY = ACB_DID( IACB )
            COMP = 'DATA'
            MAPPED = ACB_DMAP( IACB )
         ELSE
            IARY = ACB_VID( IACB )
            COMP = 'VARIANCE'
            MAPPED = ACB_VMAP( IACB )
         END IF

*  Report an error if the array component is currently mapped.
         IF( MAPPED .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = NDF__ACDEN
            CALL MSG_SETC( 'C', COMP )
            CALL ERR_REP( ' ', 'NDF1_ZPSCA: Scale and zero values '//
     :                    'cannot be set for a ^C component since the'//
     :                    'array is currently mapped for access '//
     :                    '(internal programming error).', STATUS )

*  Otherwise store the scale and zero values, casting to the
*  requested data type.
         ELSE IF( TYPE == NDF__TYPB ) THEN
            CALL ARY_PTSZB( IARY, NUM_DTOB( SCALE( ICOMP ) ),
     :                      NUM_DTOB( ZERO( ICOMP ) ), STATUS )

         ELSE IF( TYPE == NDF__TYPD ) THEN
            CALL ARY_PTSZD( IARY, SCALE( ICOMP ), ZERO( ICOMP ),
     :                      STATUS )

         ELSE IF( TYPE == NDF__TYPI ) THEN
            CALL ARY_PTSZI( IARY, NUM_DTOI( SCALE( ICOMP ) ),
     :                      NUM_DTOI( ZERO( ICOMP ) ), STATUS )

         ELSE IF( TYPE == NDF__TYPK ) THEN
            CALL ARY_PTSZK( IARY, NUM_DTOK( SCALE( ICOMP ) ),
     :                      NUM_DTOK( ZERO( ICOMP ) ), STATUS )

         ELSE IF( TYPE == NDF__TYPR ) THEN
            CALL ARY_PTSZR( IARY, NUM_DTOR( SCALE( ICOMP ) ),
     :                      NUM_DTOR( ZERO( ICOMP ) ), STATUS )

         ELSE IF( TYPE == NDF__TYPUB ) THEN
            CALL ARY_PTSZUB( IARY, NUM_DTOUB( SCALE( ICOMP ) ),
     :                      NUM_DTOUB( ZERO( ICOMP ) ), STATUS )

         ELSE IF( TYPE == NDF__TYPUW ) THEN
            CALL ARY_PTSZUW( IARY, NUM_DTOUW( SCALE( ICOMP ) ),
     :                      NUM_DTOUW( ZERO( ICOMP ) ), STATUS )

         ELSE IF( TYPE == NDF__TYPW ) THEN
            CALL ARY_PTSZW( IARY, NUM_DTOW( SCALE( ICOMP ) ),
     :                      NUM_DTOW( ZERO( ICOMP ) ), STATUS )

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETI( 'I', TYPE )
            CALL ERR_REP( ' ', 'NDF1_ZPSCA: Unsupported data type '//
     :                    '(^I) supplied (internal programming error).',
     :                    STATUS )
         END IF

      END DO

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ZPSCA', STATUS )

      END
