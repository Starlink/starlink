      SUBROUTINE ARY1_CPSCL( IDCB1, IDCB2, STATUS )
*+
*  Name:
*     ARY1_CPSCL

*  Purpose:
*     Copy scale information from one DCB entry to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_CPSCL( IDCB1, IDCB2, STATUS )

*  Description:
*     The routine copies the information describing the scale and zero
*     terms for a supplied data array (identified by its DCB entry) to
*     another existing DCB entry.

*  Arguments:
*     IDCB1 = INTEGER (Given)
*        Index to the DCB entry of the array to be copied.
*     IDCB2 = INTEGER (Given)
*        Index to the DCB entry to recieve the copy.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNV_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_CPX( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the data object is complex.
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Data object storage form.
*        DCB_SCLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Structure containing scale and zero values.

*  Arguments Given:
      INTEGER IDCB1
      INTEGER IDCB2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER LOC2*(DAT__SZLOC)! Locator for scale or zero term

*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Ensure that form information is available in the input DCB entry.
      CALL ARY1_DFRM( IDCB1, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN

*  If the first DCB entry is a scaled array then we copy the scale
*  information.
         IF( DCB_FRM( IDCB1 ) .EQ. 'SCALED' ) THEN

*  Report an error if we are trying to store scale information in a
*  complex valued array.
            CALL ARY1_DTYP( IDCB1, STATUS )
            IF( DCB_CPX( IDCB1 ) .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = ARY__USFRM
               CALL ERR_REP( 'ARY1_DSTP_SCMX', 'Complex scaled '//
     :                       'arrays are currently unsupported '//
     :                       'by the ARY library.', STATUS )
            END IF

*  Ensure scaling information is available for IDCB1.
            CALL ARY1_DSCL( IDCB1, STATUS )

*  Create a new temporary HDS structure to hold a copy of the scale
*  information, and store the new locator in the output DCB entry.
            CALL DAT_TEMP( 'SCZR', 0, 0, DCB_SCLOC( IDCB2 ), STATUS )

*  Get a locator to the input SCALE value and copy it to the temporary
*  HDS structure created above, and to the data object. Then annul the locator.
            CALL DAT_FIND( DCB_SCLOC( IDCB1 ), 'SCALE', LOC2,
     :                     STATUS )
            CALL DAT_COPY( LOC2, DCB_SCLOC( IDCB2 ), 'SCALE',
     :                     STATUS )
            CALL DAT_COPY( LOC2, DCB_LOC( IDCB2 ), 'SCALE',
     :                     STATUS )
            CALL DAT_ANNUL( LOC2, STATUS )

*  Do the same for the ZERO value.
            CALL DAT_FIND( DCB_SCLOC( IDCB1 ), 'ZERO', LOC2,
     :                     STATUS )
            CALL DAT_COPY( LOC2, DCB_SCLOC( IDCB2 ), 'ZERO',
     :                     STATUS )
            CALL DAT_COPY( LOC2, DCB_LOC( IDCB2 ), 'ZERO',
     :                     STATUS )
            CALL DAT_ANNUL( LOC2, STATUS )

*  Indicate that scaling information is now available in the output DCB
*  entry.
            DCB_KSCL( IDCB2 ) = .TRUE.

*  Ensure the storage form is now scaled in both the data object and the
*  DCB.
            CALL CMP_MODC( DCB_LOC( IDCB2 ), 'VARIANT', 6, 0, 0,
     :                     STATUS )
            CALL CMP_PUT0C( DCB_LOC( IDCB2 ), 'VARIANT', 'SCALED',
     :                      STATUS )
            DCB_FRM( IDCB2 ) = 'SCALED'

*  If the input DCB entry is not for a scaled array, then ensure the
*  output DCB entry has no scale information.
         ELSE
            IF( DCB_SCLOC( IDCB2 ) .NE. DAT__NOLOC ) THEN
               CALL DAT_ANNUL( DCB_SCLOC( IDCB2 ), STATUS )
            END IF
            DCB_KSCL( IDCB2 ) = .FALSE.

         END IF
      END IF

*  Call error tracing routine and exit.
      IF( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CPSCL', STATUS )

      END
