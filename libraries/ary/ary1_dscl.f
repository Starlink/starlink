      SUBROUTINE ARY1_DSCL( IDCB, STATUS )
*+
*  Name:
*     ARY1_DSCL

*  Purpose:
*     Ensure that scaling information is available for a data object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DSCL( IDCB, STATUS )

*  Description:
*     The routine ensures that values are available for the scale factor 
*     and zero offset associated an entry in the DCB. The routine does 
*     nothing if this information is already available. Otherwise, it 
*     obtains the information by inspecting the data object itself and 
*     stores the results in the DCB. Only those checks necessary to obtain 
*     the scaling information are performed on the data object.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index of the data object entry in the DCB.
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
*     26-APR-2006 (DSB):
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
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Form of the array.
*        DCB_KSCL( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Whether the DCB scaling information is up to date.
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to data object.
*        DCB_SCLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and Write)
*           The value of the SCALE component, converted to _DOUBLE.
*        DCB_TYP( ARY__MXDCB ) = CHARACTER * ( DAT__SZTYP ) (Read and Write)
*           The data type of the non-imaginary data component.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( DAT__SZLOC ) LOC2 ! Locator to SCALE or ZERO component
      DOUBLE PRECISION SCALE     ! The scale value
      DOUBLE PRECISION ZERO      ! The zero value
      INTEGER DIM( ARY__MXDIM )  ! Dimensions of SCALE or ZERO component
      INTEGER NDIM               ! No. of dimensions
      LOGICAL THERE              ! Whether a component exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do nothing if scaling information is already available in the DCB.
      IF ( .NOT. DCB_KSCL( IDCB ) ) THEN

*  Ensure that form information is available.
         CALL ARY1_DFRM( IDCB, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  Scaled arrays.
*  ==============
         IF ( DCB_FRM( IDCB ) .EQ. 'SCALED' ) THEN

*  Create a temporary HDS structure to hold the scale and zero terms in
*  their original data type. 
            CALL DAT_TEMP( 'SCZR', 0, 0, DCB_SCLOC( IDCB ), STATUS )

*  See if there is a SCALE component present in the data object
            CALL DAT_THERE( DCB_LOC( IDCB ), 'SCALE', THERE, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  If not, report an error.
            IF ( .NOT. THERE ) THEN
               STATUS = ARY__SCLIN
               CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
               CALL ERR_REP( 'ARY1_DSCL_BSIN',
     :                        'The SCALE component in missing in the '//
     :                        'scaled array structure ^ARRAY.', STATUS )

*  If there is, copy it to the temporary structure stored in the DCB.
            ELSE
               CALL DAT_FIND( DCB_LOC( IDCB ), 'SCALE', LOC2, STATUS )
               CALL DAT_COPY( LOC2, DCB_SCLOC( IDCB ), 'SCALE', STATUS )
               CALL DAT_ANNUL( LOC2, STATUS )
            END IF

*  See if there is a ZERO component present in the data object
            CALL DAT_THERE( DCB_LOC( IDCB ), 'ZERO', THERE, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  If not, report an error.
            IF ( .NOT. THERE ) THEN
               STATUS = ARY__SCLIN
               CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
               CALL ERR_REP( 'ARY1_DSCL_BSIN',
     :                        'The ZERO component in missing in the '//
     :                        'scaled array structure ^ARRAY.', STATUS )

*  If there is, copy it to the temporary structure stored in the DCB.
            ELSE
               CALL DAT_FIND( DCB_LOC( IDCB ), 'ZERO', LOC2, STATUS )
               CALL DAT_COPY( LOC2, DCB_SCLOC( IDCB ), 'ZERO', STATUS )
               CALL DAT_ANNUL( LOC2, STATUS )
            END IF

*  Verify the scale and zero values.
            CALL ARY1_VSCL( DCB_SCLOC( IDCB ), STATUS )

*  If OK, indicate that the scaling information is now available.
            IF( STATUS .EQ. SAI__OK ) DCB_KSCL( IDCB ) = .TRUE.

*  Simple and primitive arrays
*  ===========================
         ELSE IF ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' .OR.
     :             DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN
            DCB_SCLOC( IDCB ) = DAT__NOLOC

*  If the form information in the DCB was not valid, then report an
*  error.
*  ================================================================
         ELSE
            STATUS = ARY__FATIN
            CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
            CALL ERR_REP( 'ARY1_DSCL_FORM',
     :      'Unsupported array form ''^BADFORM'' found in Data ' //
     :      'Control Block (internal programming error).', STATUS )
         END IF

9999     CONTINUE
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DSCL', STATUS )

      END
