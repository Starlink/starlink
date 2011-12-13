      SUBROUTINE NDF_GTSZD( INDF, COMP, SCALE, ZERO, STATUS )
*+
*  Name:
*     NDF_GTSZD

*  Purpose:
*     Get the scale and zero values for an NDF array component

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_GTSZD( INDF, COMP, SCALE, ZERO, STATUS )

*  Description:
*     The routine returns the scale and zero values associated with an
*     NDF array component. If the array is stored in simple or primitive
*     form, then values of 1.0 and 0.0 are returned.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component; 'DATA' or 'VARIANCE'.
*     SCALE = DOUBLE PRECISION (Returned)
*        The new value for the scaling factor.
*     ZERO = DOUBLE PRECISION (Returned)
*        The new value for the zero offset.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - There is a routine for each of the standard Fortran numerical data
*     types: integer, real and double precision. Replace the (lower case) "x"
*     in the routine name by I, R or D as appropriate.

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
*     7-JUL-2006 (DSB):
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
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_DMBAD( NDF__MXACB ) = LOGICAL (Write)
*           Bad pixel flag for the mapped data values.
*        ACB_DMBMD( NDF__MXACB ) = LOGICAL (Write)
*           Whether the ACB_VMBAD value has been modified.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP

*  Arguments Returned:
      DOUBLE PRECISION SCALE
      DOUBLE PRECISION ZERO

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! Position of first component character
      INTEGER I2                 ! Position of last component character
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER L                  ! Position of last non-blank character
      INTEGER NCOMP              ! Number non-blank components specified
      LOGICAL THERE              ! Whether the variance array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN

*  Find the first and last non-blank characters in the supplied component
*  name.
         CALL CHR_FANDL( COMP, F, L )

*  Compare the component name with each value in turn (allowing
*  abbreviation), and take the appropriate action, or report an error
*  if an inappropriate component name has been given.

*  AXIS component:
*  ==============
*  Report an error, since this component has no scaling.
         IF ( NDF1_SIMLR( COMP( F : L ), 'AXIS', NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZD_AXI', 'Scale and zero values '//
     :                    'cannot be obtained for an AXIS component '//
     :                    '(possible programming error).', STATUS )

*  DATA component:
*  ==============
*  Use the ARY_ system to get the scale and zero values for the data array.
         ELSE IF( NDF1_SIMLR( COMP( F : L ), 'DATA', NDF__MINAB ) ) THEN
            CALL ARY_GTSZD( ACB_DID( IACB ), SCALE, ZERO, STATUS )

*  EXTENSION:
*  =========
*  Report an error, since extensions have no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZD_EXT', 'Scale and zero values '//
     :                    'cannot be obtained for an EXTENSION '//
     :                    '(possible programming error).', STATUS )

*  HISTORY component:
*  =================
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'HISTORY',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZD_HIS', 'Scale and zero values '//
     :                    'cannot be obtained for an HISTORY '//
     :                    'component (possible programming error).',
     :                    STATUS )

*  LABEL component:
*  ===============
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZD_LAB', 'Scale and zero values '//
     :                    'cannot be obtained for an LABEL component '//
     :                    '(possible programming error).', STATUS )

*  QUALITY component:
*  =================
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'QUALITY',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZD_QUA', 'Scale and zero values '//
     :                    'cannot be obtained for a QUALITY component'//
     :                    ' (possible programming error).', STATUS )

*  TITLE component:
*  ===============
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'TITLE',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZD_TIT', 'Scale and zero values '//
     :                    'cannot be obtained for a TITLE component '//
     :                    '(possible programming error).', STATUS )

*  UNITS component:
*  ===============
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZD_UNI', 'Scale and zero values '//
     :                    'cannot be obtained for a UNITS component '//
     :                    '(possible programming error).', STATUS )

*  VARIANCE component:
*  ==================
*  Ensure that variance information is available in the DCB and ACB.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                         NDF__MINAB ) ) THEN
            CALL NDF1_VIMP( IACB, STATUS )

*  See if the ARY_ system identifier for the variance array is valid.
*  If not, then the array does not exist.
            CALL ARY_VALID( ACB_VID( IACB ), THERE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If it exists, then get the scaling.
               IF( THERE ) THEN
                  CALL ARY_GTSZD( ACB_VID( IACB ), SCALE, ZERO, STATUS )
               END IF
            END IF

*  If the component name is not recognised, then report an error.
         ELSE
            STATUS = NDF__CNMIN
            CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
            CALL ERR_REP( 'NDF_GTSZD_COMP', 'Invalid array component '//
     :                    'name ''^BADCOMP'' specified (possible '//
     :                    'programming error).', STATUS )
         END IF

      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_GTSZD_ERR', 'NDF_GTSZD: Error setting '//
     :                 'the scale and zero values for an NDF array '//
     :                 'component.', STATUS )
         CALL NDF1_TRACE( 'NDF_GTSZD', STATUS )
      END IF

      END

      SUBROUTINE NDF_GTSZI( INDF, COMP, SCALE, ZERO, STATUS )
*+
*  Name:
*     NDF_GTSZI

*  Purpose:
*     Get the scale and zero values for an NDF array component

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_GTSZI( INDF, COMP, SCALE, ZERO, STATUS )

*  Description:
*     The routine returns the scale and zero values associated with an
*     NDF array component. If the array is stored in simple or primitive
*     form, then values of 1.0 and 0.0 are returned.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component; 'DATA' or 'VARIANCE'.
*     SCALE = INTEGER (Returned)
*        The new value for the scaling factor.
*     ZERO = INTEGER (Returned)
*        The new value for the zero offset.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - There is a routine for each of the standard Fortran numerical data
*     types: integer, real and double precision. Replace the (lower case) "x"
*     in the routine name by I, R or D as appropriate.

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
*     7-JUL-2006 (DSB):
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
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_DMBAD( NDF__MXACB ) = LOGICAL (Write)
*           Bad pixel flag for the mapped data values.
*        ACB_DMBMD( NDF__MXACB ) = LOGICAL (Write)
*           Whether the ACB_VMBAD value has been modified.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP

*  Arguments Returned:
      INTEGER SCALE
      INTEGER ZERO

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! Position of first component character
      INTEGER I2                 ! Position of last component character
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER L                  ! Position of last non-blank character
      INTEGER NCOMP              ! Number non-blank components specified
      LOGICAL THERE              ! Whether the variance array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN

*  Find the first and last non-blank characters in the supplied component
*  name.
         CALL CHR_FANDL( COMP, F, L )

*  Compare the component name with each value in turn (allowing
*  abbreviation), and take the appropriate action, or report an error
*  if an inappropriate component name has been given.

*  AXIS component:
*  ==============
*  Report an error, since this component has no scaling.
         IF ( NDF1_SIMLR( COMP( F : L ), 'AXIS', NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZI_AXI', 'Scale and zero values '//
     :                    'cannot be obtained for an AXIS component '//
     :                    '(possible programming error).', STATUS )

*  DATA component:
*  ==============
*  Use the ARY_ system to get the scale and zero values for the data array.
         ELSE IF( NDF1_SIMLR( COMP( F : L ), 'DATA', NDF__MINAB ) ) THEN
            CALL ARY_GTSZI( ACB_DID( IACB ), SCALE, ZERO, STATUS )

*  EXTENSION:
*  =========
*  Report an error, since extensions have no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZI_EXT', 'Scale and zero values '//
     :                    'cannot be obtained for an EXTENSION '//
     :                    '(possible programming error).', STATUS )

*  HISTORY component:
*  =================
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'HISTORY',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZI_HIS', 'Scale and zero values '//
     :                    'cannot be obtained for an HISTORY '//
     :                    'component (possible programming error).',
     :                    STATUS )

*  LABEL component:
*  ===============
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZI_LAB', 'Scale and zero values '//
     :                    'cannot be obtained for an LABEL component '//
     :                    '(possible programming error).', STATUS )

*  QUALITY component:
*  =================
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'QUALITY',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZI_QUA', 'Scale and zero values '//
     :                    'cannot be obtained for a QUALITY component'//
     :                    ' (possible programming error).', STATUS )

*  TITLE component:
*  ===============
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'TITLE',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZI_TIT', 'Scale and zero values '//
     :                    'cannot be obtained for a TITLE component '//
     :                    '(possible programming error).', STATUS )

*  UNITS component:
*  ===============
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZI_UNI', 'Scale and zero values '//
     :                    'cannot be obtained for a UNITS component '//
     :                    '(possible programming error).', STATUS )

*  VARIANCE component:
*  ==================
*  Ensure that variance information is available in the DCB and ACB.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                         NDF__MINAB ) ) THEN
            CALL NDF1_VIMP( IACB, STATUS )

*  See if the ARY_ system identifier for the variance array is valid.
*  If not, then the array does not exist.
            CALL ARY_VALID( ACB_VID( IACB ), THERE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If it exists, then get the scaling.
               IF( THERE ) THEN
                  CALL ARY_GTSZI( ACB_VID( IACB ), SCALE, ZERO, STATUS )
               END IF
            END IF

*  If the component name is not recognised, then report an error.
         ELSE
            STATUS = NDF__CNMIN
            CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
            CALL ERR_REP( 'NDF_GTSZI_COMP', 'Invalid array component '//
     :                    'name ''^BADCOMP'' specified (possible '//
     :                    'programming error).', STATUS )
         END IF

      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_GTSZI_ERR', 'NDF_GTSZI: Error setting '//
     :                 'the scale and zero values for an NDF array '//
     :                 'component.', STATUS )
         CALL NDF1_TRACE( 'NDF_GTSZI', STATUS )
      END IF

      END

      SUBROUTINE NDF_GTSZR( INDF, COMP, SCALE, ZERO, STATUS )
*+
*  Name:
*     NDF_GTSZR

*  Purpose:
*     Get the scale and zero values for an NDF array component

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_GTSZR( INDF, COMP, SCALE, ZERO, STATUS )

*  Description:
*     The routine returns the scale and zero values associated with an
*     NDF array component. If the array is stored in simple or primitive
*     form, then values of 1.0 and 0.0 are returned.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component; 'DATA' or 'VARIANCE'.
*     SCALE = REAL (Returned)
*        The new value for the scaling factor.
*     ZERO = REAL (Returned)
*        The new value for the zero offset.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - There is a routine for each of the standard Fortran numerical data
*     types: integer, real and double precision. Replace the (lower case) "x"
*     in the routine name by I, R or D as appropriate.

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
*     7-JUL-2006 (DSB):
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
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_DMBAD( NDF__MXACB ) = LOGICAL (Write)
*           Bad pixel flag for the mapped data values.
*        ACB_DMBMD( NDF__MXACB ) = LOGICAL (Write)
*           Whether the ACB_VMBAD value has been modified.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP

*  Arguments Returned:
      REAL SCALE
      REAL ZERO

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! Position of first component character
      INTEGER I2                 ! Position of last component character
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER L                  ! Position of last non-blank character
      INTEGER NCOMP              ! Number non-blank components specified
      LOGICAL THERE              ! Whether the variance array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN

*  Find the first and last non-blank characters in the supplied component
*  name.
         CALL CHR_FANDL( COMP, F, L )

*  Compare the component name with each value in turn (allowing
*  abbreviation), and take the appropriate action, or report an error
*  if an inappropriate component name has been given.

*  AXIS component:
*  ==============
*  Report an error, since this component has no scaling.
         IF ( NDF1_SIMLR( COMP( F : L ), 'AXIS', NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZR_AXI', 'Scale and zero values '//
     :                    'cannot be obtained for an AXIS component '//
     :                    '(possible programming error).', STATUS )

*  DATA component:
*  ==============
*  Use the ARY_ system to get the scale and zero values for the data array.
         ELSE IF( NDF1_SIMLR( COMP( F : L ), 'DATA', NDF__MINAB ) ) THEN
            CALL ARY_GTSZR( ACB_DID( IACB ), SCALE, ZERO, STATUS )

*  EXTENSION:
*  =========
*  Report an error, since extensions have no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZR_EXT', 'Scale and zero values '//
     :                    'cannot be obtained for an EXTENSION '//
     :                    '(possible programming error).', STATUS )

*  HISTORY component:
*  =================
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'HISTORY',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZR_HIS', 'Scale and zero values '//
     :                    'cannot be obtained for an HISTORY '//
     :                    'component (possible programming error).',
     :                    STATUS )

*  LABEL component:
*  ===============
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZR_LAB', 'Scale and zero values '//
     :                    'cannot be obtained for an LABEL component '//
     :                    '(possible programming error).', STATUS )

*  QUALITY component:
*  =================
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'QUALITY',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZR_QUA', 'Scale and zero values '//
     :                    'cannot be obtained for a QUALITY component'//
     :                    ' (possible programming error).', STATUS )

*  TITLE component:
*  ===============
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'TITLE',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZR_TIT', 'Scale and zero values '//
     :                    'cannot be obtained for a TITLE component '//
     :                    '(possible programming error).', STATUS )

*  UNITS component:
*  ===============
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTSZR_UNI', 'Scale and zero values '//
     :                    'cannot be obtained for a UNITS component '//
     :                    '(possible programming error).', STATUS )

*  VARIANCE component:
*  ==================
*  Ensure that variance information is available in the DCB and ACB.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                         NDF__MINAB ) ) THEN
            CALL NDF1_VIMP( IACB, STATUS )

*  See if the ARY_ system identifier for the variance array is valid.
*  If not, then the array does not exist.
            CALL ARY_VALID( ACB_VID( IACB ), THERE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If it exists, then get the scaling.
               IF( THERE ) THEN
                  CALL ARY_GTSZR( ACB_VID( IACB ), SCALE, ZERO, STATUS )
               END IF
            END IF

*  If the component name is not recognised, then report an error.
         ELSE
            STATUS = NDF__CNMIN
            CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
            CALL ERR_REP( 'NDF_GTSZR_COMP', 'Invalid array component '//
     :                    'name ''^BADCOMP'' specified (possible '//
     :                    'programming error).', STATUS )
         END IF

      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_GTSZR_ERR', 'NDF_GTSZR: Error setting '//
     :                 'the scale and zero values for an NDF array '//
     :                 'component.', STATUS )
         CALL NDF1_TRACE( 'NDF_GTSZR', STATUS )
      END IF

      END

