      SUBROUTINE NDF_GTDLT( INDF, COMP, ZAXIS, ZTYPE, ZRATIO, STATUS )
*+
*  Name:
*     NDF_GTDLT

*  Purpose:
*     Get compression details for a DELTA compressed NDF array component

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_GTDLT( INDF, COMP, ZAXIS, ZTYPE, ZRATIO, STATUS )

*  Description:
*     The routine returns the details of the compression used by an
*     NDF array component stored in DELTA form. If the array is not
*     stored in DELTA form, then null values are returned as listed
*     below, but no error is reported.
*
*     A DELTA array is compressed by storing only the differences between
*     adjacent array values along a nominated compression axis, rather than
*     the full array values. The differences are stored using a smaller data
*     type than the original absolute values. The compression is lossless
*     because any differences that will not fit into the smaller data type
*     are stored explicitly in an extra array with a larger data type.
*     Additional compression is achieved by replacing runs of equal values
*     by a single value and a repeat count.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component; 'DATA', 'QUALITY' or 'VARIANCE'.
*     ZAXIS = INTEGER (Returned)
*        The index of the pixel axis along which compression occurred.
*        The first axis has index 1. Zero is returned if the array is not
*        stored in DELTA form.
*     ZTYPE = CHARACTER * ( * ) (Returned)
*        The data type in which the differences between adjacent array
*        values are stored. This will be one of '_BYTE', '_WORD' or
*        '_INTEGER'. The data type of the array itself is returned if the
*        supplid array is not stored in DELTA form.
*     ZRATIO = REAL (Returned)
*        The compression factor - the ratio of the uncompressed array size
*        to the compressed array size. This is approximate as it does not
*        include the effects of the metadata needed to describe the extra
*        components of a DELTA array (i.e. the space needed to hold the
*        component names, types, dimensions, etc). A value of 1.0 is
*        returned if the supplid array is not stored in DELTA form.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     3-NOV-2010 (DSB):
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

*  Arguments Given:
      INTEGER INDF
      CHARACTER COMP*(*)

*  Arguments Returned:
      INTEGER ZAXIS
      CHARACTER ZTYPE*(*)
      REAL ZRATIO

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

*  Initialise returned values
      ZAXIS = 0
      ZTYPE = ' '
      ZRATIO = 1.0

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
            CALL ERR_REP( 'NDF_GTDLT_AXI', 'Delta compression details'//
     :                    ' cannot be obtained for an AXIS component '//
     :                    '(possible programming error).', STATUS )

*  DATA component:
*  ==============
*  Use the ARY_ system to get the delta compression details for the data array.
         ELSE IF( NDF1_SIMLR( COMP( F : L ), 'DATA', NDF__MINAB ) ) THEN
            CALL ARY_GTDLT( ACB_DID( IACB ), ZAXIS, ZTYPE, ZRATIO,
     :                      STATUS )

*  EXTENSION:
*  =========
*  Report an error, since extensions have no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTDLT_EXT', 'Delta compression details'//
     :                    ' cannot be obtained for an EXTENSION '//
     :                    '(possible programming error).', STATUS )

*  HISTORY component:
*  =================
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'HISTORY',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTDLT_HIS', 'Delta compression details'//
     :                    ' cannot be obtained for an HISTORY '//
     :                    'component (possible programming error).',
     :                    STATUS )

*  LABEL component:
*  ===============
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTDLT_LAB', 'Delta compression details'//
     :                    ' cannot be obtained for an LABEL component'//
     :                    ' (possible programming error).', STATUS )

*  QUALITY component:
*  =================
*  Use the ARY_ system to get the delta compression details for the data array.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'QUALITY',
     :                         NDF__MINAB ) ) THEN

*  Ensure that quality information is available in the DCB and ACB.
            CALL NDF1_QIMP( IACB, STATUS )

*  See if the ARY_ system identifier for the quality array is valid.
*  If not, then the array does not exist.
            CALL ARY_VALID( ACB_QID( IACB ), THERE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If it exists, then get the compression details.
               IF( THERE ) THEN
                  CALL ARY_GTDLT( ACB_QID( IACB ), ZAXIS, ZTYPE, ZRATIO,
     :                            STATUS )
               END IF
            END IF

*  TITLE component:
*  ===============
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'TITLE',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTDLT_TIT', 'Delta compression details'//
     :                    ' cannot be obtained for a TITLE component '//
     :                    '(possible programming error).', STATUS )

*  UNITS component:
*  ===============
*  Report an error, since this component has no scaling.
         ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                         NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_GTDLT_UNI', 'Delta compression details'//
     :                    ' cannot be obtained for a UNITS component '//
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

*  If it exists, then get the compression details.
               IF( THERE ) THEN
                  CALL ARY_GTDLT( ACB_VID( IACB ), ZAXIS, ZTYPE, ZRATIO,
     :                            STATUS )
               END IF
            END IF

*  If the component name is not recognised, then report an error.
         ELSE
            STATUS = NDF__CNMIN
            CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
            CALL ERR_REP( 'NDF_GTDLT_COMP', 'Invalid array component '//
     :                    'name ''^BADCOMP'' specified (possible '//
     :                    'programming error).', STATUS )
         END IF

      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_GTDLT_ERR', 'NDF_GTDLT:  Error getting '//
     :                 'information about a delta compressed NDF '//
     :                 'array component.', STATUS )
         CALL NDF1_TRACE( 'NDF_GTDLT', STATUS )
      END IF

      END

