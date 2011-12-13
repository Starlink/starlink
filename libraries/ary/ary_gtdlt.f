      SUBROUTINE ARY_GTDLT( IARY, ZAXIS, ZTYPE, ZRATIO, STATUS )
*+
*  Name:
*     ARY_GTDLT

*  Purpose:
*     Get the compressed axis and data type for a DELTA array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_GTDLT( IARY, ZAXIS, ZTYPE, ZRATIO, STATUS )

*  Description:
*     The routine returns the details of the compression used to produce
*     an array stored in DELTA form. If the array is not stored in
*     DELTA form, then null values are returned as listed below, but no
*     error is reported.
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
*     IARY = INTEGER (Given)
*        Array identifier.
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
*     2-NOV-2010 (DSB):
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
      INCLUDE 'ARY_ERR'          ! ARY_ error constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block

*  Arguments Given:
      INTEGER IARY

*  Arguments Returned:
      INTEGER ZAXIS
      CHARACTER ZTYPE*(*)
      REAL ZRATIO

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TY*(DAT__SZTYP)  ! Intermediate data type
      INTEGER IACB               ! Index to array entry in the ACB
      INTEGER IDCB               ! Index to array entry in the DCB
*.

*  Initialise returned values
      ZAXIS = 0
      ZTYPE = ' '
      ZRATIO = 1.0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )

*  Get the DCB entry associated with this ACB entry.
      IDCB = ACB_IDCB( IACB )

*  Get the compression information form the data object.
      CALL ARY1_GTDLT( IDCB, ZAXIS, TY, ZRATIO, STATUS )

*  Copy the data type into the supplied character argument.
      CALL ARY1_CCPY( TY, ZTYPE, STATUS )

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_GTDLT_ERR', 'ARY_GTDLT: Error getting '//
     :                 'information about a delta compressed array.',
     :                 STATUS )
         CALL ARY1_TRACE( 'ARY_GTDLT', STATUS )
      END IF

      END
