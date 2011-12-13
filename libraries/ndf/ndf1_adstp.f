      SUBROUTINE NDF1_ADSTP( TYPE, IAX, IACB, STATUS )
*+
*  Name:
*     NDF1_ADSTP

*  Purpose:
*     Set a new numeric type for an axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADSTP( TYPE, IAX, IACB, STATUS )

*  Description:
*     The routine sets a new numeric type for an NDF's axis data array.
*     If the array exists, then its existing values will be converted as
*     necessary.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The new numeric type (case insensitive).
*     IAX = INTEGER (Given)
*        Number of the NDF axis whose data array numeric type is to be
*        modified.
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The numeric type of an axis data array cannot be changed with
*     this routine while mapped access to any part of it is in effect.
*     -  This routine may only be used to set a numeric type for the
*     axis data array of a base NDF. It returns without action if the
*     NDF supplied is a section.

*  Algorithm:
*     -  Obtain an index to the data object entry in the DCB.
*     -  Ensure that axis data array information is available in the
*     DCB.
*     -  Check that the axis data array is not mapped for access
*     through this ACB entry. Report an error if it is.
*     -  Check that the NDF is not a section. There is nothing to do if
*     it is.
*     -  Check that there are no other mappings to this axis data
*     array.  Report an error if there are.
*     -  If the axis data array exists, then set a new numeric type for
*     it.
*     -  Otherwise, set a new default value for its numeric type in the
*     DCB (in upper case).

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

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
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ADID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifiers for axis data arrays.
*        DCB_ADTYP( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP
*        ) (Write)
*           Numeric data type of axis data arrays.
*        DCB_NADMP( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read)
*           Number of current mappings to each axis data array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ADMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read)
*           Whether NDF axis data arrays are currently mapped for
*           access.
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER IAX
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB( IACB )

*  Ensure that axis data array information is available in the DCB.
      CALL NDF1_DAD( IAX, IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the axis data array is not mapped for access through this
*  ACB entry. Report an error if it is.
         IF ( ACB_ADMAP( IAX, IACB ) ) THEN
            STATUS = NDF__ISMAP
            CALL NDF1_AMSG( 'NDF', IACB )
            CALL MSG_SETI( 'AXIS', IAX )
            CALL ERR_REP( 'NDF1_ADSTP_MAP1',
     :                    'The centre array for axis ^AXIS of the ' //
     :                    'NDF structure ^NDF is already mapped for ' //
     :                    'access through the specified identifier ' //
     :                    '(possible programming error).', STATUS )

*  Check that the NDF is not a section. There is nothing to do if it is.
         ELSE IF ( .NOT. ACB_CUT( IACB ) ) THEN

*  Check that there are no other mappings to this axis data array.
*  Report an error if there are.
            IF ( DCB_NADMP( IAX, IDCB ) .NE. 0 ) THEN
               STATUS = NDF__ISMAP
               CALL NDF1_DMSG( 'NDF', IDCB )
               CALL MSG_SETI( 'AXIS', IAX )
               CALL ERR_REP( 'NDF1_ADSTP_MAP2',
     :                       'The centre array for axis ^AXIS of ' //
     :                       'the NDF structure ^NDF is already ' //
     :                       'mapped for access through another ' //
     :                       'identifier (possible programming error).',
     :                       STATUS )

*  If the axis data array exists, then set a new numeric type for it.
            ELSE IF ( DCB_ADID( IAX, IDCB ) .NE. ARY__NOID ) THEN
               CALL ARY_STYPE( TYPE, DCB_ADID( IAX, IDCB ), STATUS )

*  Otherwise, set a new default value for its numeric type in the DCB
*  (in upper case).
            ELSE
               DCB_ADTYP( IAX, IDCB ) = TYPE
               CALL CHR_UCASE( DCB_ADTYP( IAX, IDCB ) )
            END IF
         END IF
      ENDIF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADSTP', STATUS )

      END
