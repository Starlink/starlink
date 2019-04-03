      SUBROUTINE NDF1_AWRST( IAX, IACB, STATUS )
*+
*  Name:
*     NDF1_AWRST

*  Purpose:
*     Reset an axis width array to an undefined state.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWRST( IAX, IACB, STATUS )

*  Description:
*     The routine resets an axis width array to an undefined state by
*     deleting it if it already exists.

*  Arguments:
*     IAX = INTEGER (Given)
*        Number of the NDF axis whose width array is to be reset.
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  An axis width array cannot be reset with this routine while
*     mapped access to any part of it is in effect.
*     -  This routine may only be used to reset an axis width array for
*     a base NDF. It returns without action if the NDF supplied is a
*     section.

*  Algorithm:
*     -  Obtain an index to the data object entry in the DCB.
*     -  Ensure that axis width array information is available in the
*     DCB.
*     -  Check that the axis width array is not mapped for access
*     through this ACB entry. Report an error if it is.
*     -  Check that the NDF is not a section. There is nothing to do if
*     it is.
*     -  Check that there are no other mappings to this axis width
*     array. Report an error if there are.
*     -  If the axis width array exists, then obtain its storage form
*     and numeric type. Store these in the DCB in case the array later
*     needs to be re-created.
*     -  Delete the array.
*     -  Note if the array information held in the DCB is correct.

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
*     15-OCT-1990 (RFWS):
*        Original version.
*     16-OCT-1990 (RFWS):
*        Added saving of storage form and numeric type before deleting
*        the array.
*     23-OCT-1990 (RFWS):
*        Added status check before ARY_DELET and noted whether the DCB
*        array information is correct afterwards.
*     2-NOV-1990 (RFWS):
*        Removed the status check after reconsideration.
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
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_AWFRM( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM
*        ) (Write)
*           Storage form of axis width arrays.
*        DCB_AWID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Write)
*           ARY_ system identifiers for axis width arrays.
*        DCB_AWTYP( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP
*        ) (Write)
*           Numeric data type of axis width arrays.
*        DCB_KAW( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Write)
*           Whether information is available about axis width arrays.
*        DCB_NAWMP( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read)
*           Number of current mappings to each axis width array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_AWMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read)
*           Whether NDF axis width arrays are currently mapped for
*           access.
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
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

*  Ensure that axis width array information is available in the DCB.
      CALL NDF1_DAW( IAX, IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the axis width array is not mapped for access through
*  this ACB entry. Report an error if it is.
         IF ( ACB_AWMAP( IAX, IACB ) ) THEN
            STATUS = NDF__ISMAP
            CALL MSG_SETI( 'AXIS', IAX )
            CALL NDF1_AMSG( 'NDF', IACB )
            CALL ERR_REP( 'NDF1_AWRST_MAP1',
     :                    'The width array for axis ^AXIS of the ' //
     :                    'NDF structure ^NDF is already mapped for ' //
     :                    'access through the specified identifier ' //
     :                    '(possible programming error).', STATUS )

*  Check that the NDF is not a section. There is nothing to do if it is.
         ELSE IF ( .NOT. ACB_CUT( IACB ) ) THEN

*  Check that there are no other mappings to this axis width array.
*  Report an error if there are.
            IF ( DCB_NAWMP( IAX, IDCB ) .NE. 0 ) THEN
               STATUS = NDF__ISMAP
               CALL MSG_SETI( 'AXIS', IAX )
               CALL NDF1_DMSG( 'NDF', IDCB )
               CALL ERR_REP( 'NDF1_AWRST_MAP2',
     :                       'The width array for axis ^AXIS of the ' //
     :                       'NDF structure ^NDF is already mapped ' //
     :                       'for access through another identifier ' //
     :                       '(possible programming error).', STATUS )

*  If the axis width array exists, then obtain its storage form and
*  numeric type. Store these in the DCB in case the array later needs
*  to be re-created.
            ELSE IF ( DCB_AWID( IAX, IDCB ) .NE. ARY__NOID ) THEN
               CALL ARY_FORM( DCB_AWID( IAX, IDCB ),
     :                        DCB_AWFRM( IAX, IDCB ), STATUS )
               CALL ARY_TYPE( DCB_AWID( IAX, IDCB ),
     :                        DCB_AWTYP( IAX, IDCB ), STATUS )

*  Delete the array.
               CALL ARY_DELET( DCB_AWID( IAX, IDCB ), STATUS )

*  Note if the array information held in the DCB is correct.
               DCB_KAW( IAX, IDCB ) = STATUS .EQ. SAI__OK
            END IF
         END IF
      ENDIF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWRST', STATUS )

      END
