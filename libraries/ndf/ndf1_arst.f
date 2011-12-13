      SUBROUTINE NDF1_ARST( IACB, STATUS )
*+
*  Name:
*     NDF1_ARST

*  Purpose:
*     Reset an NDF's axis component to an undefined state.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ARST( IACB, STATUS )

*  Description:
*     The routine resets an NDF's axis coordinate system to an
*     undefined state by erasing the AXIS component in the data object
*     (along with all its sub-components).  All necessary cleaning up
*     operations are performed.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  An axis component cannot be reset while mapped access to any
*     of its arrays (or any part of them) is in effect. An error will
*     rssult if this is the case.
*     -  An axis component can only be reset via a base NDF. If the NDF
*     supplied is a section, then this routine will return without
*     action. No error will result.

*  Algorithm:
*     -  Obtain the number of NDF dimensions.
*     -  Loop through all possible NDF axes, resetting the axis label,
*     units, variance and width components.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Loop to check the axis data array for each NDF dimension to
*     determine whether it is mapped for access. Report an error if it
*     is.
*     -  Check whether this is an NDF section. There is nothing more to
*     do if it is.
*     -  Loop through each possible NDF dimension, checking whether the
*     axis data array currently has mappings associated with it. Report
*     an error if it does.
*     -  Initially assume there is no axis structure present.
*     -  Loop to reset the data array on each axis. Ensure that axis
*     data array information is available in the DCB.
*     -  If the axis data array exists, then note that an NDF axis
*     structure must also exist.
*     -  Obtain the axis data array storage form and numeric type and
*     store these in the DCB in case the array must later be
*     re-created.
*     -  Delete the axis data array and annul the locator to the axis
*     structure element which contains it.
*     -  If an axis structure exists, then erase it.

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
*     16-OCT-1990 (RFWS):
*        Original version.
*     4-DEC-1990 (RFWS):
*        Changed to call ARY_NDIM instead of ARY_BOUND.
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
*        DCB_ADFRM( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM
*        ) (Write)
*           Storage form of axis data arrays.
*        DCB_ADID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           ARY_ system identifiers for axis data arrays.
*        DCB_ADTYP( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP
*        ) (Write)
*           Numeric data type of axis data arrays.
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Write)
*           Locators to axis structure elements.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_NADMP( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read)
*           Number of current mappings to each axis data array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ADMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read)
*           Whether NDF axis data arrays are currently mapped for
*           access.
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IAX                ! Loop counter for axes
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER NDIM               ! Number of NDF dimensions
      LOGICAL THERE              ! Does an axis structure exist?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the number of NDF dimensions.
      CALL ARY_NDIM( ACB_DID( IACB ), NDIM, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop through all possible NDF axes, resetting the axis label, units,
*  variance and width components.
         DO 1 IAX = 1, NDIM
            CALL NDF1_ACRST( IAX, NDF__ALAB, IACB, STATUS )
            CALL NDF1_ACRST( IAX, NDF__AUNI, IACB, STATUS )
            CALL NDF1_AVRST( IAX, IACB, STATUS )
            CALL NDF1_AWRST( IAX, IACB, STATUS )
 1       CONTINUE
      END IF

*  Obtain an index to the data object entry in the DCB.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IDCB = ACB_IDCB( IACB )

*  Loop to check the axis data array for each NDF dimension to
*  determine whether it is mapped for access. Report an error if it is.
         DO 2 IAX = 1, NDIM
            IF ( ACB_ADMAP( IAX, IACB ) ) THEN
               STATUS = NDF__ISMAP
               CALL MSG_SETI( 'AXIS', IAX )
               CALL NDF1_AMSG( 'NDF', IACB )
               CALL ERR_REP( 'NDF1_ARST_MAP1',
     :                       'The centre array for axis ^AXIS of ' //
     :                       'the NDF structure ^NDF is already ' //
     :                       'mapped for access through the ' //
     :                       'specified identifier (possible ' //
     :                       'programming error).', STATUS )
               GO TO 3
            END IF
 2       CONTINUE
 3       CONTINUE
      END IF

*  Check whether this is an NDF section. There is nothing more to do if
*  it is.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( .NOT. ACB_CUT( IACB ) ) THEN

*  Loop through each possible NDF dimension, checking whether the axis
*  data array currently has mappings associated with it. Report an
*  error if it does.
            DO 4 IAX = 1, NDIM
               IF ( DCB_NADMP( IAX, IDCB ) .NE. 0 ) THEN
                  STATUS = NDF__ISMAP
                  CALL MSG_SETI( 'AXIS', IAX )
                  CALL NDF1_DMSG( 'NDF', IDCB )
                  CALL ERR_REP( 'NDF1_ARST_MAP2',
     :                          'The centre array for axis ^AXIS of ' //
     :                          'the NDF structure ^NDF is already ' //
     :                          'mapped for access through another ' //
     :                          'identifier (possible programming ' //
     :                          'error).', STATUS )
                  GO TO 5
               END IF
 4          CONTINUE
 5          CONTINUE

*  Initially assume there is no axis structure present.
            IF ( STATUS .EQ. SAI__OK ) THEN
               THERE = .FALSE.

*  Loop to reset the data array on each axis. Ensure that axis data
*  array information is available in the DCB.
               DO 6 IAX = 1, NDIM
                  CALL NDF1_DAD( IAX, IDCB, STATUS )

*  If the axis data array exists, then note that an NDF axis structure
*  must also exist.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( DCB_ADID( IAX, IDCB ) .NE. ARY__NOID ) THEN
                        THERE = .TRUE.

*  Obtain the axis data array storage form and numeric type and store
*  these in the DCB in case the array must later be re-created.
                        CALL ARY_FORM( DCB_ADID( IAX, IDCB ),
     :                                 DCB_ADFRM( IAX, IDCB ), STATUS )
                        CALL ARY_TYPE( DCB_ADID( IAX, IDCB ),
     :                                 DCB_ADTYP( IAX, IDCB ), STATUS )

*  Delete the axis data array and annul the locator to the axis
*  structure element which contains it.
                        CALL ARY_DELET( DCB_ADID( IAX, IDCB ), STATUS )
                        CALL DAT_ANNUL( DCB_ALOC( IAX, IDCB ), STATUS )
                     END IF
                  END IF
 6             CONTINUE

*  If an axis structure exists, then erase it.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( THERE ) THEN
                     CALL DAT_ERASE( DCB_LOC( IDCB ), 'AXIS', STATUS )
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ARST', STATUS )

      END
