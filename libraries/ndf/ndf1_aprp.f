      SUBROUTINE NDF1_APRP( IACB1, ACPF, IDCB2, STATUS )
*+
*  Name:
*     NDF1_APRP

*  Purpose:
*     Propagate NDF axis structure information from one NDF to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_APRP( IACB1, ACPF, IDCB2, STATUS )

*  Description:
*     The routine propagates attributes and information from one NDF's
*     axis structure to a new one which is being created.  If required,
*     only the attributes may be propagated, leaving the actual data
*     values behind.

*  Arguments:
*     IACB1 = INTEGER (Given)
*        Index to the input NDF entry in the ACB.
*     ACPF = LOGICAL (Given)
*        Whether to propagate the axis information (as opposed to
*        simply propagating its attributes).
*     IDCB2 = INTEGER (Given)
*        Index to the output NDF entry in the DCB. This NDF should not
*        contain an axis structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Obtain an index to the input data object entry in the DCB.
*     -  Determine the number of input NDF dimensions from its data
*     array entry in the ACB.
*     -  Ensure that input axis structure information is available in
*     the DCB.
*     -  Set null initial DCB locator values for the output axis
*     structure elements.
*     -  If the axis component is being propagated and an axis
*     structure exists in the input NDF, then create a corresponding
*     axis structure with an appropriate number of elements in the
*     output NDF.
*     -  Obtain a locator to the new structure.
*     -  Loop to obtain a locator to each structure element (i.e. cell)
*     and store it in the DCB.
*     -  Annul the locator to the whole axis structure.
*     -  Propagate the axis data array information.
*     -  If an error occurred then annul any axis structure locators
*     which may have been acquired.
*     -  Erase the axis structure, ignoring any errors.
*     -  Note if axis structure information in the new DCB entry is
*     correct.
*     -  Propagate the axis character component information.
*     -  Propagate the variance and width array information.
*     -  Loop to propagate the axis normalisation flag information for
*     each input NDF axis.  Ensure that normalisation information is
*     available for the input NDF.
*     -  Propagate the normalisation value.
*     -  If axis values are being propagated and an axis structure
*     exists, then copy the NORMALISED component to the output
*     structure.
*     -  Note if the new information is correct.
*     -  Propagate any MORE (extension) components present in the input
*     axis structure.

*  Implementation Deficiencies:
*     Propagation of the axis extension component is only handled in a
*     rudimentary way.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     22-OCT-1990 (RFWS):
*        Original version.
*     30-OCT-1990 (RFWS):
*        Improved the error recovery and propagation of the normalised
*        and extension components.
*     1-NOV-1990 (RFWS):
*        Made further improvements to error recovery.
*     13-NOV-1990 (RFWS):
*        Added check that axis structure locator is valid before
*        attempting to propagate an axis extension structure.
*     14-NOV-1990 (RFWS):
*        Re-structured propagation of the axis extension components.
*     4-DEC-1990 (RFWS):
*        Improved the handling of the axis NORMALISED component.
*     18-DEC-1990 (RFWS):
*        Improved tests for axis structure existence.
*     2-JAN-1991 (RFWS):
*        Removed unnecessary use of ARY_PAR include file.
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

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ADID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           ARY_ system identifiers for axis data arrays.
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read and Write)
*           Locators to axis structure elements.
*        DCB_ANRM( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read and Write)
*           Axis normalisation value.
*        DCB_KA( NDF__MXDCB ) = LOGICAL (Write)
*           Whether axis component information is available.
*        DCB_KAN( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Write)
*           Whether information is available about axis normalisation.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB1
      LOGICAL ACPF
      INTEGER IDCB2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) ALOC ! Axis structure locator
      INTEGER ADIM( 1 )          ! Axis structure dimension size
      INTEGER CELL( 1 )          ! Axis structure cell index
      INTEGER IAX                ! Loop counter for axes
      INTEGER IDCB1              ! DCB index for input data object
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER TSTAT              ! Temporary status variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an index to the input data object entry in the DCB.
      IDCB1 = ACB_IDCB( IACB1 )

*  Determine the number of input NDF dimensions from its data array
*  entry in the ACB.
      CALL ARY_NDIM( ACB_DID( IACB1 ), NDIM, STATUS )

*  Ensure that input axis structure information is available in the
*  DCB.
      CALL NDF1_DA( IDCB1, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Set null initial DCB locator values for the output axis structure
*  elements.
         DO 1 IAX = 1, NDF__MXDIM
            DCB_ALOC( IAX, IDCB2 ) = DAT__NOLOC
 1       CONTINUE

*  If the axis component is being propagated and an axis structure
*  exists in the input NDF, then create a corresponding axis structure
*  with an appropriate number of elements in the output NDF.
         IF ( ACPF .AND. ( DCB_ALOC( 1, IDCB1 ) .NE. DAT__NOLOC ) ) THEN
            ADIM( 1 ) = NDIM
            CALL DAT_NEW( DCB_LOC( IDCB2 ), 'AXIS', 'AXIS', 1, ADIM,
     :                    STATUS )

*  Obtain a locator to the new structure.
            CALL DAT_FIND( DCB_LOC( IDCB2 ), 'AXIS', ALOC, STATUS )

*  Loop to obtain a locator to each structure element (i.e. cell) and
*  store it in the DCB.
            DO 2 IAX = 1, NDIM
               CELL( 1 ) = IAX
               CALL DAT_CELL( ALOC, 1, CELL, DCB_ALOC( IAX, IDCB2 ),
     :                        STATUS )
 2          CONTINUE

*  Annul the locator to the whole axis structure.
            CALL DAT_ANNUL( ALOC, STATUS )
         END IF

*  Propagate the axis data array information.
         CALL NDF1_ADPRP( IACB1, ACPF, IDCB2, STATUS )

*  If an error occurred then annul any axis structure locators which may
*  have been acquired.
         IF ( STATUS .NE. SAI__OK ) THEN
            DO 3 IAX = 1, NDIM
               CALL DAT_ANNUL( DCB_ALOC( IAX, IDCB2 ), STATUS )
 3          CONTINUE

*  Erase the axis structure, ignoring any errors.
            CALL ERR_MARK
            TSTAT = SAI__OK
            CALL DAT_ERASE( DCB_LOC( IDCB2 ), 'AXIS', STATUS )
            CALL ERR_ANNUL( TSTAT )
         END IF

*  Note if axis structure information in the new DCB entry is correct.
         DCB_KA( IDCB2 ) = STATUS .EQ. SAI__OK

*  Propagate the axis character component information.
         CALL NDF1_ACPRP( IACB1, NDF__ALAB, ACPF, IDCB2, STATUS )
         CALL NDF1_ACPRP( IACB1, NDF__AUNI, ACPF, IDCB2, STATUS )

*  Propagate the variance and width array information.
         CALL NDF1_AVPRP( IACB1, ACPF, IDCB2, STATUS )
         CALL NDF1_AWPRP( IACB1, ACPF, IDCB2, STATUS )

*  Loop to propagate the axis normalisation flag information for each
*  input NDF axis.  Ensure that normalisation information is available
*  for the input NDF.
         IF ( STATUS .EQ. SAI__OK ) THEN
            DO 4 IAX = 1, NDIM
               CALL NDF1_DAN( IAX, IDCB1, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Propagate the normalisation value.
                  DCB_ANRM( IAX, IDCB2 ) = DCB_ANRM( IAX, IDCB1 )

*  If axis values are being propagated and an axis structure exists,
*  then copy the NORMALISED component to the output structure.
                  IF ( ACPF .AND.
     :                 ( DCB_ALOC( IAX, IDCB1 ) .NE. DAT__NOLOC ) ) THEN
                     CALL NDF1_CPYNC( DCB_ALOC( IAX, IDCB1 ),
     :                                'NORMALISED',
     :                                DCB_ALOC( IAX, IDCB2 ), STATUS )
                  END IF
               END IF

*  Note if the new information is correct.
               DCB_KAN( IAX, IDCB2 ) = STATUS .EQ. SAI__OK
 4          CONTINUE
         END IF

*  Propagate any MORE (extension) components present in the input axis
*  structure.
         IF ( STATUS .EQ. SAI__OK ) THEN
            DO 5 IAX = 1, NDIM
               IF ( ACPF .AND.
     :              ( DCB_ALOC( IAX, IDCB1 ) .NE. DAT__NOLOC ) ) THEN
                        CALL NDF1_CPYNC( DCB_ALOC( IAX, IDCB1 ), 'MORE',
     :                                   DCB_ALOC( IAX, IDCB2 ),
     :                                   STATUS )
               END IF
 5          CONTINUE
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_APRP', STATUS )

      END
