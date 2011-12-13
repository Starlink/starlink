      SUBROUTINE NDF1_ACPRP( IACB1, ICCOMP, ACCPF, IDCB2, STATUS )
*+
*  Name:
*     NDF1_ACPRP

*  Purpose:
*     Propagate axis character information from one NDF to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ACPRP( IACB1, ICCOMP, ACCPF, IDCB2, STATUS )

*  Description:
*     The routine propagates axis character information from an
*     existing NDF to a new one which is being created. Propagation is
*     controlled by a logical flag.

*  Arguments:
*     IACB1 = INTEGER (Given)
*        Index to the input NDF entry in the ACB.
*     ICCOMP = INTEGER (Given)
*        The axis character component to be propagated (one of the
*        symbolic values NDF__ALAB or NDF__AUNI, as defined in the
*        include file NDF_CONST).
*     ACCPF = LOGICAL (Given)
*        Whether the component is to be propagated. The routine takes
*        no action if this value is .FALSE..
*     IDCB2 = INTEGER (Given)
*        Index to the output NDF entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  If axis character information is being propagated, then the
*     output NDF should contain an axis structure to receive this
*     information. Axis character information should not be present in
*     this output axis structure beforehand.

*  Algorithm:
*     -  If the axis component is being propagated, then obtain an
*     index to the input data object entry in the DCB.
*     -  Determine the number of NDF dimensions from the ARY_ system
*     identifier for the main data array, held in the ACB.
*     -  Loop to process each NDF dimension.
*     -  Ensure that axis character component information is available
*     in the input DCB entry.
*     -  If the input character component exists, then copy it to the
*     appropriate output axis structure element.

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
*     23-OCT-1990 (RFWS):
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

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ACCN( NDF__MXACN ) = CHARACTER * ( DAT__SZNAM ) (Read)
*           Axis character component names.
*        DCB_ACLOC( NDC__MXDIM, NDF__MXACN, NDF__MXDCB ) = CHARACTER * (
*        DAT__SZLOC ) (Read)
*           Locators to axis character components.
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read)
*           Locators to axis structure elements.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB1
      INTEGER ICCOMP
      LOGICAL ACCPF
      INTEGER IDCB2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IAX                ! Loop counter for axes
      INTEGER IDCB1              ! Index to input data object in the DCB
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds (junk)
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds (junk)

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the axis component is being propagated, then obtain an index to
*  the input data object entry in the DCB.
      IF ( ACCPF ) THEN
         IDCB1 = ACB_IDCB( IACB1 )

*  Determine the number of NDF dimensions from the ARY_ system
*  identifier for the main data array, held in the ACB.
         CALL ARY_BOUND( ACB_DID( IACB1 ), NDF__MXDIM, LBND, UBND, NDIM,
     :                   STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop to process each NDF dimension.
            DO 1 IAX = 1, NDIM

*  Ensure that axis character component information is available in the
*  input DCB entry.
               CALL NDF1_DAC( IAX, ICCOMP, IDCB1, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If the input character component exists, then copy it to the
*  appropriate output axis structure element.
                  IF ( DCB_ACLOC( IAX, ICCOMP, IDCB1 ) .NE.
     :                 DAT__NOLOC ) THEN
                     CALL DAT_COPY( DCB_ACLOC( IAX, ICCOMP, IDCB1 ),
     :                              DCB_ALOC( IAX, IDCB2 ),
     :                              DCB_ACCN( ICCOMP ), STATUS )
                  END IF
               END IF
 1          CONTINUE
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ACPRP', STATUS )

      END
