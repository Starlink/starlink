      SUBROUTINE NDF1_XLST( IACB, MXEXTN, EXTN, NEXTN, STATUS )
*+
*  Name:
*     NDF1_XLST

*  Purpose:
*     Obtain a list of the available extension names for an ACB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_XLST( IACB, MXEXTN, EXTN, NEXTN, STATUS )

*  Description:
*     The routine returns the names of the NDF extensions associated with
*     the specified ACB entry.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the ACB entry for which extension information is
*        required.
*     MXEXTN = INTEGER (Given)
*        The maximum number of extension names to return.
*     EXTN( MXEXTN ) = CHARACTER * ( DAT__SZNAM ) (Returned)
*        The names of the extensions in the DCB entry.
*     NEXTN = INTEGER (Returned)
*        The number of names returned in EXTN.
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
*     DSB: David S Berry (JAC, HAwaii)
*     {enter_new_authors_here}

*  History:
*     22-FEB-2010 (DSB):
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
      INCLUDE 'NDF_CONST'        ! NDF_ private constant
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_XLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to extension (MORE) structure.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB
      INTEGER MXEXTN

*  Arguments Returned:
      CHARACTER * ( DAT__SZNAM ) EXTN( * )
      INTEGER NEXTN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Extension component locator
      INTEGER IDCB               ! Input data object DCB entry index
      INTEGER IEXTN              ! Loop counter for components

*.

*  Set an initial value for the NEXTN argument.
      NEXTN = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an index to the DCB entry of the input data object.
      IDCB = ACB_IDCB( IACB )

*  Ensure that extension (MORE) structure information is available in
*  the DCB.
      CALL NDF1_DX( IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If there is no extension (MORE) structure in the NDF, then return an
*  empty list.
         IF ( DCB_XLOC( IDCB ) .NE. DAT__NOLOC ) THEN

*  Get the number of extensions in the data object.
            CALL DAT_NCOMP( DCB_XLOC( IDCB ), NEXTN, STATUS )

*  Limit this to the size of the supplied EXTN array.
            NEXTN = MIN( NEXTN, MXEXTN )

*  Loop round the required extensions.
            DO IEXTN = 1, NEXTN

*  Obtain a locator to the IEXTN'th extension structure component.
               CALL DAT_INDEX( DCB_XLOC( IDCB ), IEXTN, LOC, STATUS )

*  Obtain its name.
               CALL DAT_NAME( LOC, EXTN( IEXTN ), STATUS )

*  Annul the locator.
               CALL DAT_ANNUL( LOC, STATUS )
            END DO

         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_XLST', STATUS )

      END
