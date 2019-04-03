      SUBROUTINE NDF1_ADCRE( LBND, UBND, IAX, IDCB, STATUS )
*+
*  Name:
*     NDF1_ADCRE

*  Purpose:
*     Create an axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADCRE( LBND, UBND, IAX, IDCB, STATUS )

*  Description:
*     The routine creates an axis data array and initialises it to
*     define the default coordinate system for an NDF axis.

*  Arguments:
*     LBND( 1 ) = INTEGER (Given)
*        Lower pixel-index bound of the axis data array.
*     UBND( 1 ) = INTEGER (Given)
*        Upper pixel-index bound of the axis data array.
*     IAX = INTEGER (Given)
*        Axis number.
*     IDCB = INTEGER (Given)
*        Index to the data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  The axis structure element which will hold the new axis data
*     array must already exist and have a locator in the DCB. The axis
*     data array should not already exist. This routine does not check
*     for these conditions itself.
*     -  DCB information must already be available for the axis whose
*     data array is being created in order to establish the default
*     array attributes. (This routine cannot establish this information
*     itself by calling NDF1_DAD because the absence of a data array
*     within an existing axis structure will be detected as an arror by
*     NDF1_DAD).

*  Algorithm:
*     -  Obtain an ARY_ system placeholder for the data array in the
*     appropriate cell of the NDF's axis structure.
*     -  Create the data array with the required storage form, numeric
*     type and shape.
*     -  If required, then create a primitive array.
*     -  Map the array, initialise its values, and then unmap it.
*     -  Otherwise, if required, create a simple array. Map the array,
*     initialise its values, and then unmap it.
*     -  If the axis data array form stored in the DCB was not
*     recognised, then report an error.
*     -  If an error occurred, then erase any axis data array which may
*     have been created.
*     -  Note if the axis data array information in the DCB is valid.

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-OCT-1990 (RFWS):
*        Original version.
*     23-OCT-1990 (RFWS):
*        Changed to handle initialisation of each array storage form
*        explicitly.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read)
*           Locators to axis structure elements.
*        DCB_ADFRM( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM
*        ) (Read)
*           Storage form of axis data arrays.
*        DCB_ADID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Write)
*           ARY_ system identifiers for axis data arrays.
*        DCB_ADTYP( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP
*        ) (Read)
*           Numeric data type of axis data arrays.
*        DCB_KAD( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Write)
*           Whether information about axis data arrays is available.

*  Arguments Given:
      INTEGER LBND( 1 )
      INTEGER UBND( 1 )
      INTEGER IAX
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EL                 ! Number of mapped elements
      INTEGER PLACE              ! ARY_ system placeholder
      INTEGER PNTR               ! Pointer to mapped array

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an ARY_ system placeholder for the data array in the
*  appropriate cell of the NDF's axis structure.
      CALL ARY_PLACE( DCB_ALOC( IAX, IDCB ), 'DATA_ARRAY', PLACE,
     :                STATUS )

*  Create the data array with the required storage form, numeric type
*  and shape.

*  PRIMITIVE:
*  =========
*  Create a primitive array.
      IF ( DCB_ADFRM( IAX, IDCB ) .EQ. 'PRIMITIVE' ) THEN
         CALL ARY_NEWP( DCB_ADTYP( IAX, IDCB ), 1, UBND, PLACE,
     :                  DCB_ADID( IAX, IDCB ), STATUS )

*  Map the array, initialise its values, and then unmap it.
         CALL ARY_MAP( DCB_ADID( IAX, IDCB ), DCB_ADTYP( IAX, IDCB ),
     :                 'WRITE', PNTR, EL, STATUS )
         CALL NDF1_ADINI( DCB_ADTYP( IAX, IDCB ), LBND( 1 ), UBND( 1 ),
     :                    PNTR, STATUS )
         CALL ARY_UNMAP( DCB_ADID( IAX, IDCB ), STATUS )

*  SIMPLE:
*  ======
*  Create a simple array.
      ELSE IF ( DCB_ADFRM( IAX, IDCB ) .EQ. 'SIMPLE' ) THEN
         CALL ARY_NEW( DCB_ADTYP( IAX, IDCB ), 1, LBND, UBND, PLACE,
     :                 DCB_ADID( IAX, IDCB ), STATUS )

*  Map the array, initialise its values, and then unmap it.
         CALL ARY_MAP( DCB_ADID( IAX, IDCB ), DCB_ADTYP( IAX, IDCB ),
     :                 'WRITE', PNTR, EL, STATUS )
         CALL NDF1_ADINI( DCB_ADTYP( IAX, IDCB ), LBND( 1 ), UBND( 1 ),
     :                    PNTR, STATUS )
         CALL ARY_UNMAP( DCB_ADID( IAX, IDCB ), STATUS )

*  If the axis data array form stored in the DCB was not recognised,
*  then report an error.
      ELSE
         STATUS = NDF__FATIN
         CALL MSG_SETC( 'BADFORM', DCB_ADFRM( IAX, IDCB ) )
         CALL ERR_REP( 'NDF1_ADCRE_FORM',
     :                 'Invalid axis array storage form ' //
     :                 '''^BADFORM'' encountered in the NDF_ system ' //
     :                 'Data Control Block (internal programming ' //
     :                 'error).', STATUS )
      END IF

*  If an error occurred, then erase any axis data array which may have
*  been created.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ARY_DELET( DCB_ADID( IAX, IDCB ), STATUS )
      END IF

*  Note if the axis data array information in the DCB is valid.
      DCB_KAD( IAX, IDCB ) = STATUS .EQ. SAI__OK

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADCRE', STATUS )

      END
