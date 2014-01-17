      SUBROUTINE NDF1_PTLOC( PARAM, IPAR, VMODE, IACB, STATUS )
*+
*  Name:
*     NDF1_PTLOC

*  Purpose:
*     Store locators for an NDF parameter in the parameter system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PTLOC( PARAM, IPAR, VMODE, IACB, STATUS )

*  Description:
*     The routine stores two locators in the SUBPAR common blocks. The
*     first is a locator for the top-level container file that contains
*     the specified NDF, and the second is a locator for NDF itself.
*
*     It also stores information about the parameter in the ADAM
*     Parameter Block, so that the NDF_CANCL routine can access it if
*     required.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter.
*     IPAR = INTEGER (Given)
*        The SUBPAR index for the parameter.
*     VMODE = CHARACTER * ( * ) (Given)
*        Validated access mode string
*     IACB = INTEGER (Given)
*        Index to NDF entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     19-OCT-2012 (DSB):
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
      INCLUDE 'AST_PAR'          ! AST_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_FILE( NDF__MXDCB ) = CHARACTER * ( NDF__SZFIL )(Read)
*           Data object container file name.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

      INCLUDE 'NDF_APB'          ! NDF_ ADAM Parameter Block
*        APB_PARS = INTEGER (Read and Write)
*           An AST KeyMap holding a list of parameters with associated
*           NDFs.

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      INTEGER IPAR
      CHARACTER * ( * ) VMODE
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to NDF structure
      CHARACTER * ( DAT__SZLOC ) LOC0 ! Locator to HDS container file
      INTEGER IDCB               ! Index to data object entry in the DCB
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an index for the data object in the DCB and obtain a top-level
*  locator for its container file by re-opening the file.
      IDCB = ACB_IDCB( IACB )
      CALL HDS_OPEN( DCB_FILE( IDCB ), VMODE, LOC0, STATUS )

*  Also clone a locator for the data object itself from its DCB entry.
      CALL DAT_CLONE( DCB_LOC( IDCB ), LOC, STATUS )

*  Save these locators in the parameter system and link the object
*  locator with the parameter name. The subpar routines store the
*  supplied locators directly, rather than storing clones of the locators.
*  So we do not annul the LOC and LOC0 locators here.
      CALL SUBPAR_PUTFLOC( IPAR, LOC0, STATUS )
      CALL SUBPAR_PUTLOC( IPAR, LOC, STATUS )
      CALL HDS_LINK( LOC, PARAM, STATUS )

*  If the ADAM Parameter Block does not yet have a KeyMap to hold
*  parameter information, create one now.
      IF( APB_PARS .EQ. 0 ) THEN
         CALL AST_BEGINPM
         APB_PARS = AST_KEYMAP( ' ', STATUS )
         CALL AST_ENDPM
         CALL AST_EXEMPT( APB_PARS, STATUS )
      END IF

*  Add an entry for the supplied parameter, storing a value of zero to
*  indicate that NDF_CANCL should include it in its automatic parameter
*  cancellation.
      CALL AST_MAPPUT0I( APB_PARS, PARAM, 0, ' ', STATUS )

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PTLOC', STATUS )

      END
