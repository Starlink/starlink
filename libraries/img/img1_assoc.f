      SUBROUTINE IMG1_ASSOC( PARAM, MODE, SLOT, STATUS )
*+
*  Name:
*     IMG1_ASSOC

*  Purpose:
*     Obtain access to an input NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_ASSOC( PARAM, MODE, SLOT, STATUS )

*  Description:
*     This routine obtains access to an input NDF via the parameter
*     PARAM. The information associated with the NDF is stored in the
*     PCB tables at the SLOT position. No manipulations or checking of
*     the NDF is performed, however, the PCB tables are initialised to
*     show that the data array has not been mapped in. If an NDF is not
*     accessed successfully then the PCB entry associated with this SLOT
*     is released.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name.
*     MODE = CHARACTER * ( * ) (Given)
*        Mode of access for the NDF. One of UPDATE or READ.
*     SLOT = INTEGER (Given)
*        The slot number allocated to the NDF to be accessed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-NOV-1994 (PDRAPER):
*        Original version.
*     28-NOV-1994 (PDRAPER):
*        Now uses IMG1_ACNDF to check access mode of NDF.
*     29-NOV-1995 (PDRAPER):
*        Now uses NDF_ASSOC and has an argument to specify the access mode.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ private constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Global Variables:
      INCLUDE 'IMG_PCB'          ! IMG_ Parameter Control Block
*        PCB_INDF( IMG__MXPCB ) = INTEGER (Write)
*           NDF identifier.
*        PCB_PNTR( IMG__MXPCB ) = INTEGER (Write)
*           Pointer to mapped data.

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) MODE
      INTEGER SLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER INDF               ! NDF identifier

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access an NDF.
      CALL NDF_ASSOC( PARAM, MODE, INDF, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If the NDF was acquired successfully, then record the ID in the PCB
*  tables. Also make sure its data array pointer is initialised to
*  IMG__NOPTR so that later routines know this hasn't been mapped yet.
         PCB_INDF( SLOT ) = INDF
         PCB_PNTR( SLOT ) = IMG__NOPTR

*  If an error occurred free the slot.
      ELSE
         CALL IMG1_FRSLT( SLOT, .TRUE., STATUS )
      END IF
      END

*  $Id$
