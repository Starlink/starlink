      SUBROUTINE IMG_INDF( PARAM, INDF, STATUS )
*+
*  Name:
*     IMG_INDF

*  Purpose:
*     Obtains an NDF identifier for an image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_INDF( PARAM, INDF, STATUS )

*  Description:
*     This routine obtains an NDF identifier for an image. The
*     resulting identifier may be passed to routines from the NDF
*     library (see SUN/33) to perform lower-level operations on the
*     data which cannot be done using IMG routines.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name (case insensitive).
*     INDF = INTEGER (Returned)
*        NDF identifier for the image.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The NDF identifier returned by this routine should be annulled
*     (e.g. using NDF_ANNUL) when it is no longer required.  The IMG
*     system will not perform this operation itself.
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOID will be returned for the INDF argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.  The
*     NDF__NOID constant is defined in the NDF_PAR include file.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     18-FEB-1992 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Global Variables:
      INCLUDE 'IMG_PCB'          ! IMG_ Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read)
*           NDF identifier.

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) VPAR ! Validated parameter name
      INTEGER SLOT               ! PCB slot number
      LOGICAL WASNEW             ! New slot? (junk argument)

*.

*  Set an initial null value for the INDF argument.
      INDF = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the parameter name and find the PCB slot associated with
*  it.
      CALL IMG1_VPAR( PARAM, VPAR, STATUS )
      CALL IMG1_GTSLT( VPAR, .FALSE., SLOT, WASNEW, STATUS )

*  If successful, clone the NDF identifier.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL NDF_CLONE( PCB_INDF( SLOT ), INDF, STATUS )
      END IF

*  If an error occurred, report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IMG_INDF_ERR',
     :                 'IMG_INDF: Error obtaining an NDF identifier ' //
     :                 'for an image.', STATUS )
      END IF

      END
* $Id$
