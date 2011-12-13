      SUBROUTINE KPG1_QNCOL( NUMENT, STATUS )
*+
*  Name:
*     KPG1_QNCOL

*  Purpose:
*     Inquires the number of intensity-table entries of the current
*     graphics device.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_QNCOL( NUMENT, STATUS )

*  Description:
*     This routine determines how many "colour table" entries or
*     intensities the current GKS graphics device has.  This includes
*     devices that do not support colour.

*  Arguments:
*     NUMENT = INTEGER (Returned)
*        The number of entries in the intensity table.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  An SGS workstation must be open.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 February 12 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GKS_PAR'          ! GKS parameter definitions

*  Arguments Returned:
      INTEGER NUMENT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CONID              ! Connection identifier
      INTEGER GSTAT              ! Graphics status
      INTEGER MFA                ! Max.no. of fill-area bundle table
                                 ! entries
      INTEGER MPI                ! Max. no. of pattern indices
      INTEGER MPL                ! Max. no. of polyline bundle table
                                 ! entries
      INTEGER MPM                ! Max. no. of polymarker bundle table entries
      INTEGER MTX                ! Max. no. of text bundle table entries
      INTEGER WKID               ! GKS workstation identifier
      INTEGER WSTYPE             ! Workstation type

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Inquire the workstation identifier for GKS inquiries.
      CALL SGS_ICURW( WKID )

*  Get the workstation type.
      CALL GQWKC( WKID, GSTAT, CONID, WSTYPE )

*  Inquire whether GKS/SGS has reported an error.
      CALL GKS_GSTAT( STATUS )

*  Initialise the returned value.
      NUMENT = 1

*  Inquire the number of greyscale intensities that are available on
*  the specified device.
      CALL GQLWK( WSTYPE, GSTAT, MPL, MPM, MTX, MFA, MPI, NUMENT )

*  Inquire whether GKS has reported an error.
      CALL GKS_GSTAT( STATUS )

      END
