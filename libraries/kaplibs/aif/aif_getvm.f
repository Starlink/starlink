      SUBROUTINE AIF_GETVM( TYPE, NDIM, DIMS, PNTR, WKLOC, STATUS )
*+
*  Name:
*     AIF_GETVM

*  Purpose:
*     Obtains a pointer and a locator to mapped HDS workspace.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL AIF_GETVM( TYPE, NDIM, DIMS, PNTR, WKLOC, STATUS )

*  Description:
*     This routine obtains and file maps a temporary array of a given
*     data type and dimensions via HDS.  A pointer to the mapped work
*     array is returned, as is a locator so that the temporary array
*     may be annulled when no longer required.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The HDS data type of the temporary array.
*     NDIM = INTEGER (Given)
*        The number of dimensions of the work array.
*     DIMS( NDIM ) = INTEGER (Given)
*        The dimensions of the temporary array.
*     PNTR = INTEGER (Returned)
*        The pointer to the mapped temporary array.
*     WKLOC = CHARACTER * ( DAT__SZLOC )(Returned)
*        The HDS locator to the temporary array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The current HDS tuning parameter MAP is stored, so that the
*     temporary array may be accessed via file mapping, and upon exit
*     restored to its former value.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 February 22 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Arguments Given:
      CHARACTER * ( * )
     :  TYPE

      INTEGER
     :  NDIM,
     :  DIMS( NDIM )

*  Arguments Returned:
      INTEGER
     :  PNTR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  OLDMAP                   ! The value of the HDS MAP tuning
                                 ! parameter on entry to this routine

      CHARACTER * ( DAT__SZLOC )
     :  WKLOC                    ! LOcator to the workspace
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Inquire the current HDS MAP flag, since workspace requires that
*  the data are written by file mapping.
      CALL HDS_GTUNE( 'MAP', OLDMAP, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Set the tuning parameter to the file-mapping setting.
         CALL HDS_TUNE( 'MAP', 1, STATUS )

*  Obtain the workspace.
         CALL AIF_TEMP( TYPE, NDIM, DIMS, WKLOC, STATUS )

*  Map the workspace.
         CALL DAT_MAP( WKLOC, TYPE, 'WRITE', NDIM, DIMS, PNTR, STATUS )

*  Return to the old tuning setting being careful to ensure that this
*  is successful, even if there has been an error.
         CALL ERR_BEGIN( STATUS )
         CALL HDS_TUNE( 'MAP', OLDMAP, STATUS )
         CALL ERR_END( STATUS )
      END IF

      END
