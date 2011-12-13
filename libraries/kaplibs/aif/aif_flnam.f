      SUBROUTINE AIF_FLNAM( PARNAM, FILNAM, STATUS )
*+
*  Name:
*     AIF_FLNAM

*  Purpose:
*     Returns the name of a file as a character string given its
*     parameter name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL AIF_FLNAM( PARNAM, FILNAM, STATUS )

*  Description:
*     Normally, the handles to HDS data files are locators and files
*     are obtained via the parameter system.  The name of a data
*     file itself is not available to applications via the user-level
*     parameter-system library, and so applications cannot place the
*     file name in log files or use the it to generate compound names.
*     This routine provides that functionality by calling internal
*     parameter-system routines to obtain the name.  This routine also
*     works for Fortran files associated via an ADAM parameter.

*  Arguments:
*     PARNAM = CHARACTER * ( * ) (Given)
*        Parameter name associated with the data object whose name is
*        required.
*     FILNAM = CHARACTER * ( * ) (Returned)
*        The name of the file.  If %FILNAM is not long enough a bad
*        status is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This cannot be used to obtain the names of objects within
*        an HDS file.

*  Algorithm:
*     -  Get the ADAM internal code that refers to the piece of
*        parameter space associated with the input parameter.
*     -  Get the file name associated with the ADAM internal pointer.

*  Prior Requirements:
*     -  Must have obtained the file via the input parameter.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     1990 Feb 20 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * )
     :  PARNAM                   ! Parameter name of the file whose
                                 ! name is required

*  Arguments Returned:
      CHARACTER * ( * )
     :  FILNAM                   ! File name.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  IMCODE                   ! ADAM internal parameter pointer.
*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the ADAM internal code that refers to the piece of
*    parameter space associated with the input parameter PARNAM.

      CALL SUBPAR_FINDPAR( PARNAM, IMCODE, STATUS )

*    Get the file name associated with the ADAM internal pointer.

      CALL SUBPAR_GETNAME( IMCODE, FILNAM, STATUS )

      END
