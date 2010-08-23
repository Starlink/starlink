      SUBROUTINE KPG1_NDFNM( INDF, NAME, NMLEN, STATUS )
*+
*  Name:
*     KPG1_NDFNM

*  Purpose:
*     Returns the name of an NDF without a directory path (Unix only).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_NDFNM( INDF, NAME, NMLEN, STATUS )

*  Description:
*     Gets the full path to the supplied NDF, then removes any directory
*     path from the start, and return the resulting string.
*
*     Note, Unix file names are assumed.

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF.
*     NAME = CHARACTER * ( * ) (Returned)
*        The NDF name without directory path.
*     NMLEN = INTEGER (Returned)
*        The used length of NAME.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 CLRC
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-AUG-1999 (DSB):
*        Original version.
*     27-DEC-2005 (TIMJ):
*        Call CHR_LASTO rather than NDG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      CHARACTER NAME*(*)
      INTEGER NMLEN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER PATH*255         ! Full path for NDF
      INTEGER PLEN               ! Used length of PATH
      INTEGER DIREND             ! Index of final "/" character
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the full path to the NDF.
      CALL NDF_MSG( 'NDF', INDF )
      CALL MSG_LOAD( ' ', '^NDF', PATH, PLEN, STATUS )

*  Find the last "/" marking the end of the directory path.
      CALL CHR_LASTO( PATH( : PLEN ), '/', DIREND )
      DIREND = DIREND + 1

*  Copy the NDF name to the returned string.
      NAME = PATH( DIREND:PLEN )

*  Return the length of the string.
      NMLEN =  PLEN - DIREND + 1

      END
