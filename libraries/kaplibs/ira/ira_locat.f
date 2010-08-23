      SUBROUTINE IRA_LOCAT( XNAME, ASNAME, STATUS )
*+
*  Name:
*     IRA_LOCAT

*  Purpose:
*     Sets the location for new IRA astrometry structures.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_LOCAT( XNAME, ASNAME, STATUS )

*  Description:
*     By default, IRA_CREAT and IRA_EXPRT store astrometry information
*     in a component named "ASTROMETRY" within the "IRAS" NDF
*     extension.  These names may be changed if necessary by calling
*     this routine. The supplied arguments give the name of the NDF
*     extension and the component name to be used by all future calls
*     to IRA_CREAT or IRA_EXPRT. It should be ensured that the
*     extension exists before calling IRA_CREAT or IRA_EXPRT.

*  Arguments:
*     XNAME = CHARACTER * ( * ) (Given)
*        The name of NDF extension in which astrometry structures are to
*        be created. If a blank value is supplied the current value is
*        left unchanged.
*     ASNAME = CHARACTER * ( * ) (Given)
*        The name of a component of the NDF extension in which to store
*        astrometry information. If a blank value is supplied the
*        current value is left unchanged.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-JAN-1991 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified for IRA version 2.
*     12-FEB-1993 (DSB):
*        Description re-written to take account of the fact that this
*        routine now only effects newly created astrometry structures.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_XNAME = CHARACTER (Read and Write)
*           The NDF extension name.
*        ACM_ASNAME = CHARACTER (Read and Write)
*           The Astrometry Structure name.

*  Arguments Given:
      CHARACTER XNAME*(*)
      CHARACTER ASNAME*(*)

*  Status:
      INTEGER STATUS                ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the given NDF extension name is not blank, change the current
*  value.
      IF( XNAME .NE. ' ' ) ACM_XNAME = XNAME

*  If the given AS name is not blank, change the current
*  value.
      IF( ASNAME .NE. ' ' ) ACM_ASNAME = ASNAME

*  Tidy the new values.
      CALL CHR_UCASE( ACM_XNAME )
      CALL CHR_LDBLK( ACM_XNAME )
      CALL CHR_UCASE( ACM_ASNAME )
      CALL CHR_LDBLK( ACM_ASNAME )

      END
