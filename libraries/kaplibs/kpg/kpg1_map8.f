      SUBROUTINE KPG1_MAP8( INDF, COMP, TYPE, MMOD, PNTR, EL, STATUS )
*+
*  Name:
*     KPG1_MAP8

*  Purpose:
*     Obtains mapped access to an array component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_MAP8( INDF, COMP, TYPE, MMOD, PNTR, EL, STATUS )

*  Description:
*     This routine is a wrapper for NDF_MAP8 which obtains mapped access
*     to an array component of an NDF, returning a pointer to the mapped
*     values and a count of the number of elements mapped. At the moment
*     this wrapper routine is a dummy which does nothing else.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component to be mapped: 'DATA',
*        'QUALITY' or 'VARIANCE' (or 'ERROR').
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric type to be used for access (e.g. '_REAL').
*     MMOD = CHARACTER * ( * ) (Given)
*        Mapping mode for access to the array: 'READ', 'UPDATE' or
*        'WRITE', with an optional initialisation mode '/BAD' or
*        '/ZERO' appended.
*     PNTR( * ) = INTEGER (Returned)
*        Pointer(s) to the mapped values (see the Notes section).
*     EL = INTEGER*8 (Returned)
*        Number of elements mapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     5-DEC-2019 (DSB):
*        Original version.
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
      CHARACTER * ( * ) COMP
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) MMOD

*  Arguments Returned:
      INTEGER PNTR( * )
      INTEGER*8 EL

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Call NDF_MAP8 to map the array.
      CALL NDF_MAP8( INDF, COMP, TYPE, MMOD, PNTR, EL, STATUS )

      END
