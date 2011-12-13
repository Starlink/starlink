      LOGICAL FUNCTION CCD1_FILAS( NAME, STATUS )
*+
*  Name:
*     CCD1_FILAS

*  Purpose:
*     Checks that frame type may be associated with a FILTER.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CCD1_FILAS( NAME, STATUS )

*  Description:
*     The routine checks the string NAME against all those which may be
*     associated with a FILTER specification.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        Frame type to check against those which may be associated with
*        a FILTER.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     CCD1_FILAS = LOGICAL
*        True if NAME is a frame type which may be associated with a
*        FILTER.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-FEB-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK constants

*  Arguments Given:
      CHARACTER * ( * ) NAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CCD1__NMLEN ) CLIST( 5 ) ! List of frame types
                                 ! associated with FILTERS

*  Local Data:
      DATA CLIST / 'TARGET',
     :             'FLAT',
     :             'TWILIGHT_SKY',
     :             'NIGHT_SKY',
     :             'DOME' /

*.
      CCD1_FILAS = NAME .EQ. CLIST ( 1 ) .OR.
     :             NAME .EQ. CLIST ( 2 ) .OR.
     :             NAME .EQ. CLIST ( 3 ) .OR.
     :             NAME .EQ. CLIST ( 4 ) .OR.
     :             NAME .EQ. CLIST ( 5 )

      END
* $Id$
