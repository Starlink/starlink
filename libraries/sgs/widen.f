      SUBROUTINE sgs_WIDEN (CIDENT,WS,CONID,JSTAT)
*+
*  Name:
*     WIDEN

*  Purpose:
*     Convert a character string workstation description to an integer
*     workstation sequence number and connection identifier by calling
*     the GNS library.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     CIDENT = CHAR (Given)
*         The character workstation identifier
*     WS = INTEGER (Returned)
*         Workstation sequence number (if JSTAT=0)
*     CONID = INTEGER (Returned)
*         Connection ID (if JSTAT=0)
*     JSTAT = INTEGER (Returned)
*         Status  = 0 if success

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
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
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     gns_TNG

*-

      IMPLICIT NONE

      CHARACTER CIDENT*(*)
      INTEGER WS,CONID,JSTAT

      CHARACTER*5 RNAME
      PARAMETER(RNAME='WIDEN')



      JSTAT = 0

      CALL gns_TNG( CIDENT, WS, CONID, JSTAT)

      END
