      SUBROUTINE sgs_RELZ (IZONID)
*+
*  Name:
*     RELZ

*  Purpose:
*     Release a zone that is no longer required.

*  Language:
*     Starlink Fortran 77

*  Description:
*     Neither a base zone nor the current zone can be released.

*  Arguments:
*     IZONID = INTEGER (Given)
*         Zone identifier

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

*  Constants From Sgscom:
*     MXZ          i      maximum number of zones allowed

*  Read From Common:
*     ISZID        i      current zone ID
*     IZTW         i()    zone table - workstation ID

*  Written To Common:
*     IZTW         i()    zone table - workstation ID

*-

      IMPLICIT NONE

      INTEGER IZONID
      INCLUDE 'sgscom'



*  Release the zone (unless it is the current zone, a base zone or junk)
      IF (IZONID.NE.ISZID .AND.
     :    IZONID.GT.0 .AND.
     :    IZONID.LE.MXZ .AND.
     :    IZTW(IZONID).GE.0) IZTW(IZONID)=0

      END
