      SUBROUTINE sgs_MARKL (MTYPE)
*+
*  Name:
*     MARKL

*  Purpose:
*     Draw marker at current end of polyline.

*  Language:
*     Starlink Fortran 77

*  Description:
*     The marker type is set to MARKL. The marker type ASF is assumed to
*     be set to GINDIV.
*
*     If no polyline has yet been begun no marker is drawn.

*  Arguments:
*     MARKL = INTEGER (Given)
*         Marker type

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
*     sgs_OPOLY, GSMK

*  Read From Common:
*     NPOLY      i      length of current polyline
*     XPOLY      i()    current polyline (X)
*     YPOLY      i()       "       "     (Y)

*-

      IMPLICIT NONE

      INTEGER MTYPE

      INCLUDE 'sgscom'



*  Make sure there's a polyline
      IF (NPOLY.GT.0) THEN

*     Flush it
         CALL sgs_OPOLY

*     Set the marker type
         CALL GSMK(MTYPE)

*     Draw the marker
         CALL GPM(1,XPOLY,YPOLY)
      END IF

      END
