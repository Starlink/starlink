      LOGICAL FUNCTION gns_FILTG(ITYPE)
*+
*  Name:
*     GNS_FILTG

*  Purpose:
*     Filter list of available workstations

*  Language:
*     Starlink Fortran 77

*  Description:
*     Filters the list of available workstations rejecting those which are
*     not supported by GKS (unless GKS is closed in which case all
*     workstations are passed).

*  Arguments:
*     ITYPE = INTEGER (Given)
*         Workstation type

*  Returned Value:
*     gns_FILTG = LOGICAL (Returned)
*         Include in list

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Constants From GKS_PAR:
*     GGKCL   i    GKS closed

*  Externals:
*     GQWKCA

*-
      IMPLICIT NONE

      INCLUDE 'GKS_PAR'

      INTEGER IERR, ITYPE, ICAT, ISTATE

*  Check for GKS being open
      CALL GQOPS(ISTATE)
      IF (ISTATE.EQ.GGKCL) THEN
         gns_FILTG = .TRUE.
      ELSE

*  Make an inquiry about the workstation type and assume that an error
*  means that it doesn't exist.
         CALL GQWKCA(ITYPE,IERR,ICAT)
         IF (IERR.EQ.0) THEN
            gns_FILTG = .TRUE.
         ELSE
            gns_FILTG = .FALSE.
         END IF
      END IF
      END
