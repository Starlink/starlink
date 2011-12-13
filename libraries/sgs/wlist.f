      SUBROUTINE sgs_WLIST (LU)
*+
*  Name:
*     WLIST

*  Purpose:
*     List all the SGS workstation names.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     LU = INTEGER (Given)
*         Fortran I/O stream number to which the
*         list will be output

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
*     sgs_WNAME, sgs_1WLST, gns_START

*-

      IMPLICIT NONE

      INTEGER LU

      EXTERNAL sgs_1WLST
      INTEGER JSTAT
      CHARACTER*5 RNAME
      PARAMETER (RNAME='WLIST')



*  Check that the workstation name database is accessible
      JSTAT = 0
      CALL gns_START('GKS',JSTAT)

      IF (JSTAT.EQ.0) THEN
         CALL sgs_WNAME(sgs_1WLST,LU,JSTAT)
         WRITE (LU,'(1X)')
      END IF

      END
