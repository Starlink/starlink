      SUBROUTINE sgs_1GKSIN (LUN, JSTAT)
*+
*  Name:
*     GKSIN

*  Purpose:
*     Initialise GKS.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Description:
*     If GKS is already open, it is left open.

*  Arguments:
*     LUN = INTEGER (Given)
*         Logical unit for error messages
*     JSTAT = INTEGER (Given & Returned)
*         Inherited status (if option selected)
*         Status: 0=OK

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

*  Constants From GKS_PAR:
*     GGKCL    i      operating state - closed

*  Externals:
*     sgs_1HSTAT, sgs_1GKERR, GQOPS, GOPKS

*-

      IMPLICIT NONE

      INTEGER LUN,JSTAT

      INCLUDE 'GKS_PAR'


      INTEGER IOPSTA
      CHARACTER*5 RNAME
      PARAMETER (RNAME='GKSIN')



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Open GKS (unless already open)
      CALL GQOPS(IOPSTA)
      IF (IOPSTA.EQ.GGKCL) CALL GOPKS(LUN,-1)

*  Check for GKS errors
      CALL sgs_1GKERR(RNAME,JSTAT)

*  Exit
 9999 CONTINUE

      END
