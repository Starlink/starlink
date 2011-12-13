      SUBROUTINE sgs_WNAME (ACTROU, INTARG, JSTAT)
*+
*  Name:
*     WNAME

*  Purpose:
*     Call the action routine once for every workstation name found,
*     passing to it the workstation name and its associated description.

*  Language:
*     Starlink Fortran 77

*  Description:
*     GKS is opened with unit 6 as the error channel if not already open.
*     (See sgs_OPEN for a discussion of the choice of error channel.)

*  Arguments:
*     ACTROU = SUBROUTINE (Given)
*         Action routine
*     INTARG = INTEGER (Given)
*         Argument passed to action routine
*     JSTAT = INTEGER (Returned)
*         Status = 0 if success

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
*     07-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     gns_GWNG, GQOPS, GOPKS

*-

      IMPLICIT NONE

      EXTERNAL ACTROU
      INTEGER INTARG,JSTAT

      INCLUDE 'GKS_PAR'


      LOGICAL gns_FILTG
      EXTERNAL gns_FILTG
      INTEGER ICNTX, LDESCR, IOPSTA
      CHARACTER*15 WSNAME
      CHARACTER*72 DESCR
      CHARACTER*5 RNAME
      PARAMETER (RNAME='WNAME')



*  Initialize context variable
      ICNTX = 0
      JSTAT = 0

*  Open GKS (unless already open)
      CALL GQOPS(IOPSTA)
      IF (IOPSTA.EQ.GGKCL) CALL GOPKS(6,-1)

  100    CONTINUE
         CALL gns_GWNG(gns_FILTG, ICNTX, WSNAME, DESCR, LDESCR, JSTAT)
         IF (JSTAT.NE.0) GO TO 9999

         IF (ICNTX.NE.0) THEN
            CALL ACTROU(WSNAME, DESCR(:LDESCR), INTARG, JSTAT)

*        Go back for next name
            GO TO 100
         END IF

 9999 CONTINUE

      END
