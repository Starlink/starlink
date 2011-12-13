      SUBROUTINE sgs_FLUSH
*+
*  Name:
*     FLUSH

*  Purpose:
*     Flush any pending output on all active workstations.

*  Language:
*     Starlink Fortran 77

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
*     GWSAC      i       at least one workstation active
*     GSGOP      i       segment open
*     GPOSTP     i       regeneration postponed

*  Errors:
*     Error returned by GKS inquiry

*  Externals:
*     sgs_OTEXT, sgs_OPOLY, sgs_1ERR, GQOPS, GQACWK, GUWK

*-

      IMPLICIT NONE

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'


      INTEGER IERR,N,NACT,IWKID,JSTAT,IOPSTA
      CHARACTER*5 RNAME
      PARAMETER (RNAME='FLUSH')



*  Flush the text & polyline buffers
      CALL sgs_OTEXT
      CALL sgs_OPOLY

*  See if any workstations are active
      CALL GQOPS(IOPSTA)
      IF (IOPSTA.EQ.GWSAC.OR.IOPSTA.EQ.GSGOP) THEN

*     Find how many active workstations there are
         N = 1
         CALL GQACWK(N,IERR,NACT,IWKID)
         IF (IERR.NE.0) THEN
            CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQACWK',
     :      JSTAT)
            GO TO 9999
         END IF

*     Update all active workstations
         DO 10 N = 1,NACT
            CALL GQACWK(N,IERR,NACT,IWKID)
            IF (IERR.EQ.0) THEN
               CALL GUWK(IWKID,GPOSTP)
            ELSE
               CALL sgs_1ERR(SGS__INQER,RNAME,
     :                                'Error returned by GQACWK', JSTAT)
            END IF
   10    CONTINUE
      END IF

 9999 CONTINUE

      END
