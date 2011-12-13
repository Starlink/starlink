      SUBROUTINE sgs_CLSWK (IZONID, JSTAT)
*+
*  Name:
*     CLSWK

*  Purpose:
*     Close a workstation.

*  Language:
*     Starlink Fortran 77

*  Description:
*     If there are no other base zones on the workstation: delete the zone
*     and all other zones on the workstation; de-activate and close the
*     workstation. Otherwise just delete the base zone.  If the current
*     zone has been deleted the current zone ID is set to zero.
*
*     The specified zone must be a base zone.

*  Arguments:
*     IZONID = INTEGER (Given)
*         Zone identifier
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
*     07-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Constants From Sgscom:
*     MXZ       i       maximum number of zones allowed

*  Errors:
*     Invalid zone ID
*     Specified zone does not exist
*     Specified zone is not a base zone

*  Externals:
*     sgs_1HSTAT, sgs_1ERR, sgs_1CLWK, sgs_OTEXT, sgs_OPOLY

*  Read From Common:
*     IZTW      i()     zone table - workstation ID
*     NTEXT     i       length of current polyline
*     OTEXT     i       length of current text string
*     IWTID     i()     workstation table - GKS workstation ID

*  Written To Common:
*     IWTID     i()     workstation table - GKS workstation ID
*     IWTTY     i()     workstation table - Type
*     IWTCO     i()     workstation table - Connection ID
*     IWTCA     i()     workstation table - Category

*-

      IMPLICIT NONE

      INTEGER IZONID,JSTAT

      INCLUDE 'sgscom'

      INCLUDE 'SGS_ERR'


      INTEGER IWKID,LSTAT,IZONE,IGWKID,IWK

      CHARACTER RNAME*5
      PARAMETER (RNAME='CLSWK')



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      LSTAT = JSTAT
      JSTAT = 0

*  Validate zone number
      IF (IZONID .LT. 1 .OR. IZONID .GT. MXZ) THEN
         CALL sgs_1ERR(SGS__INVZN,RNAME,'Invalid zone ID',JSTAT)
         GO TO 9999
      END IF

*  Check workstation ID
      IF (IZTW(IZONID) .EQ. 0) THEN
         CALL sgs_1ERR(SGS__ZONNF,RNAME,'Specified zone does not exist',
     :                                                            JSTAT)
         GO TO 9999
      END IF

*  Check that it is a base zone
      IF (IZTW(IZONID) .GT. 0) THEN
         CALL sgs_1ERR(SGS__ZONNB,RNAME,
     :                       'Specified zone is not a base zone', JSTAT)
         GO TO 9999
      END IF

*  Flush any pending output
      IF (NTEXT.GT.0) CALL sgs_OTEXT
      IF (NPOLY.GT.1) CALL sgs_OPOLY

*  Save SGS workstation ID
      IWKID = -IZTW(IZONID)

*  Delete any zones connected to this workstation
      DO 20 IZONE=1,MXZ
         IF (IZTW(IZONE) .EQ. IWKID) THEN
            IZTW(IZONE) = 0

*        If we are deleting the current zone set the current zone to
*        zero
            IF (IZONE.EQ.ISZID) ISZID = 0
         END IF
   20 CONTINUE

*  Delete specified zone
      IZTW(IZONID) = 0

*  Save the GKS workstation ID
      IGWKID = IWTID(IWKID)

*  Reset SGS workstation table entry
      IWTID(IWKID) = 0
      IWTTY(IWKID) = 0
      IWTCO(IWKID) = 0
      IWTCA(IWKID) = 0

*  Check for other SGS workstations open on this GKS workstation
      DO 30 IWK = 1,MXWK
         IF (IWTID(IWK).EQ.IGWKID) GO TO 9999
   30 CONTINUE

*  Close the GKS workstation
      CALL SGS_1CLWK(IGWKID)

*    Exit
9999  CONTINUE

*    Reinstate incoming status
      IF (LSTAT .NE. 0) THEN
         JSTAT = LSTAT
      END IF

      END
