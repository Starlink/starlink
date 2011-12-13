      SUBROUTINE sgs_1GETZ (IWKID, IZONID)
*+
*  Name:
*     GETZ

*  Purpose:
*     Allocate a zone table entry.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Description:
*     A negative IWKID is specified when the base zone for a workstation
*     is being created.
*
*     IZONID = 0 indicates either that the workstation ID is illegal
*     (i.e. zero) or that all zone table entries are in use.

*  Arguments:
*     IWKID = INTEGER (Given)
*         Workstation ID or its complement
*     IZONID = INTEGER (Returned)
*         Zone ID  (0=failure)

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
*     MXZ       i     maximum number of zones allowed

*  Read From Common:
*     IZTW      i()   zone table - workstation ID

*  Written To Common:
*     IZTW      i()   zone table - workstation ID

*-

      IMPLICIT NONE

      INCLUDE 'sgscom'


      INTEGER IWKID,IZONID

      INTEGER NZONE



*  Preset zone ID to failure
      IZONID=0

*  Validate IWKID
      IF (IWKID.EQ.0) GO TO 9999

*  Search for an unused zone table entry
      DO 10 NZONE=1,MXZ
         IF (IZTW(NZONE).EQ.0) THEN
            IZONID=NZONE
            IZTW(NZONE)=IWKID
            GO TO 9999
         END IF
   10 CONTINUE

*  Exit
 9999 CONTINUE

      END
