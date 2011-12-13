      SUBROUTINE sgs_CUVIS (ON)
*+
*  Name:
*     CUVIS

*  Purpose:
*     Set the visibility of the cursor on the current SGS device.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     ON = LOGICAL (Given)
*         The desired visibilty

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

*  Constants From GKS_PAR:
*     GECHO    i     echo on
*     GNECHO   i     echo off

*  Externals:
*     sgs_1ILCMO, GSLCM

*  Read From Common:
*     IZTW     i()   zone table - SGS workstation ID
*     IWTID    i()   workstation table - GKS workstation ID
*     ISZID    i     current zone ID

*-

      IMPLICIT NONE

      LOGICAL ON

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'


      INTEGER MODE,IESW,JSTAT



*   Inquire current locator mode
      CALL sgs_1ILCMO(MODE,IESW,JSTAT)
      IF (JSTAT.NE.0) GO TO 999

*   Set echo switch
      IF (ON) THEN
         IESW = GECHO
      ELSE
         IESW = GNECHO
      END IF
      CALL GSLCM(IWTID(ABS(IZTW(ISZID))),1,MODE,IESW)

  999 CONTINUE

      END
