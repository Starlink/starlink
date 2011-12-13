      SUBROUTINE sgs_1WLST (NAME, COMMNT, LU, JSTAT)
*+
*  Name:
*     WLST

*  Purpose:
*     Internal routine which lists one SGS workstation name for the
*     sgs_WLIST routine.  It is called by the sgs_WNAME routine once per
*     workstation name.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Arguments:
*     NAME = CHAR (Given)
*         SGS workstation name
*     COMMNT = CHAR (Given)
*         Description
*     LU = INTEGER (Given)
*         Fortran I/O unit for output
*     JSTAT = INTEGER (Returned)
*         Status:  0 = OK

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

*-

      IMPLICIT NONE

      CHARACTER*(*) NAME, COMMNT
      INTEGER LU,JSTAT

      INTEGER LW,NL,NLBIG,NLHUGE
      PARAMETER (NLBIG=15,NLHUGE=72)
      CHARACTER WNAME*(NLHUGE),WDESC*(NLHUGE-NLBIG-4)
      CHARACTER SPACES*(17)
      PARAMETER (SPACES=' ')



*  Copy SGS workstation name and description
      WNAME=NAME
      NL=LEN(NAME)
      WDESC=COMMNT
      LW = LEN(COMMNT)

*  Examine length of name
      IF (NL.LE.NLBIG) THEN

*     Report a name of normal length
         WRITE (LU,'(3X,A,2X,A)',ERR=99) WNAME(:NLBIG),WDESC(:LW)
      ELSE

*     Flag a name of extreme length
         IF (NL.GT.NLHUGE) WNAME(NLHUGE-6:)='......'

*     Report a longer than normal name
         WRITE (LU,'(3X,A/3X,2A)',ERR=99) WNAME,SPACES,WDESC(:LW)
      END IF

*  Set status to indicate success
      JSTAT=0
      GO TO 999

*  Set status to indicate I/O error
 99   CONTINUE
      JSTAT=-1

 999  CONTINUE

      END
