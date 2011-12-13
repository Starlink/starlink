      SUBROUTINE sgs_SW (X1, X2, Y1, Y2, JSTAT)
*+
*  Name:
*     SW

*  Purpose:
*     Set window for current zone.

*  Language:
*     Starlink Fortran 77

*  Description:
*     The smaller of X1 and X2, and of Y1 and Y2, are the coordinates of
*     the bottom left-hand corner of the resulting window.  Both extents
*     must be greater than zero.

*  Arguments:
*     X1 = REAL (Given)
*         Window limit x
*     X2 = REAL (Given)
*         "     "   x
*     Y1 = REAL (Given)
*         "     "   y
*     Y2 = REAL (Given)
*         "     "   y
*     JSTAT = INTEGER (Given & Returned)
*         Inherited status (if option selected)
*         Status (0=OK) (if not inherited)

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
*     sgs_1HSTAT, sgs_1BNORM, sgs_1GKERR, sgs_OPOLY, sgs_OTEXT, GSWN

*  Read From Common:
*     ISZID     i       current zone id
*     NPOLY     i       length of current polyline
*     NTEXT     i       length of current text

*  Written To Common:
*     ZTW       r()     zone table - window

*-

      IMPLICIT NONE

      REAL X1,X2,Y1,Y2
      INTEGER JSTAT

      REAL X1N,X2N,Y1N,Y2N
      CHARACTER RNAME*2
      PARAMETER (RNAME='SW')

      INCLUDE 'sgscom'

      INCLUDE 'SGS_ERR'




*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Finish any plotting
      IF (NPOLY.GT.1) CALL sgs_OPOLY
      IF (NTEXT.GT.0) CALL sgs_OTEXT

*  Normalise the window
      CALL sgs_1BNORM(X1,X2,Y1,Y2,X1N,X2N,Y1N,Y2N,JSTAT)
      IF (JSTAT.NE.0) THEN
         CALL SGS_1ERR(SGS__FLSWN, RNAME,'Failed to set window',JSTAT)
         GO TO 9999
      END IF

*  Update zone table
      ZTW(1,ISZID)=X1N
      ZTW(2,ISZID)=X2N
      ZTW(3,ISZID)=Y1N
      ZTW(4,ISZID)=Y2N

*  Set window
      CALL GSWN(1,X1N,X2N,Y1N,Y2N)
      CALL sgs_1GKERR(RNAME,JSTAT)

*  Exit
 9999 CONTINUE

      END

