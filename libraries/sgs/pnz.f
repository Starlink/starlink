      SUBROUTINE sgs_1PNZ (X0,Y0, XV,YV, XVN,YVN, POS, IZONID, JSTAT)
*+
*  Name:
*     PNZ

*  Purpose:
*     Create a new zone of given size and position.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Arguments:
*     X0 = REAL (Given)
*         Current zone viewport corner (x)
*     Y0 = REAL (Given)
*         "     "       "      "    (y)
*     XV = REAL (Given)
*         Current zone viewport size (x)
*     YV = REAL (Given)
*         "     "       "     "   (y)
*     XVN = REAL (Given)
*         New zone viewport size (x)
*     YVN = REAL (Given)
*         "    "     "       "  (y)
*     POS = CHAR*2 (Given)
*         Position code
*     IZONID = INTEGER (Returned)
*         Zone identifier for new zone
*     JSTAT = INTEGER (Given & Returned)
*         Inherited status (if option selected)
*         Status (0=OK) (if non-inherited)

*  Notes:
*     Position code;  the first two characters determine the position of
*     the new zone within the current zone as follows:
*
*     1st character = B (bottom), C (centre) or T (top).
*     2nd character = L (left),   C (centre) or R (right).
*
*     The new zone can be positioned in one corner of the current zone
*     by specifiying 'BL', 'BR', 'TL' or 'TR'.  The new zone can be
*     positioned centrally against one edge via 'BC', 'CR', 'TC' or 'CL'.
*     'CC' causes the new zone to be concentric with the current one.
*
*     The new zone has a window whose world coordinate extent is (0,0) to
*     (X,Y), where X/Y is the true aspect ratio of the viewport and the
*     smaller of X or Y is unity.

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

*  Errors:
*     Too many zones

*  Externals:
*     sgs_1HSTAT, sgs_1GETZ, sgs_1ERR, sgs_1UPCAS, sgs_1NEWZ

*  Read From Common:
*     ISZID      i      current zone ID
*     IZTW       i()    zone table - SGS workstation ID
*     ZVT        r()    zone table - viewport

*-

      IMPLICIT NONE

      REAL X0,Y0,XV,YV,XVN,YVN
      CHARACTER*(*) POS
      INTEGER IZONID,JSTAT

      INCLUDE 'sgscom'

      INCLUDE 'SGS_ERR'


      REAL DX,DY,X,Y,X1,Y1
      CHARACTER LPOS*2

      CHARACTER RNAME*3
      PARAMETER (RNAME='PNZ')



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Allocate a zone table entry
      CALL sgs_1GETZ(ABS(IZTW(ISZID)),IZONID)
      IF (IZONID.EQ.0) THEN
         CALL sgs_1ERR(SGS__ZNZEX,RNAME,'Too many zones',JSTAT)
         GO TO 9999
      END IF

*  Convert alignment string to uppercase
      CALL sgs_1UPCAS(POS(:2),LPOS)

*  Viewport alignment: horizontal
      DX=XV-XVN
      IF (LPOS(2:2).EQ.'R') THEN
         X=X0+DX
      ELSE IF (LPOS(2:2).EQ.'C') THEN
         X=X0+DX/2.0
      ELSE
         X=X0
      END IF

*  Viewport alignment: vertical
      DY=YV-YVN
      IF (LPOS(:1).EQ.'T') THEN
         Y=Y0+DY
      ELSE IF (LPOS(:1).EQ.'C') THEN
         Y=Y0+DY/2.0
      ELSE
         Y=Y0
      END IF

*  Check that new bounds lie within the current zone
      X = MAX(X,ZTV(1,ISZID))
      Y = MAX(Y,ZTV(3,ISZID))
      X1 = MIN(X+XVN,ZTV(2,ISZID))
      Y1 = MIN(Y+YVN,ZTV(4,ISZID))

*  Set up zone with appropriate viewport
      CALL sgs_1NEWZ(IZONID,X,X1,Y,Y1)

*  Exit
 9999 CONTINUE

      END
