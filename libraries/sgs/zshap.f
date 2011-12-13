      SUBROUTINE sgs_ZSHAP (AR, POS, IZONID, JSTAT)
*+
*  Name:
*     ZSHAP

*  Purpose:
*     Create and select a zone of the specified shape.

*  Language:
*     Starlink Fortran 77

*  Description:
*     The new zone has a window whose world coordinate extent is (0,0) to
*     (X,Y), where X/Y is the true aspect ratio of the viewport and the
*     smaller of X or Y is unity.

*  Arguments:
*     AR = REAL (Given)
*         Aspect ratio: viewport X/Y
*     POS = CHAR (Given)
*         Position code (see below)
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
*     sgs_1HSTAT, sgs_1NORM, sgs_1PNZ

*  Read From Common:
*     ISZID      i      current zone ID
*     ZTV        i()    zone table - viewport

*-

      IMPLICIT NONE

      INCLUDE 'sgscom'


      REAL AR
      CHARACTER POS*(*)
      INTEGER IZONID,JSTAT

      REAL ASPR,X1V,Y1V,X2V,Y2V,XV,YV,P,Q,XVN,YVN



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Normalise aspect ratio
      ASPR=MAX(ABS(AR),1E-6)

*  Current viewport
      X1V=ZTV(1,ISZID)
      X2V=ZTV(2,ISZID)
      Y1V=ZTV(3,ISZID)
      Y2V=ZTV(4,ISZID)
      XV=X2V-X1V
      YV=Y2V-Y1V

*  NDC size of new viewport
      CALL sgs_1NORM(XV,ASPR*YV,P,Q)
      XVN=XV*Q
      YVN=YV*P

*  Set up new zone
      CALL sgs_1PNZ(X1V,Y1V,XV,YV,XVN,YVN,POS,IZONID,JSTAT)

*  Exit
 9999 CONTINUE

      END
