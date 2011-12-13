      SUBROUTINE sgs_ITXA (IF, IPR, HT, AR, XU,YU, SP, TXJ)
*+
*  Name:
*     ITXA

*  Purpose:
*     Inquire text attributes.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     IF = INTEGER (Returned)
*         Text font number
*     IPR = INTEGER (Returned)
*         Text precision
*     HT = REAL (Returned)
*         Text height
*     AR = REAL (Returned)
*         Text aspect ratio (H/W)
*     XU = REAL (Returned)
*         Text orientation direction cosine (x)
*     YU = REAL (Returned)
*         "      "           "       "    (y)
*     SP = REAL (Returned)
*         Text spacing
*     TXJ = CHAR (Returned)
*         Text justification code

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

*  Read From Common:
*     IFONT       i     text font number
*     IPREC       i     text precision
*     HTX         r     text height
*     ARTX        r     text aspect ratio (H/W)
*     XUPTX       r     text orientation direction cosine (x)
*     YUPTX       r       "       "          "        "   (y)
*     STX         r     text spacing
*     CTXJ        c     text justification code

*-

      IMPLICIT NONE

      INTEGER IF,IPR
      REAL HT,AR,XU,YU,SP
      CHARACTER*2 TXJ

      INCLUDE 'sgscom'




      IF = IFONT
      IPR = IPREC
      HT = HTX
      AR = ARTX
      XU = XUPTX
      YU = YUPTX
      SP = STX
      TXJ = CTXJ

      END
