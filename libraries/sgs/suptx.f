      SUBROUTINE sgs_SUPTX (XU,YU)
*+
*  Name:
*     SUPTX

*  Purpose:
*     Specify text orientation vector.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     XU = REAL (Given)
*         X 'up' vector (magnitude immaterial)
*     YU = REAL (Given)
*         Y  "     "        "          "

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
*     sgs_OTEXT, sgs_1SETTX

*  Written To Common:
*     XUPTX     r      current up vector (x)
*     YUPTX     r        "     "    "    (y)

*-

      IMPLICIT NONE

      REAL XU,YU

      REAL R,RLMIN
      PARAMETER (RLMIN = 1.0E-35)

      INCLUDE 'sgscom'




*  Flush any existing text string
      CALL sgs_OTEXT

*  Normalise & save vector
      R=SQRT(XU*XU+YU*YU)
      IF (R.GT.RLMIN) THEN
         XUPTX=XU/R
         YUPTX=YU/R
      ELSE
         XUPTX=0.0
         YUPTX=1.0
      END IF

*  Translate SGS text parameters to GKS
      CALL sgs_1SETTX

      END
