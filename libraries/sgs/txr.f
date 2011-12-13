      SUBROUTINE sgs_TXR (X,Y, R, NFI, NDP)
*+
*  Name:
*     TXR

*  Purpose:
*     Begin a new text string with a formatted real number.

*  Language:
*     Starlink Fortran 77

*  Description:
*     (In practice, this means simply "plot a formatted real number")

*  Arguments:
*     X = REAL (Given)
*         Position of text string (x)
*     Y = REAL (Given)
*         "    "   "      "   (y)
*     R = REAL (Given)
*         Real number to be formatted
*     NFI = INTEGER (Given)
*         Format indicator:-
*         either  number of leading spaces (NFI.GE.0)
*         or  minus the field width (NFI.LT.0)
*     NDP = INTEGER (Given)
*         Number of decimal places
*         If NDP.LT.0, only the integer part appears
*         If NDP.EQ.0, the decimal point appears
*         If NDP.GT.0, NDP digits appear after the point

*  Notes:
*     The field width is limited in size - see sgs_ATXR coding.

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
*     sgs_BTEXT, sgs_ATXR

*-

      IMPLICIT NONE

      REAL X,Y,R
      INTEGER NFI,NDP



*  Begin the new text string
      CALL sgs_BTEXT(X,Y)

*  Format the number onto it
      CALL sgs_ATXR(R,NFI,NDP)

      END
