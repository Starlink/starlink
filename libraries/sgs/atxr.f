      SUBROUTINE sgs_ATXR (R, NFI, NDP)
*+
*  Name:
*     ATXR

*  Purpose:
*     Format a real number and append to the text buffer.

*  Language:
*     Starlink Fortran 77

*  Description:
*     The field width is limited in size - see coding.

*  Arguments:
*     R = REAL (Given)
*         Real number to be formatted
*     NFI = INTEGER (Given)
*         I Format Indicator:
*            either  number of leading spaces (NFI.GE.0)
*            or  minus the field width (NFI.LT.0)
*     NDP = INTEGER (Given)
*         Number of decimal places
*           if NDP.LT.0, only the integer part appears
*           if NDP.EQ.0, the decimal point appears
*           if NDP.GT.0, NDP digits appear after the point

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

*  Externals:
*     sgs_ATXB, sgs_ATEXT

*-

      IMPLICIT NONE

      REAL R
      INTEGER NFI,NDP

      INTEGER NF,NW,ILAST
      PARAMETER (NW=100)
      CHARACTER FMT*20,STRING*(NW)



*  Build format specification
      NF=MAX(0,NDP)
      WRITE (FMT,'('' (F'',I3,''.'',I3,'')'')') NW,NF

*  Format the number
      WRITE (STRING,FMT) R

*  Point to last character to be appended
      IF (NDP.GE.0) THEN
         ILAST=NW
      ELSE
         ILAST=NW-1
      END IF

*  Append the field in the appropriate manner
      IF (NFI.GE.0) THEN
         CALL sgs_ATXB(STRING(:ILAST),NFI)
      ELSE
         CALL sgs_ATEXT(STRING(MAX(ILAST+NFI+1,1):ILAST))
      END IF

      END
