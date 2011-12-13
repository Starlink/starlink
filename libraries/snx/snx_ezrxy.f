      SUBROUTINE snx_EZRXY (XDRA, YDRA, NPTS, XLAB, YLAB, GLAB)

*+
*  Name:
*     EZRXY

*  Purpose:
*     Version of NCAR AUTOGRAPH routine EZXY which includes
*     axis labelling.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     XDRA() = REAL (Given)
*         Array of x data
*     YDRA() = REAL (Given)
*         Array of y data
*     NPTS = INTEGER (Given)
*         Number of points
*     XLAB = CHAR (Given)
*         X axis label (bottom)
*     YLAB = CHAR (Given)
*         Y axis label (left)
*     GLAB = CHAR (Given)
*         Graph label (top)

*  Copyright:
*     Copyright (C) 1986 Science & Engineering Research Council. All
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
*     {enter_new_authors_here}

*  History:
*     01-APR-1986 (PTW):
*        Original.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     snx_AGLAB, EZXY

*-

      IMPLICIT NONE

      REAL XDRA,YDRA
      INTEGER NPTS
      CHARACTER*(*) XLAB,YLAB,GLAB



*  Set up axis labels
      CALL snx_AGLAB('B',XLAB)
      CALL snx_AGLAB('L',YLAB)
      CALL snx_AGLAB('T',GLAB)

*  Plot the graph
      CALL EZXY(XDRA,YDRA,NPTS,CHAR(0))

      END
