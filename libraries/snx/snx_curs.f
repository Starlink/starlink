      SUBROUTINE snx_CURS (X,Y,N)

*+
*  Name:
*     CURS

*  Purpose:
*     Read a cursor position

*  Language:
*     Starlink Fortran 77

*  Description:
*     This routine may only be called after NCAR AUTOGRAPH has been
*     used to plot a graph, thus having specified the mapping between
*     the coordinate systems involved.
*
*     Variations of choice device, cursor visibility, echo type, etc
*     may be made by direct SGS/GKS calls prior to calling this
*     routine.

*  Arguments:
*     X,Y = REAL (Given & Returned)
*         Where cursor is to be preset to if possible
*         and where cursor was when choice was made
*         X,Y are USER coordinates (i.e. data coordinates)
*     N = INTEGER (Returned)
*         Choice selected

*  Copyright:
*     Copyright (C) 1987 Science & Engineering Research Council. All
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
*     01-MAY-1987 (PTW):
*        Original.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     PLOTIT, sgs_FLUSH, sgs_SETCU, sgs_REQCU,
*     snx_TO, snx_AGCS, snx_AGUGX, snx_AGUGY, snx_AGGUX, snx_AGGUY

*-

      IMPLICIT NONE

      REAL X,Y
      INTEGER N

      REAL XG,YG

      REAL snx_AGUGX,snx_AGUGY,snx_AGGUX,snx_AGGUY



*  Flush
      CALL PLOTIT(0,0,2)
      CALL sgs_FLUSH

*  Save NCAR normalisation transformation
      CALL snx_TO('SGS')

*  Make the SGS world coordinates match the AUTOGRAPH grid coordinates
      CALL snx_AGCS

*  Transform preset user coordinates to grid coordinates
      XG = snx_AGUGX(X)
      YG = snx_AGUGY(Y)

*  Set the cursor position (if possible)
      CALL sgs_SETCU(XG,YG)

*  Get a cursor position (grid coordinates)
      CALL sgs_REQCU(XG,YG,N)

*  Convert to user coordinates
      X = snx_AGGUX(XG)
      Y = snx_AGGUY(YG)

*  Restore NCAR normalisation transformation
      CALL snx_TO('NCAR')

      END
