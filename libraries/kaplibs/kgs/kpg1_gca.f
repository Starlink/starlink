      SUBROUTINE KPG1_GCA( PX, PY, QX, QY, NX, NY, LX, LY, KCOLA,
     :                     STATUS )
*+
*  Name:
*     KPG1_GCA

*  Purpose:
*     Plots a cell array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GCA( PX, PX, QX, QY, NX, NY, LX, LY, KCOLA, STATUS )

*  Description:
*     This uses GKS to plot a cell array.  It is needed as an interface
*     procedure for GKS 7.2 and 7.4 GCA usage, since these have
*     different argument lists.  This is the GKS 7.4 version.

*  Arguments:
*     PX = REAL (Given)
*        X world co-ordinate of lower-left corner of the cell array.
*     PY = REAL (Given)
*        Y world co-ordinate of lower-left corner of the cell array.
*     QX = REAL (Given)
*        X world co-ordinate of upper-right corner of the cell array.
*     QY = REAL (Given)
*        Y world co-ordinate of upper-right corner of the cell array.
*     NX = INTEGER (Given)
*        Number of columns in the cell array.
*     NY = INTEGER (Given)
*        Number of lines in the cell array.
*     LX = INTEGER (Given)
*        X dimension of the cell array.
*     LY = INTEGER (Given)
*        Y dimension of the cell array.
*     KCOLA( LX, LY ) = INTEGER (Given)
*        The array of colour indices.
*     STATUS = INTEGER (Given)
*        The global status.

*  Prior Requirements:
*     -  GKS must be in state WSAC or SGOP.

*  Notes:
*     -  There is no validation by this routine of the input arguments
*     as they are passed directly to GCA.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 April 30 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER LX
      INTEGER LY
      INTEGER KCOLA( LX, LY )
      REAL PX
      REAL PY
      REAL QX
      REAL QY

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call the GKS 7.4 cell array routine.  Note that the full array is
*  plotted.
      CALL GCA( PX, QY, QX, PY, NX, NY, 1, 1, LX, LY, KCOLA )

      END
