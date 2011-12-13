      SUBROUTINE sgs_CLOSE
*+
*  Name:
*     CLOSE

*  Purpose:
*     Flush the buffers, close each open workstation (deactivating if
*     active), and close GKS.

*  Language:
*     Starlink Fortran 77

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
*     sgs_FLUSH, sgs_1GKSTM, gns_STOP

*-

      IMPLICIT NONE

      INTEGER JSTAT



      JSTAT = 0

*  Flush the buffers
      CALL sgs_FLUSH

* Terminate GKS
      CALL sgs_1GKSTM(JSTAT)

* Shut down the GNS database
      CALL gns_STOP('GKS',JSTAT)

      END
