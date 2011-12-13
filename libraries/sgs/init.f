      SUBROUTINE sgs_INIT (LUN, JSTAT)
*+
*  Name:
*     INIT

*  Purpose:
*     Open SGS.

*  Language:
*     Starlink Fortran 77

*  Description:
*     Open GKS (unless already open), Set default values in the SGS
*     common block.

*  Arguments:
*     LUN = INTEGER (Given)
*         Logical unit for error messages
*     JSTAT = INTEGER (Returned)
*         Status: 0=OK

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
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
*     13-JAN-1992 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     sgs_ISTAT, sgs_1GKSIN, sgs_1SGSIN

*-

      IMPLICIT NONE

      INTEGER LUN,JSTAT



*  Initialise status handling mode if not already set
      CALL sgs_ISTAT(-1, JSTAT)

*  Initialise GKS
      CALL sgs_1GKSIN(LUN,JSTAT)

*  Initialise SGS
      CALL sgs_1SGSIN(JSTAT)

      END
