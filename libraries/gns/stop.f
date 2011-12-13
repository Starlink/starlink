      SUBROUTINE GNS_STOP ( PKG, STATUS )

*+
*  Name:
*     GNS_STOP

*  Purpose:
*     Stop the GNS system

*  Invocation:
*     CALL GNS_STOP( PKG, STATUS )

*  Description:
*     The GNS databases for the specified package are closed.

*  Arguments:
*     PKG = CHARACTER*(*) (Given)
*        The package name. The only packages currently supported are
*        GKS and IDI.
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Copyright:
*     Copyright (C) 1988, 1990, 1992 Science & Engineering Research Council.
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
*     DLT: D.L. Terrett (STARLINK)
*     NE: Nick Eaton (Durham University)

*  History:
*     16-MAY-1988 (DLT):
*        Original version.
*      9-JUL-1990
*        Added error reporting
*      1-SEP-1992 (NE):
*        Updated prologue.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'GNS_ERR'

*  Arguments Given:
      CHARACTER*(*) PKG

*  Status:
      INTEGER STATUS
*.

      IF (PKG.EQ.'GKS'.OR.PKG.EQ.'gks') THEN
         CALL gns_1TERMG(STATUS)
      ELSE IF (PKG.EQ.'IDI'.OR.PKG.EQ.'idi') THEN
         CALL gns_1TERMI(STATUS)
      ELSE
         STATUS = GNS__PKGNS
         CALL EMS_REP( 'GNS_STOP_PKGNS',
     :                 'Package not supported by GNS', STATUS )
      END IF

      END

