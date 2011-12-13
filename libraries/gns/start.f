      SUBROUTINE GNS_START ( PKG, STATUS )

*+
*  Name:
*     GNS_START

*  Purpose:
*     Start the GNS system

*  Invocation:
*     CALL GNS_START( PKG, STATUS )

*  Description:
*     The GNS databases for the specified package are opened. This routine
*     is called automatically by any other routine that accesses the
*     databases.

*  Arguments:
*     PKG = CHARACTER*(*) (Given)
*        The package name. The only packages currently supported are
*        GKS and IDI.
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Copyright:
*     Copyright (C) 1989, 1990, 1992 Science & Engineering Research Council.
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
*      3-APR-1989 (DLT):
*        Original version.
*      9-JUL-1990 (NE):
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

      IF (STATUS.EQ.0) THEN

         IF (PKG.EQ.'GKS'.OR.PKG.EQ.'gks') THEN
            CALL gns_1INITG(STATUS)
         ELSE IF (PKG.EQ.'IDI'.OR.PKG.EQ.'idi') THEN
            CALL gns_1INITI(STATUS)
         ELSE
            STATUS = GNS__PKGNS
            CALL EMS_REP( 'GNS_START_PKGNS',
     :                    'Package not supported by GNS', STATUS )
         END IF
      END IF
      END

