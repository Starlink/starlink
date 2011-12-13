      SUBROUTINE GNS_TNG ( NAME, IWKTYP, ICONID, STATUS )

*+
*  Name:
*     GNS_TNG

*  Purpose:
*     Translate name to a GKS device specification

*  Invocation:
*     CALL GNS_TNG( NAME, IWKTYP, ICONID, STATUS )

*  Description:
*     The workstation name is translated to a GKS workstation type and
*     connection identifier and, if necessary, a logical name created to
*     map the connection identifier onto the device implied by the
*     workstation name.
*
*     This routine is the same as GNS_TNDG but without an explicit
*     physical device name argument.

*  Arguments:
*     NAME = CHARACTER*(*) (Given)
*        Workstation name
*     IWKTYP = INTEGER (Returned)
*        GKS workstation type
*     ICONID = INTEGER (Returned)
*        Connection identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
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
*      3-JUN-1988 (DLT):
*        Original version.
*      1-SEP-1992 (NE):
*        Updated prologue.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      CHARACTER*(*) NAME

*  Arguments Returned:
      INTEGER IWKTYP
      INTEGER ICONID

*  Status:
      INTEGER STATUS
*.

      CALL gns_TNDG(NAME,' ',IWKTYP,ICONID,STATUS)

      END

