      BLOCK DATA PGP1_BLK
*+
*  Name:
*     PGP1_BLK

*  Purpose:
*     PGP Block Data Initialisation

*  Description:
*     Initialise the PGPGO Common Block so that implicit activation
*     of PGP can be done.

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
*     DLT: David Terrett (Starlink, RAL)

*  History:
*     28-JAN-1992 (DLT):
*        Original.
*-

*  Type Definitions:
      IMPLICIT NONE

*    Global variables:
      INCLUDE 'pgpgo_cmn'                  ! PGP Initialisation Switch

*    Global data:
      DATA PGPSLP /.TRUE./
*.

      END
