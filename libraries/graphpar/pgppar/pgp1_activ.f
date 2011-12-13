      SUBROUTINE PGP1_ACTIV ( STATUS )
*+
*  Name:
*     PGP1_ACTIV

*  Purpose:
*     Initialise PGP library

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PGP1_ACTIV( STATUS )

*  Description:
*     The table of assigned graphics devices in the PGP_IO Common Block
*     is initialised.

*  Arguments:
*     STATUS = INTEGER (Given and returned)
*        The global status.

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
*     {enter_new_authors_here}

*    History:
*     23-JAN-1992 (DLT):
*        Original.

*  Bugs:
*     {note_any_bugs_here}

*  Type Definitions:
      IMPLICIT NONE

*    Global constants:
      INCLUDE 'SAE_PAR'   ! SAI Symbolic Constants

      INCLUDE 'PAR_PAR'   ! PAR Symbolic Constants

      INCLUDE 'pgpenv_par'              ! PGP Environment Symbolic Constants

*    Status return:
      INTEGER STATUS                    ! Status

*    Global variables:
      INCLUDE 'pgppa_cmn'               ! PGP Parameter Table

      INCLUDE 'pgpgo_cmn'               ! PGP Initialisation Switch

      EXTERNAL PGP1_BLK

*    Local variables:
      INTEGER I                         ! loop index
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise common blocks
      DO I = 1, PGP__MXPAR
         PFREE(I) = .TRUE.
         PDESC(I) = 0
         PTNAME(I) = ' '
      ENDDO

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Declare PGP awake
      PGPSLP = .FALSE.

      END
