      SUBROUTINE SGS1_ACTIV ( STATUS )
*+
*  Name:
*     SGS1_ACTIV

*  Purpose:
*     Initialise SGS library

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SGS1_ACTIV( STATUS )

*  Description:
*     The table of assigned graphics devices in the SGS_IO Common Block
*     is initialised.

*  Arguments:
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1981, 1983, 1985, 1986, 1990, 1992 Science & Engineering Research Council.
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
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelley (ROE)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*    History:
*     15-OCT-1981 (SLW):
*        Original.
*     17-APR-1983 (SLW):
*       Starlink Version.
*     13-MAR-1985 (BDK):
*        ADAM version.
*     09-APR-1985 (BDK):
*        Open GKS
*     19-FEB-1986 (AJC):
*        SGS/GKS 7.2 version
*     29-MAY-1986 (AJC):
*        Use new SGS_INIT routine
*     16-JUN-1986 (AJC):
*        SGS_INIT has additional parameter
*     14-FEB-1990 (AJC):
*        Remove commented out code
*     13-JAN-1992 (DLT):
*        Change name to SGS1_ACTIV
*        Remove redundant includes
*        Change SAI_PAR to SAE_PAR and add PAR_PAR
*        Reformat comments
*        Add new argument to SGS_ISTAT

*  Bugs:
*     {note_any_bugs_here}

*  Type Definitions:
      IMPLICIT NONE

*    Global constants:
      INCLUDE 'SAE_PAR'   ! SAI Symbolic Constants

      INCLUDE 'PAR_PAR'   ! PAR Symbolic Constants

      INCLUDE 'sgsenv_par'              ! SGS Environment Symbolic Constants

*    Status return:
      INTEGER STATUS                    ! Status

*    Global variables:
      INCLUDE 'sgspa_cmn'               ! SGS Parameter Table

      INCLUDE 'sgsgo_cmn'               ! SGS Initialisation Switch

      EXTERNAL SGS1_BLK

*    Local constants:
      INTEGER LUEGKS                    ! unit number for GKS error
      PARAMETER ( LUEGKS = 6 )          ! messages (not used under ADAM)

*    Local variables:
      INTEGER I                         ! loop index
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Establish status handling 'mode'
      CALL SGS_ISTAT(1, STATUS)

*   Initialise GKS
      CALL GKS1_ACTIV ( STATUS )

*   Initialise common blocks
      DO I = 1, SGS__MXPAR
         PFREE(I) = .TRUE.
         PDESC(I) = 0
         PTNAME(I) = ' '
      ENDDO

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise SGS error stream number is not important
      CALL SGS_INIT ( LUEGKS, STATUS )

*   Declare SGS awake
      SGSSLP = .FALSE.

      END
