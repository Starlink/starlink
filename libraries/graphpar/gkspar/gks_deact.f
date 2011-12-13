      SUBROUTINE GKS_DEACT ( STATUS )
*+
*  Name:
*     GKS_DEACT

*  Purpose:
*     De-activate ADAM GKS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GKS_DEACT ( STATUS )

*  Description:
*     De-activate ADAM GKS after use by an application.

*  Arguments:
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Notes:
*     On entry, the STATUS variable holds the global status value.
*     If the given value of STATUS is SAI__OK and the routine fails to
*     complete, STATUS will be set to an appropriate error number.
*     If the given value of STATUS is not SAI__OK, then the routine
*     will still attempt to execute and will return with STATUS
*     unchanged.

*  Algorithm:
*     Annul any graphics devices still open.

*  Copyright:
*     Copyright (C) 1983, 1985, 1990, 1992 Science & Engineering Research Council.
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
*     Sid Wright.  (UCL::SLW)
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelly (ROE)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1983 (SLW):
*        Starlink Version.
*     04-MAR-1985 (BDK):
*        ADAM version.
*     19-FEB-1990 (AJC):
*        Correct error reporting.
*        Improved documentation.
*     27-NOV-1990 (PMA):
*        Converted prologue to new style.
*        Removed tabs from in line comments.
*     09-JAN-1992 (DLT):
*        Change names of internal routines from GKS_ to GKS1_.
*        Include par_par.
*        Declare GKS1_BLK as EXTERNAL to force loading on block data subprogam
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'     ! SAE Symbolic Constants
      INCLUDE 'PAR_PAR'     ! PAR Symbolic Constants
      INCLUDE 'GKS_ERR'     ! GKS Error codes
      INCLUDE 'gksenv_par'                ! GKS Environment Symbolic Constants


*  Global Variables:
      INCLUDE 'gksgo_cmn'        ! GKS Initialisation Switch
      EXTERNAL GKS1_BLK
      INCLUDE 'gkspa_cmn'        ! GKS Parameter Table


*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ISTAT              ! Local status
      INTEGER I                  ! Loop index

*.


*   Save the initial status value
      ISTAT = STATUS
      STATUS = SAI__OK

*   Set error context level
      CALL ERR_MARK

*    Release resources
      DO I = 1, GKS__MXPAR
         IF ( .NOT. PFREE(I) ) THEN
            CALL GKS_ANNUL ( PDESC(I), STATUS )
         ENDIF
      ENDDO

*    Make sure common block is in initial state
      DO I = 1, GKS__MXPAR
         PFREE(I) = .TRUE.
         PDESC(I) = 0
         PTNAME(I) = ' '
      ENDDO

*   Terminate GKS
      CALL GKS1_STOP ( STATUS )

*   If initial status was bad, ignore all internal errors
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = ISTAT
      ENDIF

*    Set GKS asleep
      GKSSLP = .TRUE.

*   Release error context
      CALL ERR_RLSE

      END
