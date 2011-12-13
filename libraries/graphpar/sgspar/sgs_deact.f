      SUBROUTINE SGS_DEACT ( STATUS )
*+
*  Name:
*     SGS_DEACT

*  Purpose:
*     De-activate ADAM SGS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SGS_DEACT ( STATUS )

*  Description:
*     De-activate ADAM SGS after use by an application.

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
*     Free all resources still allocated.
*     Shut down SGS.

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
*     PMA: Peter Allan (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     15-OCT-1981 (SLW):
*        Original.
*     17-APR-1983 (SLW):
*       Starlink Version.
*     13-MAR-1985 (BDK):
*        ADAM version.
*     28-APR-1986 (AJC):
*        GKS7.2 version.
*     16-FEB-1990 (AJC):
*        Improve documentation.
*     27-NOV-1990 (PMA):
*        Convert prologue to new style.
*        Remove tabs from in line comments and improve the layout of
*        variable declarations.
*     13-JAN-1992 (DLT):
*        Eliminate redundant includes and add par_par
*        Replace call to SGS_$GKERR with EMS_STAT
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAE Symbolic Constants

      INCLUDE 'PAR_PAR'          ! PAR Symbolic Constants

      INCLUDE 'SGS_ERR'          ! SGS Error codes

      INCLUDE 'GKS_ERR'          ! GKS Error codes

      INCLUDE 'sgsenv_par'       ! SGS Environment Symbolic Constants


*  Global Variables:
      INCLUDE 'sgsgo_cmn'        ! SGS Initialisation Switch

      INCLUDE 'sgspa_cmn'        ! SGS Parameter Table

      EXTERNAL SGS1_BLK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ISTAT              ! Local status
      INTEGER I                  ! Loop index
      INTEGER LASTER             ! Last error report on stack

*.


*   Save the initial status value
      ISTAT = STATUS
      STATUS = SAI__OK

*   Set error context level
      CALL ERR_MARK

*   Annul all used parameters
      DO I = 1, SGS__MXPAR
         IF ( .NOT. PFREE(I) ) THEN
            CALL SGS_ANNUL ( PDESC(I), STATUS )
         ENDIF
      ENDDO

*   Close SGS
      CALL SGS_CLOSE

*   Check for GKS errors
      CALL EMS_STAT(LASTER)
      IF ( LASTER.EQ.GKS__ERROR) ISTAT = GKS__ERROR

*   Set SGS asleep
      SGSSLP = .TRUE.

*   If initial status was bad, ignore all internal errors
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = ISTAT
      ENDIF

*   Release error context
      CALL ERR_RLSE

      END
