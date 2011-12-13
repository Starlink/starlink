      SUBROUTINE GKS1_STOP(STATUS)
*+
*  Name:
*     GKS1_STOP

*  Purpose:
*     Close down GKS

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GKS1_STOP ( STATUS )

*  Description:
*     Close all open Workstations and close GKS.

*  Arguments:
*     STATUS = INTEGER (Given and returned)
*           Variable to contain the status. This routine is executed
*           regardless of the Import value of status.

*  Algorithm:
*     Ask GKS for open workstations and close them.

*  Copyright:
*     Copyright (C) 1983, 1985, 1992 Science & Engineering Research Council.
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
*     BDK: Dennis Kelly (ROE)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1983 (SLW):
*        Starlink Version.
*     06-MAR-1985 (BDK):
*        ADAM version.
*     09-JAN-1992 (DLT):
*        Reformat comments and change name to GSK1_STOP
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global constants:
      INCLUDE 'SAE_PAR'   ! SAE Global Constants
      INCLUDE 'GKS_PAR'   ! GKS kernel parameters

*  Status:
      INTEGER STATUS                    ! status return

*  Local variables :
      INTEGER ISTAT                    ! temporary status
      INTEGER NWKOPN                   ! loop controller
      INTEGER ERRIND                   ! Inquiry error indicator
      INTEGER IWKID                    ! workstation identifier
*.

      ISTAT = STATUS
      STATUS = SAI__OK

*  Shut down any open workstations
      NWKOPN = -1

      DO WHILE ( NWKOPN .NE. 0 )

         NWKOPN=0

*     Ask for an open workstation
         CALL GQOPWK( 1, ERRIND, NWKOPN, IWKID )
         IF ( NWKOPN .NE. 0 ) THEN

*        One exists. If it's active, deactivate it and close it
            CALL GKS1_DEAS(IWKID, STATUS)

         END IF

      END DO

*  Close gks
      CALL GCLKS

*  Check for GKS Errors
      CALL GKS_GSTAT(STATUS)

      IF ( ISTAT .NE. SAI__OK ) THEN
         STATUS = ISTAT
      ENDIF

      END
