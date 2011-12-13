      SUBROUTINE GKS1_CHKID ( WKID, RGD, STATUS )
*+
*  Name:
*     GKS1_CHKID

*  Purpose:
*     Get graphics descriptor from Workstation identifier

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GKS1_CHKID ( WKID, RGD, STATUS )

*  Description:
*     Given the GKS Workstation-ID, the corresponding Graphics
*     descriptor is found.

*  Arguments:
*     WKID = INTEGER (Given)
*        Workstation identifier
*     RGD = INTEGER (Returned)
*        Relative graphics descriptor
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Notes:
*     No error report is made when status GKS__UNKPA is set.
*     This is deliberate.

*  Algorithm:
*     The Workstation id is looked up in the GKS_PA Common block. .
*     If it is found, then the graphics descriptor corresponding to
*     its position in the table is returned.

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
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelly (ROE)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*    History:
*  History:
*     11-OCT-1983 (SLW):
*        Starlink Version.
*     07-MAR-1985 (BDK):
*        ADAM version
*     19-FEB-1990 (AJC):
*        Justify no error report
*     09-JAN-1992 (DLT):
*        Renamed to GKS1_CHKID and comments reformated
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'   ! SAE Symbolic Constants
      INCLUDE 'PAR_PAR'   ! PAR Symbolic Constants
      INCLUDE 'GKS_ERR'   ! GKS Error codes
      INCLUDE 'gksenv_par'              ! GKS Environment Symbolic Constants

*  Arguments given:
      INTEGER WKID                      ! Workstation-ID

*  Arguments returned:
      INTEGER RGD                       ! relative graphics descriptor

*  Status:
      INTEGER STATUS                    ! status return

*    Global variables :
      INCLUDE 'gkspa_cmn'               ! GKS Parameter Table

*    Local variables :
      INTEGER I                         ! Loop index
      LOGICAL DONE                      ! loop controller
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      DONE = .FALSE.
      I = 1

      DO WHILE ( ( .NOT. DONE ) .AND. ( I .LE. GKS__MXPAR ) )

         IF ( ( .NOT. PFREE(I) ) .AND. ( WKID .EQ. PDESC(I) ) ) THEN
            RGD = I
            DONE = .TRUE.
         ELSE
            I = I + 1
         ENDIF

      ENDDO

      IF ( .NOT. DONE ) THEN

*      Set status
*      This is an internal routine and it is known that more helpful
*      messages will be produced by the calling routines.
         STATUS = GKS__UNKPA
      ENDIF

      END
