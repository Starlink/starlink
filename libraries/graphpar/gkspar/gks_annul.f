      SUBROUTINE GKS_ANNUL ( WKID, STATUS )
*+
*  Name:
*     GKS_ANNUL

*  Purpose:
*     Close a graphics workstation without cancelling the parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GKS_ANNUL( WKID, STATUS )

*  Description:
*     De-activate and close the graphics workstation whose Workstation
*     Identifier was obtained using GKS_ASSOC, and annul the
*     Workstation Identifier. Do not cancel the associated parameter.

*  Arguments:
*     WKID = INTEGER (Given)
*        A variable containing the Workstation Identifier.
*        The value is destroyed by this routine.
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
*     Check that the descriptor is valid, and is active.
*     Release the relevant table entries.

*  Copyright:
*     Copyright (C) 1983, 1985, 1986, 1990, 1992 Science & Engineering Research Council.
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
*     AJC: A J Chipperfield (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1983 (SLW):
*        Starlink Version.
*     05-MAR-1985 BDK):
*        ADAM version.
*     12-SEP-1986 (AJC):
*        Correct error reporting.
*        Shorten prologue lines.
*     21-NOV-1990 (PMA):
*        Convert prologue to new style so that documentation can be
*        generated autometically.
*        Convert type definitions to new style.
*     09-JAN-1992 (DLT):
*        Change names of internal routines from GKS_ to GKS1_
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE constants
      INCLUDE 'PAR_PAR'     ! PAR constants
      INCLUDE 'gksenv_par'                ! GKS Environment Symbolic Constants

*  Global Variables:
      INCLUDE 'gkspa_cmn'                 ! GKS Parameter Table
*        PFREE(GKS__MXPAR ) = LOGICAL (Write)
*           Whether slot used
*        PDESC( GKS__MXPAR ) = INTEGER (Write)
*           Workstation ID
*        PTNAME( GKS__MXPAR ) = CHARACTER * ( PAR__SZNAM ) (Write)
*           Parameter names

*  Arguments Given:
      INTEGER WKID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER RGD                ! Relative graphics descriptor
      INTEGER ISTAT              ! Local status value

*.

*   Save the initial status value
      ISTAT = STATUS
      STATUS = SAI__OK

*   Set error context level
      CALL ERR_MARK

*   Get the graphics descriptor for this WKID
      CALL GKS1_CHKID ( WKID, RGD, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( PDESC(RGD) .NE. 0 ) THEN
            CALL GKS1_DEAS ( PDESC(RGD), ISTAT )
         ENDIF
*
*       Set current state
*
         PTNAME(RGD) = ' '
         PDESC(RGD) = 0
         PFREE(RGD) = .TRUE.
         WKID = 0
      ENDIF

*   If initial status was bad, ignore all internal errors
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = ISTAT
      ENDIF

*   Release the error context
      CALL ERR_RLSE

      END
