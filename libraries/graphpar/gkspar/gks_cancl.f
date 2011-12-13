      SUBROUTINE GKS_CANCL ( PNAME, STATUS )
*+
*  Name:
*     GKS_CANCL

*  Purpose:
*     Close graphics workstation and cancel the parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GKS_CANCL ( PNAME, STATUS )

*  Description:
*     De-activate and close the graphics workstation associated with
*     the specified Graphics Device Parameter and cancel the parameter.
*     The workstation must have been opened using GKS_ASSOC.

*  Arguments:
*     PNAME = CHARACTER * ( * ) (Given)
*        Expression specifying the name of a Graphics Device
*        Parameter.
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
*     If the graphics device is currently active, de-activate it.
*     Cancel the device parameter.

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
*     AJC: Alan Chipperfield (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1983 (SLW):
*        Starlink Version.
*     06-MAR-1985 (BDK):
*        ADAM version.
*     12-SEP-1986 (AJC):
*        Correct error reporting.
*        Shorten prologue lines.
*     27-NOV-1990 (PMA):
*        Converted prologue to new style.
*        Removed tabs from in line comments.
*     09-JAN-1992 (DLT):
*        Change names of internal routines from GKS_ to GKS1_.
*        Include par_par.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'     ! SAE Symbolic Constants
      INCLUDE 'PAR_PAR'     ! PAR Symbolic Constants
      INCLUDE 'GKS_ERR'     ! GKS Error codes
      INCLUDE 'gksenv_par'                ! GKS Environment Symbolic Constants


*  Global Variables:
      INCLUDE 'gkspa_cmn'        ! GKS Parameter Table


*  Arguments Given:
      CHARACTER*(*) PNAME        ! Graphics device Parameter

*  Status:
      INTEGER STATUS             ! GlOBAL return

*  Local Variables:
      INTEGER RGD                ! Relative graphics descriptor
      INTEGER ISTAT              ! Local status

*.


*   Save initial status
      ISTAT = STATUS
      STATUS = SAI__OK

*   Set error context level
      CALL ERR_MARK

*   Find the graphics descriptor for this parameter
      CALL GKS1_FNDGD ( PNAME, RGD, STATUS)

      IF ( STATUS .EQ. SAI__OK ) THEN

         IF ( PDESC(RGD) .NE. 0 ) THEN

*         Close workstation using PDESC(RGD) = WKID
            CALL GKS1_DEAS ( PDESC(RGD), ISTAT )

         ENDIF

*      Reset common block variables
         PTNAME(RGD) = ' '
         PDESC(RGD) = 0
         PFREE(RGD) = .TRUE.

      ENDIF

*   Cancel the parameter
      CALL PAR_CANCL(PNAME, STATUS)

      IF ( ISTAT .NE. SAI__OK ) THEN

*      If initial status was bad, ignore all internal errors
         CALL ERR_ANNUL( STATUS )
         STATUS = ISTAT
      ENDIF

*   Release error context level
      CALL ERR_RLSE

      END
