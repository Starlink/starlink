      SUBROUTINE SGS_ANNUL ( ZONE, STATUS )
*+
*  Name:
*     SGS_ANNUL

*  Purpose:
*     Close a graphics workstation without cancelling the parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SGS_ANNUL ( ZONE, STATUS )

*  Description:
*     De-activate and close the graphics workstation whose base
*     zone (ZONE) was obtained using SGS_ASSOC, and annul the zone
*     identifier.
*     Do not cancel the associated parameter.

*  Arguments:
*     ZONE = INTEGER (Given and returned)
*        A variable containing the base zone identifier.
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
*     Flush SGS.
*     Check that the zone number is valid, and is active.
*     Release the relevant table entries.
*     Set the zone identifier to zero.

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
*     16-APR-1983 (SLW):
*        Original.
*     13-MAR-1985 (BDK):
*        ADAM version.
*     10-APR-1985 (BDK):
*        Flush SGS.
*     13-MAR-1986 (AJC):
*        Use SGS_CLSWK instead of SGS_RELZ.
*     14-FEB-1990 (AJC):
*        Add error reports.
*        Improve documentation.
*     27-NOV-1990 (PMA):
*        Convert prologue to new style.
*        Remove tabs from in line comments and tidy up definitions of
*        variables.
*     10-JAN-1992 (DLT):
*        Eliminate redunant include files and add par_par
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAE Symbolic Constants
      INCLUDE 'PAR_PAR'          ! PAR Symbolic Constants
      INCLUDE 'sgsenv_par'       ! SGS Environment Symbolic Constants


*  Global Variables:
      INCLUDE 'sgspa_cmn'        ! SGS Parameter Table


*  Arguments Given:
      INTEGER ZONE               ! SGS zone number

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER RZD                ! Relative zone descriptor
      INTEGER ISTAT              ! Local status value

*.


*   Save the initial status value
      ISTAT = STATUS
      STATUS = SAI__OK

*   Set error context level
      CALL ERR_MARK

*   Flush SGS
      CALL SGS_FLUSH

*   Check the zone exists
      CALL SGS1_CHKZN ( ZONE, RZD, STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SGS_CLSWK ( ZONE, STATUS)

*       Set current state
         PTNAME(RZD) = ' '
         PDESC(RZD) = 0
         PFREE(RZD) = .TRUE.
         ZONE = 0

      ELSE
         CALL ERR_REP( 'SGS_ANNUL_UNKPA',
     :   'SGS_ANNUL: Specified zone is not associated with a parameter',
     :   STATUS )

      ENDIF

*   If initial status was bad, ignore all internal errors
      IF (ISTAT .NE. SAI__OK) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = ISTAT
      ENDIF

*   Release the error context
      CALL ERR_RLSE

      END
