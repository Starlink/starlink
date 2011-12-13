      SUBROUTINE SGS_CANCL ( PNAME, STATUS )
*+
*  Name:
*     SGS_CANCL

*  Purpose:
*     Close a graphics workstation and cancel the parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SGS_CANCL ( PNAME, STATUS )

*  Description:
*     De-activate and close the graphics workstation associated with
*     the specified graphics device parameter and cancel the parameter.
*     The workstation must have been opened using SGS_ASSOC.

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
*     Flush SGS.
*     The zone descriptor associated with this parameter is
*     found, then the device is deassigned.
*     The parameter is then canceled.

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
*     BDK: Dennis Kelley (ROE)
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
*     15-SEP-1986 (AJC):
*        Shorten prologue lines for DOMAN.
*     14-FEB-1990 (AJC):
*        Add error reports and improve comments.
*        Improve documentation.
*        Prevent PAR_CANCL if not associated with SGS.
*     27-NOV-1990 (PMA):
*        Convert prologue to new style.
*        Remove tabs from in line comments and improve layout of
*        variable declarations.
*     13-JAN-1992 (DLT):
*        Eliminate redundant include files and add PAR.PAR
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

      INCLUDE 'sgsenv_par'       ! SGS Environment Symbolic Constants


*  Global Variables:
      INCLUDE 'sgspa_cmn'        ! SGS Parameter Table


*  Arguments Given:
      CHARACTER*(*) PNAME        ! Zone Parameter Name

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ISTAT              ! Local status
      INTEGER RZD                ! Relative zone descriptor

*.


*   Save the initial status
      ISTAT = STATUS
      STATUS = SAI__OK

*   Set error context level
      CALL ERR_MARK

*   Flush SGS
      CALL SGS_FLUSH

*   Look up the zone
      CALL SGS1_FNDZD ( PNAME, RZD, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
*      The parameter is associated with zone descriptor RZD
         IF ( PDESC(RZD) .NE. 0 ) THEN
             CALL SGS_CLSWK( PDESC(RZD) ,STATUS )
         ENDIF

*      Set current state
         PTNAME(RZD) = ' '
         PDESC(RZD) = 0
         PFREE(RZD) = .TRUE.

*      Cancel the parameter
         CALL PAR_CANCL ( PNAME, STATUS )


      ELSE
*      Attempt to cancel parameter not associated with a device
         CALL MSG_SETC( 'PAR', PNAME )
         CALL ERR_REP('SGS_CANCL_UNKPA',
     :   'SGS_CANCL: Parameter ^PAR is not associated with SGS zone',
     :   STATUS )

      ENDIF

*   If initial status was bad, ignore all internal errors
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = ISTAT
      ENDIF

*   Release error context level
      CALL ERR_RLSE

      END
