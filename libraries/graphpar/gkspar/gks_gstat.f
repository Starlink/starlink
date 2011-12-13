      SUBROUTINE GKS_GSTAT ( STATUS )
*+
*  Name:
*     GKS_GSTAT

*  Purpose:
*     Inquire whether GKS has reported an error.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GKS_GSTAT ( STATUS )

*  Description:
*     Check whether the last reported error was GKS_ERROR and if it was, set
*     STATUS to GKS__ERROR.

*  Arguments:
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Notes:
*     The global status will be set to GKS__ERROR if GKS has reported
*     an error; otherwise it will be unchanged.  If the given value of
*     STATUS is not SAI__OK, then the routine will return without
*     action.

*  Algorithm
*     Call EMS_STAT to get the last reported error code and see if the code
*     is GKS__ERROR.

*  Copyright:
*     Copyright (C) 1985, 1986, 1990, 1992 Science & Engineering Research Council.
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
*     BDK: Denis Kelly (ROE)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     12-MAR-1985 (BDK):
*        Original.
*     21-FEB-1986 (AJC):
*        Modification to use JGKERR instead of ADMGKS.
*     12-SEP-1986 (AJC):
*        Shorten prologue lines.
*     26-FEB-1990 (AJC):
*        No action on bad given status.
*        Get include file from library.
*     27-NOV-1990 (PMA):
*        Convert prologue to new style.
*     09-JAN-1992 (DLT):
*        Re-written for te "Starlink" GKS error handler.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'GKS_ERR'


*  Status:
      INTEGER STATUS

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Test error stack for the presence of an error
      CALL EMS_STAT(STATUS)

*   Reset status to SAI__OK unless the last error was the GKS error
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS.NE.GKS__ERROR) STATUS = SAI__OK
      END IF

      END
