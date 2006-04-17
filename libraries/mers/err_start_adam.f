      SUBROUTINE ERR_START
*+
*  Name:
*     ERR_START

*  Purpose:
*     Initialise the Error Reporting System.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_START

*  Description:
*     Initialises the global variables used by the Error Reporting System. 

*  Implementation Notes:
*     This subroutine is for use only with the ADAM implementation of
*     the Error Reporting System.

*  Algorithm:
*     -  Call EMS1_MSTRT.
*     -  Call EMS1_ESTRT.

*  Copyright:
*     Copyright (C) 1983, 1989, 1990, 1991 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     SLW: Sid Wright (UCL)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J.Chipperfield (STARLINK,RAL)
*     {enter_new_authors_here}

*  History:
*     23-FEB-1983 (SLW):
*        Original version.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     11-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     12-JAN-1990 (PCTR):
*        Converted to use EMS_ calls.
*     4-JUN-1991 (PCTR):
*        Added call to EMS1_MSTRT.
*     16-FEB-2001 (AJC):
*        Avoid use of EMS internals
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  local variables:
      INTEGER STATUS                    ! Internal status
      INTEGER LEVEL                     ! The default level 
                                        ! (prevents EMS output)
*.

*  Initialise the EMS_ error reporting system.
*  We can assume the tables are initialised
!      CALL EMS1_MSTRT
!      CALL EMS1_ESTRT

*  Start a new error context
      STATUS = SAI__OK
      CALL EMS_BEGIN( STATUS )

*  Get the new level as the 'default' level
      CALL EMS_LEVEL( LEVEL )

*  Set the default level for EMS. That is a level below which EMS will not
*
      CALL EMS_TUNE( 'MSGDEF', LEVEL, STATUS )

      END
