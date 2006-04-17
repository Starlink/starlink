      SUBROUTINE ERR_CLEAR( STATUS )
*+
*  Name:
*     ERR_CLEAR

*  Purpose:
*     Return the error table to the default context and flush its
*     contents.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_CLEAR( STATUS )

*  Description:
*     The Error Reporting System is returned to its default context
*     level and any pending messages are flushed. This routine
*     effectively resets the Error Reporting System: 
*
*        -  unlike ERR_FLUSH, no 'faulty application' error message is 
*        reported if it is called when there are no error messages 
*        pending output, or if it is called with the status value set 
*        to SAI__OK; 
*        -  the error table is always annulled by a call to ERR_CLEAR, 
*        irrespective of any message output errors which may occur. 
*
*     On exit, the status is always returned as SAI__OK.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status.

*  Implementation Notes:
*     This subroutine is for use only with the ADAM implementation of
*     the Error Reporting System.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995, 2001 Central Laboratory of the Research Councils.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J.Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1991 (PCTR):
*        Original version.
*     14-DEC-1995 (AJC):
*        Correct EMS1_IEPND to LOGICAL type
*     16-FEB-2001 (AJC):
*        Avoid EMS internals
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LEVEL              ! Error context level
      INTEGER TSTLEV             ! Test level variable
*.

*  Initialise the error context level.
      TSTLEV = 0
      CALL EMS_LEVEL( LEVEL )

*  Loop to return the Error Reporting System to the default context
*  level. We assume we're there when EMS_RLSE will go no lower.
*  DO WHILE loop.
 10    CONTINUE     
      IF ( LEVEL .NE. TSTLEV ) THEN
         TSTLEV = LEVEL
         CALL EMS_RLSE
         CALL EMS_LEVEL( LEVEL )
         GO TO 10
      END IF

*  Check if there are any error messages pending output in the error
*  table.
      CALL EMS_STAT( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
*     There are error messages pending output, so call ERR_FLUSH to
*     deliver them to the user.
         CALL ERR_FLUSH( STATUS )

*     Check the returned status for output errors: if they have
*     occurred, annul the error table at the current (default) context.
         IF ( STATUS .NE. SAI__OK ) CALL EMS_ANNUL( STATUS )
      ELSE

*     There are no pending error messages, so just reset the status to
*     SAI__OK.
         STATUS = SAI__OK
      END IF

      END
