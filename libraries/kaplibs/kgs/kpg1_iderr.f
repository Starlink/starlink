      SUBROUTINE KPG1_IDERR( ERR, IDSTAT, STATUS )
*+
*  Name:
*     KPG1_IDERR

*  Purpose:
*     Reports an IDI error and copies IDI status to inherited status.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_IDERR( ERR, IDSTAT, STATUS )

*  Description:
*     This routine simply packages the operations required to report
*     an error from IDI, which does not use inherited status, and
*     copy the IDI status to inherited status.  Since these operations
*     should be called after each IDI call this routine can considerably
*     shorten applications code.

*  Arguments:
*     ERR = CHARACTER * ( * ) (Given)
*        The name of the error message.
*     IDSTAT = INTEGER (Given)
*        The IDI status.  If this is IDI__OK, the routine will not
*        make an error report and the inherited status will not be
*        altered.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 April 30 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IDI_ERR'          ! IDI error codes

*  Arguments Given:
      INTEGER
     :  IDSTAT

      CHARACTER * ( * )
     :  ERR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER ERRLEN             ! Maximum number of characters in the
                                 ! error message.
      PARAMETER ( ERRLEN = 132 )

*  Local Variables:
      CHARACTER * ( ERRLEN )
     :  BUFFER                   ! Buffer for error messages

      INTEGER
     :  NCHAR                    ! Number of characters in the error
                                 ! message

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( IDSTAT .NE. IDI__OK ) THEN

*      Obtain a meaningful IDI error message.

         CALL IIDERR( IDSTAT, BUFFER, NCHAR )

*       Output this message.  Copy the IDI status, as the global status
*       must be set to not OK in order to make an error report.

         STATUS = IDSTAT
         CALL ERR_REP( ERR, BUFFER( :NCHAR ), STATUS )
      END IF

      END
