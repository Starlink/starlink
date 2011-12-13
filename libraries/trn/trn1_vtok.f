      SUBROUTINE TRN1_VTOK( TOKEN, STATUS )
*+
*  Name:
*     TRN1_VTOK

*  Purpose:
*     Validate a token.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_VTOK( TOKEN, STATUS )

*  Description:
*     The routine validates a character string containing a token name.
*     If the token name is not valid, then STATUS is set and an error
*     is reported.

*  Algorithm:
*     Call TRN1_ISNAM to determine if the token is a valid name.  Set
*     STATUS and report an error if it is not.

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     6-MAY-1988 (RFWS):
*        Original version.
*     12-MAY-1988 (RFWS):
*        Re-written to call TRN1_ISNAM.
*     13-FEB-1992 (RFWS):
*        Improved the error reporting.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      CHARACTER * ( * ) TOKEN   ! The token to be validated


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
*     <declarations and descriptions for exported arguments>


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      LOGICAL OK                ! Whether the token name is valid
      INTEGER LTOK              ! Length of the token name


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Check the token is a valid name.
      CALL TRN1_ISNAM( TOKEN, OK, LTOK )


*   Report an error if it is not.
      IF( .NOT. OK ) THEN
         STATUS = TRN__TOKIN
         CALL MSG_SETC( 'TOKEN', TOKEN )
         CALL ERR_REP( 'TRN1_VTOK_BAD',
     :                 'Invalid token name ''^TOKEN'' specified ' //
     :                 '- bad syntax (possible programming error).',
     :                 STATUS )
      ENDIF


*   Exit routine.
      END
