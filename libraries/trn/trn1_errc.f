      SUBROUTINE TRN1_ERRC( ROUTIN, STR, CNAME, STATUS )
*+
*  Name:
*     TRN1_ERRC

*  Purpose:
*     report error about a structure component.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_ERRC( ROUTIN, STR, CNAME, STATUS )

*  Description:
*     The routine reports an error about a structure component.  It does
*     this by calling TRN1_ERROR, with error text of the form:

*        <structure>.<component>

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     11-FEB-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
*     <any INCLUDE files containing global constant definitions>


*  Arguments Given:
      CHARACTER * ( * ) ROUTIN  ! Name of routine reporting the error
      CHARACTER * ( * ) STR     ! Locator to structure
      CHARACTER * ( * ) CNAME   ! Structure component name


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
*     <declarations for local variables>


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Set message tokens for the error report.
      CALL DAT_MSG( 'STRUCTURE', STR )
      CALL MSG_SETC( 'COMPONENT', CNAME )


*   Report the error message.
      CALL TRN1_ERROR( ROUTIN, '^STRUCTURE.^COMPONENT', STATUS )


*   Exit routine.
      END
