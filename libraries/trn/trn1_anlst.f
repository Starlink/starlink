      SUBROUTINE TRN1_ANLST( NLOC, LOCLST, STATUS )








*+
*  Name:
*     TRN1_ANLST

*  Purpose:
*     annul a list of HDS locators.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_ANLST( NLOC, LOCLST, STATUS )

*  Description:
*     The routine annuls a sequence of HDS locators passed as elements
*     of a character string array.  If STATUS is set on entry, this
*     routine will still attempt to execute and to annul all the
*     locators supplied, although no error report will be made if it
*     fails under these circumstances.  If an error report is made (one
*     or more of the locators could not be annulled and STATUS was not
*     set on entry) then it will only relate to the first locator which
*     caused an error.

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
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants


*  Arguments Given:
      INTEGER NLOC              ! Number of locators to annul
      CHARACTER * ( * ) LOCLST( * )
                                ! List of active locators to annul


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
      INTEGER LSTAT             ! Local status variable
      INTEGER ILOC              ! Loop counter for locators


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Initialise the local status variable.
      LSTAT = STATUS


*   Annul each locator in turn.
      DO ILOC = 1, NLOC
        CALL DAT_ANNUL( LOCLST( ILOC ), LSTAT )


*   If STATUS is not set, return the local status value.
        IF( STATUS .EQ. SAI__OK ) STATUS = LSTAT
      ENDDO


*   Exit routine.
      END
