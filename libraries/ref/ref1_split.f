      SUBROUTINE REF1_SPLIT( PATH, MAXLEV, NLEV, COMP, STATUS )
*+
*  Name:
*     REF1_SPLIT

*  Purpose:
*     Split an object path name into components.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL REF1_SPLIT( PATH, MAXLEV, NLEV, COMP, STATUS )

*  Description:
*     This routine splits an HDS object path name into its individual
*     components.

*  Arguments:
*     PATH = CHARACTER * ( * ) (Given)
*        The path name.
*     MAXLEV = INTEGER (Given)
*        The maximum number of components to be extracted.
*     COMP ( MAXLEV  ) = CHARACTER * ( * ) (Returned)
*        The individual component names.
*     NLEV = INTEGER (Returned)
*        The number of components in the path name.
*     STATUS = INTEGER (Given and Returned)
*        Inherited global status.

*  Copyright:
*     Copyright (C) 1987, 1992 Science & Engineering Research Council.
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
*     AJC: A.J. Chipperfield: (STARLINK, RAL)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     17-FEB-1987  (AJC):
*        Original version.
*     20-FEB-1992 (RFWS):
*        Standardise prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'REF_ERR'          ! REF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) PATH
      INTEGER MAXLEV

*  Arguments Returned:
      INTEGER NLEV
      CHARACTER * ( * ) COMP( MAXLEV )

*  Status:
      INTEGER STATUS		 ! Global status

*  Local Variables:
      INTEGER FINISH             ! Point to end of component
      INTEGER START              ! Point to start of component
      LOGICAL MORE               ! If more components


*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      MORE = .TRUE.
      START = 1
      NLEV = 0

*  Loop to process all the path components.
      DO WHILE ( MORE .AND. ( NLEV .LT. MAXLEV ) )
         FINISH = INDEX( PATH( START : ), '.' ) + START - 1

*  Normal component found.
         IF( FINISH .GT. START ) THEN
           NLEV = NLEV + 1
           COMP( NLEV ) = PATH( START : FINISH - 1 )
           START = FINISH + 1

*  No '.' found - last component.
         ELSE IF ( FINISH .LT. START ) THEN
            NLEV = NLEV + 1
            COMP( NLEV ) = PATH( START : )
            MORE = .FALSE.

*  Two consecutive '.' - an error.
         ELSE
            STATUS = REF__NAMIN
            CALL EMS_SETC( 'NAME', PATH )
            CALL EMS_REP( 'REF1_SPLIT_BAD1',
     :                    'The object name ^NAME is invalid; two ' //
     :                    'consecutive ''.'' characters appear.',
     :                    STATUS )
            MORE = .FALSE.
         END IF
      END DO

*  Check that the maximum no. of components was not exceeded. Report an
*  error if it was.
      IF ( ( NLEV .EQ. MAXLEV ) .AND. MORE ) THEN
         STATUS = REF__NAMIN
         CALL EMS_SETC( 'NAME', PATH )
         CALL EMS_SETI( 'MAXLEV', MAXLEV )
         CALL EMS_REP( 'REF1_SPLIT_BAD2',
     :                 'The object name ^NAME contains too many ' //
     :                 'components (a maximum of ^MAXLEV are ' //
     :                 'allowed).', STATUS )
      END IF

      END
