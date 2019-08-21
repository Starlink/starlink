      SUBROUTINE NDF1_VACCN( CCOMP, ICCOMP, STATUS )
*+
*  Name:
*     NDF1_VACCN

*  Purpose:
*     Validate an NDF axis character component name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VACCN( CCOMP, ICCOMP, STATUS )

*  Description:
*     The routine checks that the name of an NDF axis character
*     component is valid (or is a valid abbreviation) and returns an
*     integer identifying the character component. If the name is not
*     valid, then an error is reported.

*  Arguments:
*     CCOMP = CHARACTER * ( * ) (Given)
*        The component name to be validated.
*     ICCOMP = INTEGER (Returned)
*        An identifier for the character component (one of the symbolic
*        constants NDF__ALAB or NDF__AUNI).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Compare the component name with each permitted value in turn,
*     allowing abbreviations and assigning the appropriate returned
*     value.
*     -  If the component name was not recognised, then report an error.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-JUL-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) CCOMP

*  Arguments Returned:
      INTEGER ICCOMP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare allowing abbreviation

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Compare the component name with each permitted value in turn,
*  allowing abbreviations and assigning the appropriate returned value.

*  ...LABEL component.
      IF ( NDF1_SIMLR( CCOMP, 'LABEL', NDF__MINAB ) ) THEN
         ICCOMP = NDF__ALAB

*  ...UNITS component.
      ELSE IF ( NDF1_SIMLR( CCOMP, 'UNITS', NDF__MINAB ) ) THEN
         ICCOMP = NDF__AUNI

*  If the component name was not recognised, then report an error.
      ELSE
         STATUS = NDF__CNMIN
         CALL MSG_SETC( 'BADCOMP', CCOMP )
         CALL ERR_REP( 'NDF1_VACCN_BAD',
     :                 'Invalid axis character component name ' //
     :                 '''^BADCOMP'' specified (possible ' //
     :                 'programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VACCN', STATUS )

      END
