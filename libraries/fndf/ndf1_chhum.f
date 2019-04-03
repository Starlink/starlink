      SUBROUTINE NDF1_CHHUM( HMODE, HUM, STATUS )
*+
*  Name:
*     NDF1_CHHUM

*  Purpose:
*     Validate an NDF history update mode string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CHHUM( HMODE, HUM, STATUS )

*  Description:
*     The routine checks an NDF history update mode string for
*     validity, allowing abbreviation, and returns the equivalent
*     integer code. An error is reported and STATUS set if the string
*     supplied is not valid.

*  Arguments:
*     HMODE = CHARACTER * ( * ) (Given)
*        The string to be validated. Valid values are: 'DISABLED',
*        'QUIET', 'NORMAL' or 'VERBOSE' (case insensitive).
*        Abbreviation to no less than NDF__MINAB characters are
*        allowed.
*     HUM = INTEGER (Returned)
*        The corresponding history update mode code: one of NDF__HDISA,
*        NDF__HQUIE, NDF__HNORM or NDF__HVERB, as defined in the
*        include file NDF_CONST.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     19-MAY-1993 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  External References:
      LOGICAL NDF1_SIMLR         ! String comparison with abbreviation

*  Arguments Given:
      CHARACTER * ( * ) HMODE

*  Arguments Returned:
      INTEGER HUM

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the update mode string against each valid value in turn,
*  allowing abbreviation. Assign the appropriate history update mode
*  code.
      IF ( NDF1_SIMLR( HMODE, 'DISABLED', NDF__MINAB ) ) THEN
         HUM = NDF__HDISA
      ELSE IF ( NDF1_SIMLR( HMODE, 'QUIET', NDF__MINAB ) ) THEN
         HUM = NDF__HQUIE
      ELSE IF ( NDF1_SIMLR( HMODE, 'NORMAL', NDF__MINAB ) ) THEN
         HUM = NDF__HNORM
      ELSE IF ( NDF1_SIMLR( HMODE, 'VERBOSE', NDF__MINAB ) ) THEN
         HUM = NDF__HVERB

*  If the string is not recognised, then report an error.
      ELSE
         STATUS = NDF__HUMIN
         CALL MSG_SETC( 'HMODE', HMODE )
         CALL ERR_REP( 'NDF1_CHHUM_BAD',
     :                 'Invalid history update mode string ' //
     :                 '''^HMODE'' specified (possible programming ' //
     :                 'error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CHHUM', STATUS )

      END
