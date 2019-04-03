      SUBROUTINE NDF1_GTXTN( NAME, MXXTN, DEF, XTN, XTN1, XTN2, NXTN,
     :                       STATUS )
*+
*  Name:
*     NDF1_GTXTN

*  Purpose:
*     Get a list of NDF extension names from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_GTXTN( NAME, MXXTN, DEF, XTN, XTN1, XTN2, NXTN, STATUS )

*  Description:
*     The routine obtains a list of NDF extension names from the
*     environment and parses and validates this list. It is intended for
*     obtaining lists of extensions which are to be recognised when
*     converting to or from foreign format data files.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the environment variable which contains the list.
*     MXXTN = INTEGER (Given)
*        Maximum number of extension names that can be accommodated.
*     DEF = LOGICAL (Returned)
*        Whether the environment variable was defined.
*     XTN = CHARACTER * ( * ) (Returned)
*        The translation of the specified environment variable, with all
*        extension name fields validated and converted to upper case.
*     XTN1( MXXTN ) = INTEGER (Returned)
*        The character positions in the XTN string at which each
*        extension name field begins.
*     XTN2( MXXTN ) = INTEGER (Returned)
*        The character positions in the XTN string at which each
*        extension name field ends.
*     NXTN = INTEGER (Returned)
*        The number of extension names found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     If the specified environment variable is not defined, then DEF
*     will be set to .FALSE. and no further extension name information
*     will be returned.

*  Copyright:
*     Copyright (C) 1994 Particle Physics & Astronomy Research Council

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
*     31-OCT-1994 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PSX_ERR'          ! PSX_ error codes

*  Arguments Given:
      CHARACTER * ( * ) NAME
      INTEGER MXXTN

*  Arguments Returned:
      LOGICAL DEF
      CHARACTER * ( * ) XTN
      INTEGER XTN1( MXXTN )
      INTEGER XTN2( MXXTN )
      INTEGER NXTN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER F                  ! First character position
      INTEGER IXTN               ! Loop counter for extension names
      INTEGER L                  ! Last character position

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Translate the environment variable to obtain a list of NDF extension
*  names. Note if there is no translation.
      DEF = .TRUE.
      CALL ERR_MARK
      CALL PSX_GETENV( NAME, XTN, STATUS )
      IF ( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         DEF = .FALSE.
      END IF
      CALL ERR_RLSE

*  If the environment variable was defined, parse the resulting
*  extension list to split it into separate fields containing extension
*  names. Return the field positions and number of extension fields
*  found.
      NXTN = 0
      IF ( ( STATUS .EQ. SAI__OK ) .AND. DEF ) THEN
         CALL NDF1_PSFFL( XTN, MXXTN, XTN1, XTN2, NXTN, STATUS )

*  If OK, then loop to validate each extension name.
         IF ( STATUS .EQ. SAI__OK ) THEN
            DO 1 IXTN = 1, NXTN
               F = XTN1( IXTN )
               L = XTN2( IXTN )

*  Check each name for validity. If OK, then convert it to upper case.
               CALL NDF1_CHXNM( XTN( F : L ), STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL CHR_UCASE( XTN( F : L ) )

*  Abort if a bad name is encountered.
               ELSE
                  GO TO 2
               END IF
 1          CONTINUE
 2          CONTINUE
         END IF

*  If an error occurred, then report contextual information.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'NDF1_GTXTN_BAD',
     :           'Error occurred while reading the ^NAME list of ' //
     :           'NDF extension names (possible bad environment ' //
     :           'variable setting).', STATUS )
         END IF
      END IF

*  Call error tracing routine if necessary.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_INFCB', STATUS )

      END
