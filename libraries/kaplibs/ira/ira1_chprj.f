      SUBROUTINE IRA1_CHPRJ( IN, OUT, NP, STATUS )
*+
*  Name:
*     IRA1_CHPRJ

*  Purpose:
*     Identify a projection.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_CHPRJ( IN, OUT, NP, STATUS )

*  Description:
*     The input string should contain an unambiguous abbreviation of a
*     projection name. The output string returns the full name of
*     the projection name. If the input value is not legal, or is
*     ambiguous, an error is generated. The number of parameter values
*     used by the projection is also returned.

*  Arguments:
*     IN = CHARACTER * ( * ) (Given)
*        The input string.
*     OUT = CHARACTER * ( * ) (Returned)
*        The full projection name.
*     NP = INTEGER (Returned)
*        The number of parameters used in the projection.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JAN-1991 (DSB):
*        Original version.
*     23-APR-1991 (DSB):
*        Name changed from IRA_$CHPRJ to IRA1_CHPRJ
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Arguments Given:
      CHARACTER IN*(*)

*  Arguments Returned:
      CHARACTER OUT*(*)
      INTEGER   NP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CHR_LEN            ! Function giving used length of a string.
      INTEGER END                ! End position of a name.
      CHARACTER LIST*(IRA__SZPLS)! List of supported projection names.
      INTEGER LPROJ              ! Used length of PROJ.
      LOGICAL MORE               ! True if more projection names remain.
      INTEGER NMATCH             ! No. of matches found between IN and
                                 ! LIST.
      CHARACTER PROJ*(IRA__SZPRJ)! Tidy version of IN.
      INTEGER START              ! Start position of a name.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Strip leading blanks from the input projection name and convert to
*  upper case.
      CALL CHR_FANDL( IN, START, END )
      PROJ = IN( START:END )
      CALL CHR_UCASE( PROJ )
      LPROJ = END - START + 1

*  Get a string containing the list of supported projection names.
      CALL IRA_IPROJ( LIST, STATUS )

*  Loop round each name in the list.
      MORE = .TRUE.
      START = 1
      NMATCH = 0

      DO WHILE( MORE )

*  Get the next name from the list.
         END=INDEX( LIST(START:), ',' ) + START - 2
         IF( END .EQ. START - 2 ) THEN
            END = CHR_LEN( LIST)
            MORE = .FALSE.
         END IF

*  See if the input string is an abbreviation of this name. If it is
*  increment the number of matches found and store the full name.
         IF( INDEX( LIST( START:END ), PROJ( :LPROJ ) ) .EQ. 1 ) THEN
            NMATCH = NMATCH + 1
            OUT = LIST( START:END )
         END IF

*  Set the start of the next name in the list
         START = END + 2

      END DO

*  If no matches were found, give an error.
      IF( NMATCH .EQ. 0 ) THEN
         STATUS = IRA__BADPR
         CALL MSG_SETC( 'PROJ', PROJ(:LPROJ) )
         CALL ERR_REP( 'IRA1_CHPRJ_ERR1',
     :            'IRA1_CHPRJ: Unsupported projection specified: ^PROJ',
     :                 STATUS )

*  If the input name was ambiguous, give an error message.
      ELSE IF( NMATCH .GT. 1 ) THEN
         STATUS = IRA__AMBPR
         CALL MSG_SETC( 'PROJ', PROJ(:LPROJ) )
         CALL ERR_REP( 'IRA1_CHPRJ_ERR2',
     :              'IRA1_CHPRJ: Ambiguous projection specified: ^PROJ',
     :                 STATUS )

      END IF

*  Set up the number of projection parameters. All currently defined
*  projections need same number of parameters, but this may change in
*  future.
      NP = IRA__MAXP

      END
