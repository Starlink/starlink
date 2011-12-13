      SUBROUTINE IMG1_VPAR( PARAM, VPAR, STATUS )
*+
*  Name:
*     IMG1_VPAR

*  Purpose:
*     Validate a parameter name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_VPAR( PARAM, VPAR, STATUS )

*  Description:
*     The routine checks a parameter name for validity, returning an
*     upper-case left-justified version. If the name supplied is not
*     valid, then no value is returned and an error is reported and
*     STATUS set.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name to be validated.
*     VPAR = CHARACTER * ( * ) (Returned)
*        Upper-case, left-justified version of the parameter name. The
*        length of this argument must be at least equal to IMG__SZPAR,
*        as defined in the include file IMG_CONST.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     22-JAN-1991 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ private constants
      INCLUDE 'IMG_ERR'          ! IMG_ error codes

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      CHARACTER * ( * ) VPAR

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_ISALF          ! Is character alphabetic?
      LOGICAL CHR_ISNAM          ! Is character string a name?

*  Local Variables:
      INTEGER F                  ! First non-blank character position
      INTEGER L                  ! Last non-blank character position

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the parameter name.
      CALL CHR_FANDL( PARAM, F, L )

*  Check the name is not blank.
      IF ( F .GT. L ) THEN
         STATUS = IMG__PARIN
         CALL ERR_REP( 'IMG1_VPAR_BLANK',
     :                 'Invalid blank parameter name given ' //
     :                 '(possible programming error).', STATUS )

*  Check the name is not too long.
      ELSE IF ( L - F + 1 .GT. IMG__SZPAR ) THEN
         STATUS = IMG__PARIN
         CALL MSG_SETC( 'PARAM', PARAM( F : L ) )
         CALL MSG_SETI( 'SZPAR', IMG__SZPAR )
         CALL ERR_REP( 'IMG1_VPAR_2LONG',
     :                 'Invalid parameter name ''^PARAM'' has more ' //
     :                 'than ^SZPAR characters (possible ' //
     :                 'programming error).', STATUS )

*  Check the name begins with an alphabetic character.
      ELSE IF ( .NOT. CHR_ISALF( PARAM( F : F ) ) ) THEN
         STATUS = IMG__PARIN
         CALL MSG_SETC( 'PARAM', PARAM( F : L ) )
         CALL ERR_REP( 'IMG1_VPAR_ALF',
     :                 'Invalid parameter name ''PARAM'' does not ' //
     :                 'start with an alphabetic character ' //
     :                 '(possible programming error).', STATUS )

*  Check the name continues with alphanumeric characters only.
      ELSE IF ( .NOT. CHR_ISNAM( PARAM( F : L ) ) ) THEN
         STATUS = IMG__PARIN
         CALL MSG_SETC( 'PARAM', PARAM( F : L ) )
         CALL ERR_REP( 'IMG1_VPAR_NTNAM',
     :                 'Invalid parameter name ''^PARAM'' contains ' //
     :                 'non-alphanumeric characters (possible ' //
     :                 'programming error).', STATUS )

*  If everything is OK, copy the name to the output argument and convert
*  it to upper case.
      ELSE
         VPAR = PARAM( F : L )
         CALL CHR_UCASE( VPAR )
      END IF

      END
* $Id$
