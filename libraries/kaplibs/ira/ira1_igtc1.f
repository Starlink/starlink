      SUBROUTINE IRA1_IGTC1( PARAM, PROMPT, NAME, NC, DEFLT, VALUE,
     :                      STATUS )
*+
*  Name:
*     IRA1_IGTC1

*  Purpose:
*     Gets a single sky coordinate value from the ADAM environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_IGTC1( PARAM, PROMPT, NAME, NC, DEFLT, VALUE, STATUS )

*  Description:
*     This routine provides the functionality of IRA_GTCO1
*     without argument verification or context messages.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter used to get the sky
*        coordinate value.
*     PROMPT = CHARACTER * ( * ) (Given)
*        A string to override the current prompt for the parameter.
*        If this is blank, the prompt is left at its current value.
*        The initial value for the prompt is defined in the interface
*        file.
*     NAME = CHARACTER * ( * ) (Given)
*        The full sky coordinate system name (with no equinox
*        specifier).
*     NC = INTEGER (Given)
*        Determines which sky coordinate is to be returned. If a value
*        of 1 is supplied, the string obtained for the parameter is
*        interpreted as a longitude value (eg RA if an equatorial
*        system is being used). If a value of 2 is supplied, the string
*        is interpreted as a latitude value.
*     DEFLT = LOGICAL (Given)
*        If true, then the input value is communicated to
*        the environment as a dynamic default. If false, or if VALUE is
*        "BAD" on entry (i.e. equal to VAL__BADD), then no dynamic
*        default is set up.
*     VALUE = DOUBLE PRECISION (Given and Returned)
*        The sky coordinate value. On input it contains the default
*        value (in radians) to use if DEFLT is true. On exit it
*        contains the decoded value obtained from the environment, in
*        radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     20-SEP-1990 (DSB):
*        Original version.
*     26-APR-1991 (DSB):
*        Modified for version 2 of IRA
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants (VAL__)
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors
      INCLUDE 'PAR_ERR'          ! PAR errors.

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER PROMPT*(*)
      INTEGER   NC
      CHARACTER NAME*(*)
      LOGICAL   DEFLT

*  Arguments Given and Returned:
      DOUBLE PRECISION VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER   CHR_LEN          ! Function giving used length of a
                                 ! string.
      CHARACTER TEXT*(IRA__SZFSC)! String obtained from the
                                 ! environment.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If required, set up the default for the parameter.
      IF( DEFLT .AND. VALUE .NE. VAL__BADD ) THEN

*  Format the input value into the character string.
         CALL IRA_DTOC1( VALUE, NAME, NC, 0, TEXT, STATUS )

*  Make these values the default for the given ADAM parameter.
         CALL PAR_DEF0C( PARAM, TEXT, STATUS )

*  Otherwise set the string blank.
      ELSE
         TEXT = ' '

      END IF

*  If required, set up the parameter prompt.
      IF( PROMPT .NE. ' ' ) THEN
         CALL PAR_PROMT( PARAM, PROMPT(:CHR_LEN(PROMPT)), STATUS )

      END IF

*  Obtain a value for the sky cordinate from the environment.
  10  CALL PAR_GET0C( PARAM, TEXT, STATUS )

*  If no value was obtained, exit.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to decode the string.
      CALL IRA1_ICTD1( TEXT, NAME, NC, VALUE, STATUS )

*  If unable to decode the string, reprompt.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         CALL PAR_CANCL( PARAM, STATUS )
         GO TO 10
      END IF

 999  CONTINUE

      END
