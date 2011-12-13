      SUBROUTINE IRA_CTOD1( TEXT, SCS, NC, VALUE, STATUS )
*+
*  Name:
*     IRA_CTOD1

*  Purpose:
*     Converts a single formatted sky co-ordinate value into a
*     double-precision value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_CTOD1( TEXT, SCS, NC, VALUE, STATUS )

*  Description:
*     The input string is presumed to hold a sky co-ordinate value in
*     character form. If NC is 1, the string is interpreted as a
*     longitude value. If NC is 2, the string is interpreted as a
*     latitude value.  This routine reads the string and produces a
*     double precision value holding the co-ordinate value in radians.
*     The value is not shifted into the first order range (eg if an
*     angular value equivalent to 3*PI is given, the value 3*PI will be
*     returned, not 1*PI). If the input string is blank the output
*     value is set to the Starlink "BAD" value (VAL__BADD). Refer to
*     IRA_CTOD for details of the allowed format for the input string.

*  Arguments:
*     TEXT = CHARACTER * ( * ) (Given)
*        The string containing the formatted version of the sky
*        co-ordinate value. If this string is blank, VALUE is returned
*        with the "BAD" value (VAL__BADD), but no error report is
*        made.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky co-ordinate system (see ID2 section "Sky Coordinates").
*        Any unambiguous abbreviation will do.
*     NC = INTEGER (Given)
*        Determines which sky co-ordinate is to be used. If a value of 1
*        is supplied, the string is interpreted as a longitude value
*        (e.g. RA if an equatorial system is being used). If a value of
*        2 is supplied, the string is interpreted as a latitude value.
*        Any other value results in an error being reported.
*     VALUE = DOUBLE PRECISION (Returned)
*        The numerical value of the sky co-ordinate represented by the
*        string in TEXT. The value is in radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1990 (DSB):
*        Original version.
*     26-APR-1991 (DSB):
*        Modified for IRA Version 2.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Arguments Given:
      CHARACTER TEXT*(*)
      CHARACTER SCS*(*)
      INTEGER   NC

*  Arguments Returned:
      DOUBLE PRECISION VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER        BJ*1      ! The type of epoch (Besselian or
                                 ! Julian) held by variable EQU.
      DOUBLE PRECISION EQU       ! The epoch of the reference equinox
                                 ! specified in argument SCS.
      CHARACTER NAME*(IRA__SZSCS) ! The full name of the SCS
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Verify the argument NC.
      IF( NC .NE. 1 .AND. NC .NE. 2 ) THEN
         STATUS = IRA__BADNC
         CALL MSG_SETI( 'NC', NC )
         CALL ERR_REP( 'IRA_CTOD1_ERR1',
     :       'IRA_CTOD1: Invalid value supplied for argument NC: ^NC',
     :                 STATUS )
         GO TO 999
      END IF

*  Identify the sky co-ordinate system.
      CALL IRA1_CHSCS( SCS, NAME, EQU, BJ, STATUS )

*  Call IRA1_ICTD1 to do the work.
      CALL IRA1_ICTD1( TEXT, NAME, NC, VALUE, STATUS )

*  If any error occurred, give a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_CTOD1_ERR2',
     :     'IRA_CTOD1: Unable to read formatted sky co-ordinate value',
     :     STATUS )
      END IF

 999  CONTINUE

      END
