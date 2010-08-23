      SUBROUTINE IRA_GTCO1( PARAM, PROMPT, SCS, NC, DEFLT, VALUE,
     :                      STATUS )
*+
*  Name:
*     IRA_GTCO1

*  Purpose:
*     Obtains a single sky co-ordinate value from the ADAM environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_GTCO1( PARAM, PROMPT, SCS, NC, DEFLT, VALUE, STATUS )

*  Description:
*     The ADAM parameter specified by argument PARAM is used to acquire
*     a longitude or latitude value in the requested sky co-ordinate
*     system. Argument NC determines which is to be obtained.  The
*     string is decoded into a double precision number representing the
*     sky position.  See the documentation for IRA_CTOR for a
*     description of the allowed formats.  The input sky co-ordinate
*     value can optionally be communicated to the environment as a
*     dynamic default.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter used to get the sky
*        co-ordinate value.
*     PROMPT = CHARACTER * ( * ) (Given)
*        A string to override the current prompt for the parameter.
*        If this is blank, the prompt is left at its current value.
*        The initial value for the prompt is defined in the interface
*        file. Note, unlike routine IRA_GETCO, the axis name is not
*        automatically included in the prompt.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky co-ordinate system in use. Any unambiguous abbreviation
*        will do  (see ID2 section "Sky Coordinates").
*     NC = INTEGER (Given)
*        Determines which sky co-ordinate is to be returned. If a value
*        of 1 is supplied, the string obtained for the parameter is
*        interpreted as a longitude value (e.g. RA if an equatorial
*        system is being used). If a value of 2 is supplied, the string
*        is interpreted as a latitude value.  Any other value causes an
*        error to be reported.
*     DEFLT = LOGICAL (Given)
*        If true, then the value of VALUE on entry is communicated to
*        the environment as a dynamic default. If false, or if VALUE is
*        "BAD" on entry (i.e. equal to VAL__BADD), then no dynamic
*        default is set up.
*     VALUE = DOUBLE PRECISION (Given and Returned)
*        The sky co-ordinate value. On input it contains the default
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1990 (DSB):
*        Original version.
*     26-APR-1991 (DSB):
*        Modified for IRA version 2.
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
      CHARACTER PARAM*(*)
      CHARACTER PROMPT*(*)
      INTEGER   NC
      CHARACTER SCS*(*)
      LOGICAL   DEFLT

*  Arguments Given and Returned:
      DOUBLE PRECISION VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER        BJ*1      ! The type of epoch (Besselian or
                                 ! Julian) held by variable EQU.
      DOUBLE PRECISION EQU       ! The epoch of the reference equinox
                                 ! specified in argument SCS.
      CHARACTER NAME*(IRA__SZSCS)! Full SCS name.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Identify the SCS.
      CALL IRA1_CHSCS( SCS, NAME, EQU, BJ, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Verify the argument NC.
      IF( NC .NE. 1 .AND. NC .NE. 2 ) THEN
         STATUS = IRA__BADNC
         CALL MSG_SETI( 'NC', NC )
         CALL ERR_REP( 'IRA_GTCO1_ERR1',
     :       'IRA_GTCO1: Invalid value supplied for argument NC: ^NC',
     :                 STATUS )
      END IF

*  Call IRA1_IGTC1 to do the work.
      CALL IRA1_IGTC1( PARAM, PROMPT, NAME, NC, DEFLT, VALUE,
     :                 STATUS )

*  If an error occurred, give a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_GTCO1_ERR2',
     :     'IRA_GTCO1: Unable to obtain a sky co-ordinate value',
     :     STATUS )
      END IF

 999  CONTINUE

      END
