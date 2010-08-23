      SUBROUTINE IRA_GTSCS( SCSPAR, DEFLT, SCS, STATUS )
*+
*  Name:
*     IRA_GTSCS

*  Purpose:
*     Gets the full name of a Sky Co-ordinate System (with equinox
*     specifier) from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_GTSCS( SCSPAR, DEFLT, SCS, STATUS )

*  Description:
*     The ADAM parameter specified by argument SCSPAR is used to get a
*     character string from the environment. A check is done to make
*     sure that the string obtained represents a supported Sky
*     co-ordinate System (SCS). The user may include an equinox
*     specifier (see IRA_ISCS) in the text string to override the
*     default reference equinox of B1950. If an illegal SCS name is
*     entered the user is reprompted. If DEFLT is given true, the value
*     of SCS on entry is used as a default for the parameter.  The
*     value of SCS is expanded (both on entry and exit) to a full SCS
*     name (an abbreviation of the SCS may be supplied either by the
*     calling routine or by the user instead of the full name).

*  Arguments:
*     SCSPAR = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter to use, which should be of type
*        LITERAL.
*     DEFLT = LOGICAL (Given)
*        If true, then the value of argument SCS on entry is used (after
*        expansion) as the run-time default for the parameter.
*     SCS = CHARACTER * ( * ) (Given and Returned)
*        On entry, specifies the default value for the parameter. On
*        exit, contains the full version of the sky co-ordinate system
*        entered by the user. The supplied variable should have a
*        declared length given by symbolic constant IRA__SZSCS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-APR-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA error values.

*  Arguments Given:
      CHARACTER SCSPAR*(*)
      LOGICAL   DEFLT

*  Arguments Given and Returned:
      CHARACTER SCS*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BJ*1             ! Epoch type specifier (B or J).
      CHARACTER DEFSCS*(IRA__SZSCS)!Full default SCS.
      DOUBLE PRECISION EQU       ! Epoch of reference equinox.
      CHARACTER NAME*(IRA__SZSCS)! Full name of SCS without equinox
                                 ! specifier.
      CHARACTER SCSLST*(IRA__SZCLS)! List of supported SCS names.
      INTEGER  TSTAT             ! Temporary status value.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check declared length of SCS is not too small.
      IF( LEN( SCS ) .LT. IRA__SZSCS ) THEN
         STATUS = IRA__TOOSH
         CALL ERR_REP( 'IRA_GTSCS_ERR1',
     :   'IRA_GTSCS: Declared size of argument SCS is too small',
     :                 STATUS )
         GO TO 999
      END IF

*  If the value of SCS on entry is to be used as a default value, verify
*  that it is a legal value, and expand it if it is an abbreviation.
      IF( DEFLT ) THEN
         CALL IRA_GETEQ( SCS, EQU, BJ, NAME, STATUS )

*  Set the full SCS name and equinox specifier as the parameter
*  run-time default
         DEFSCS = SCS
         CALL PAR_DEF0C( SCSPAR, DEFSCS, STATUS )

*  If there is a problem weith the default value, quit.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

      END IF

*  Get a value for the SCS name from the environment.
 10   CONTINUE
      CALL PAR_GET0C( SCSPAR, SCS, STATUS )

*  Check it is legal.
      CALL IRA_GETEQ( SCS, EQU, BJ, NAME, STATUS )

*  If not, annul the error, cancel the parameter value, reinstate
*  the default value (if required), and reprompt. In addition, if an
*  unrecognised values was supplied, display the list of recognised
*  values.
      IF( STATUS .EQ. IRA__AMBSC .OR.
     :    STATUS .EQ. IRA__BADBJ .OR.
     :    STATUS .EQ. IRA__BADEQ .OR.
     :    STATUS .EQ. IRA__BADSC ) THEN


         TSTAT = STATUS
         CALL ERR_FLUSH( STATUS )

         IF( TSTAT .EQ. IRA__BADSC ) THEN
            CALL IRA_ISCS( SCSLST, STATUS )
            CALL MSG_OUT( 'IRA_ISCS_MSG1',
     :                    'Supported values (without optional '//
     :                    'equinox specifiers) are: '//SCSLST, STATUS )
         ENDIF

         CALL PAR_CANCL( SCSPAR, STATUS )
         IF( DEFLT ) CALL PAR_DEF0C( SCSPAR, DEFSCS, STATUS )
         GO TO 10

      END IF

 999  CONTINUE

*  If an error has occurred, give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PAR', SCSPAR )
         CALL ERR_REP( 'IRA_GTSCS_ERR1',
     :     'IRA_GTSCS: Unable to obtain legal value for parameter ^PAR',
     :          STATUS )
      END IF

      END
