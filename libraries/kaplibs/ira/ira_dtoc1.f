      SUBROUTINE IRA_DTOC1( VALUE, SCS, NC, STYLE, TEXT, STATUS )
*+
*  Name:
*     IRA_DTOC1

*  Purpose:
*     Convert a single floating=point sky co-ordinate value to
*     character form.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_DTOC1( VALUE, SCS, NC, STYLE, TEXT, STATUS )

*  Description:
*     This routine creates a text string containing a formatted version
*     of the given sky co-ordinate value. The value is assumed to be a
*     longitude value if NC is 1, and a latitude if NC is 2. The
*     formats of the output string are as described in routine
*     IRA_DTOC. Longitude values are shifted into the range 0 - 2*PI
*     before being used.  Latitude values are shifted into the range
*     +/- PI before being used. An error is reported if a latitude
*     value then has an absolute value greater than PI/2 (this differs
*     from the behaviour of IRA_NORM which always reduces the latitude
*     value to +/- PI/2).

*  Arguments:
*     VALUE = DOUBLE PRECISION (Given)
*        The value of the sky co-ordinate to be formatted, in radians.
*        If VALUE has the Starlink "BAD" value (VAL__BADD) then the
*        output string TEXT is set blank.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky co-ordinate system in use (see ID2 section "Sky
*        Coordinates"). Any unambiguous abbreviation will do.
*     NC = INTEGER (Given)
*        Determines which sky co-ordinate is given. If a value of 1 is
*        supplied, VALUE is interpreted as a longitude value (e.g. RA if
*        an equatorial system is being used).  If a value of 2 is
*        supplied, VALUE is interpreted as a latitude value. Any other
*        value causes an error to be reported.
*     STYLE = INTEGER (Given)
*        A value in the range 1 to 5 which specifies the style of
*        output formatting required. In addition a value of zero can be
*        specified which causes a default style to be used dependant on
*        the value of SCS. See routine IRA_DTOC for a description of
*        the styles and defaults.
*     TEXT = CHARACTER * ( * ) (Returned)
*        The string containing the formatted description of the sky
*        co-ordinate value. The variable supplied for TEXT should have a
*        declared length equal to the value of parameter IRA__SZFSC.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1990 (DSB):
*        Original version.
*     7-MAR-1991 (DSB):
*        Style 4 added.
*     26-APR-1991 (DSB):
*        Modified for Version 2 of IRA.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Arguments Given:
      DOUBLE PRECISION VALUE
      CHARACTER SCS*(*)
      INTEGER NC
      INTEGER STYLE

*  Arguments Returned:
      CHARACTER TEXT*(*)

*  Status:
      INTEGER STATUS              ! Global status

*  Local Variables:
      CHARACTER        BJ*1      ! The type of epoch (Besselian or
                                 ! Julian) held by variable EQU.
      DOUBLE PRECISION EQU       ! The epoch of the reference equinox
                                 ! specified in argument SCS.
      DOUBLE PRECISION SLA_DRANGE ! SLALIB function.
      DOUBLE PRECISION SLA_DRANRM ! SLALIB function.
      DOUBLE PRECISION SVAL       ! The sky co-ordinate shifted
                                  ! into its first order range.
      CHARACTER NAME*(IRA__SZSCS)! Full name of SCS (with no equinox
                                  ! specifier)
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Verify the argument NC.
      IF( NC .NE. 1 .AND. NC .NE. 2 ) THEN
         STATUS = IRA__BADNC
         CALL MSG_SETI( 'NC', NC )
         CALL ERR_REP( 'IRA_DTOC1_ERR1',
     :       'IRA_DTOC1: Invalid value supplied for argument NC: ^NC',
     :                 STATUS )
      END IF

*  Verify the argument STYLE.
      IF( STYLE .LT. 0 .OR. STYLE .GT. 5 ) THEN
         STATUS = IRA__BADST
         CALL MSG_SETI( 'ST', STYLE )
         CALL ERR_REP( 'IRA_DTOC1_ERR2',
     :     'IRA_DTOC1: Invalid value supplied for argument STYLE: ^ST',
     :                 STATUS )
      END IF

*  Check that the output string is long enough.
      IF( LEN( TEXT ) .LT. IRA__SZFSC ) THEN
         STATUS = IRA__TOOSH
         CALL ERR_REP( 'IRA_DTOC1_ERR3',
     :         'IRA_DTOC1: Supplied text string is too short',
     :         STATUS )
      END IF

*  Identify the SCS.
      CALL IRA1_CHSCS( SCS, NAME, EQU, BJ, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Shift the input sky co-ordinate value into its first order range.
      IF( VALUE .NE. VAL__BADD ) THEN
         IF( NC .EQ. 1 ) THEN
            SVAL = SLA_DRANRM( VALUE )
         ELSE
            SVAL = SLA_DRANGE( VALUE )
         END IF
      END IF

*  Check that second sky co-ordinates values have a magnitude less than
*  or equal to pi/2.
      IF( NC .EQ. 2 .AND. ABS(SVAL) .GT. IRA__PIBY2 ) THEN
         STATUS = IRA__BAD2V
         CALL ERR_REP( 'IRA_DTOC1_ERR4',
     :     'IRA_DTOC1: Second sky co-ordinate has magnitude greater '//
     :     'than 90 degrees', STATUS )
      END IF

*  Call IRA1_IDTC1 to do the work.
      CALL IRA1_IDTC1( SVAL, NAME, NC, STYLE, TEXT, STATUS )

*  If an error occurred, give a contextual message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_DTOC1_ERR5',
     :     'IRA_DTOC1: Error formatting a sky co-ordinate position',
     :     STATUS )
      END IF

      END
