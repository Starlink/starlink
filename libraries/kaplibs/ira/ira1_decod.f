      SUBROUTINE IRA1_DECOD( IN, OUT, BADFLD, STATUS )
*+
*  Name:
*     IRA1_DECOD

*  Purpose:
*     Decode an "encoded" coordinate value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_DECOD( IN, OUT, BADFLD, STATUS )

*  Description:
*     The input value represents a time value in the form
*     ..hhhhmmss.ss.., or an angular value in the form ..ddddmmss.ss..
*     The hours (h) or degrees (d) fields can have any number of
*     digits, as can the fractional part of the seconds (s) field. The
*     output value is a normal DOUBLE PRECISION value in units of hours
*     or degrees. The absolute value of the input is used, but if the
*     input is negative, then the output is negated before being
*     returned.

*  Arguments:
*     IN = DOUBLE PRECISION (Given)
*        The input "encoded" value.
*     OUT = DOUBLE PRECISION (Returned)
*        The output value in units of hours or degrees.
*     BADFLD = REAL (Returned)
*        If either the minutes or seconds field has a value greater
*        than 60, STATUS is returned equal to IRA__RANGE, and the
*        illegal field value is returned in BADFLD. NOTE, no error
*        is reported in this situation (an exception to standard
*        Starlink policy!).
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
*     11-JAN-1991 (DSB):
*        Original version.
*     26-APR-1991 (DSB):
*        Changed for 2nd version of IRA
*     18-SEP-1991 (DSB):
*        Checks on minutes and seconds field values included.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_ERR'          ! IRA error values.

*  Arguments Given:
      DOUBLE PRECISION  IN

*  Arguments Returned:
      DOUBLE PRECISION  OUT
      REAL   BADFLD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER INTPT              ! Integer part after division.
      DOUBLE PRECISION    REMPT  ! Remainder after subtraction of
                                 ! integer part.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Divide by 10000.0. The integer part is the hours or degrees
*  field.
      INTPT = INT( ABS(IN)/10000.0D0 )
      REMPT = ABS(IN) - INTPT*10000.0D0
      OUT = INTPT

*  Divide the remainder by 100.0. The integer part of the result
*  is the minutes field.

      INTPT = INT( REMPT/100.0D0 )
      REMPT = REMPT - INTPT*100.0D0
      OUT = OUT + INTPT/60.0D0

*  Check that the minutes field is less than 60.
      IF( INTPT .GE. 60 ) THEN
         STATUS = IRA__RANGE
         BADFLD = REAL( INTPT )
         GO TO 999
      END IF

*  The remainder is the seconds field.
      OUT = OUT + REMPT/3600.0D0

*  Check that the seconds field is less than 60.
      IF( REMPT .GE. 60 ) THEN
         STATUS = IRA__RANGE
         BADFLD = REAL( REMPT )
         GO TO 999
      END IF

*  Transfer the sign from the input value.
      OUT = SIGN( OUT, IN )

 999  CONTINUE

      END
