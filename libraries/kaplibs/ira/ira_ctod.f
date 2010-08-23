      SUBROUTINE IRA_CTOD( ATEXT, BTEXT, SCS, A, B, STATUS )
*+
*  Name:
*     IRA_CTOD

*  Purpose:
*     Converts formatted sky co-ordinate values into double-precision
*     values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_CTOD( ATEXT, BTEXT, SCS, A, B, STATUS )

*  Description:
*     The input strings are presumed to hold sky co-ordinate values in
*     character form.  This routine reads the strings and produces
*     double precision values holding the co-ordinate values, in
*     radians.
*
*     Each input string can consist of a set of up to three "fields".
*     Each field starts with a numeric value (which can have a
*     fractional part) terminated by a character string. This character
*     string consists of an optional single character, called the
*     terminator character, followed by an arbitrary number of spaces
*     The terminator character (if present) must be one of the letters
*     h,d,m,s or r.  An h terminator indicates that the field value is
*     in units of hours. A d terminator indicates that the field value
*     is in units of degrees.  An r terminator indicates that the field
*     value is in units of radians.  An m terminator indicates that the
*     field value is in units of minutes.  An s terminator indicates
*     that the field value is in units of seconds.  The interpretation
*     of minutes and seconds depends on whether the value is a time
*     value or an angle value. The longitude value for equatorial sky
*     co-ordinate systems (RA) is expected to be a measure of time, all
*     other co-ordinate values are expected to be a measure of angle.
*     These defaults are overriden if the first field is a "degrees",
*     "hours" or "radians" field (as indicated by the presence of a d,
*     h or r terminator character).  The interpretation of fields with
*     no terminator character depends on which field is being
*     considered. If the first field has no terminator, it is assumed
*     to be either a degrees or hours field.  If the second or third
*     field has no terminator, it is assumed to be a seconds field if
*     the previous field was a minutes field, and a minutes field if
*     the previous value was an hours or degrees field.
*
*     In addition, an input string may contain a single field with no
*     terminator character in an "encoded" form. "Encoded" fields are
*     identified by the fact that the field contains 5 or more digits
*     to the left of the decimal point (including leading zeros if
*     necessary).  These fields are decoded into hours or degrees as
*     follows: Any fractional part is taken as the fractional part of
*     the seconds field, the tens and units digits are taken as the
*     integer part of the seconds field, the hundreds and thousands
*     digits are taken as the minutes fields, the remaining digits are
*     taken as the degrees or hours field. Thus -12345.4 would be
*     interpreted as (- 1 hour 23 mins 45.4 seconds) or (- 1 degree 23
*     mins 45.4 seconds). The same value could also be specified as
*     -1 23 45.5, -1h 23m 45.5s (if it represents a time value), or
*     -1d 23 45.5 (if it represents an angular value).
*
*     The supplied values must be in their first order ranges (i.e. 0
*     to 2.PI for longitude values and -PI/2 to +PI/2 for latitude
*     values). Values outside these ranges cause an error to be
*     reported, and the status value IRA__RANGE is returned). The
*     exception to this is if the string is prefixed with a "*"
*     character, in which case any numeric value may be supplied. In
*     this case the supplied value is returned directly (e.g. if the
*     string "*400D" is given, the radian equivalent of 400 degrees
*     will be returned, not 40 (=400-360) degrees).  If either of the
*     input strings are blank the corresponding output value is set to
*     the Starlink "BAD" value (VAL__BADD).

*  Arguments:
*     ATEXT = CHARACTER * ( * ) (Given)
*        The string containing the formatted version of the longitude
*        value. If this string is blank, A is returned with the "BAD"
*        value (VAL__BADD), but no error is reported.
*     BTEXT = CHARACTER * ( * ) (Given)
*        The string containing the formatted version of the latitude
*        value. If this string is blank, B is returned with the "BAD"
*        value (VAL__BADD), but no error is reported.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky co-ordinate system (see ID2 section "Sky Coordinates").
*        Any unambiguous abbreviation will do.
*     A = DOUBLE PRECISION (Returned)
*        The longitude value represented by the string ATEXT, in
*        radians.
*     B = DOUBLE PRECISION (Returned)
*        The latitude value represented by the string BTEXT, in
*        radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990, 1991, 1992 Science & Engineering Research Council.
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
*

*  History:
*     20-SEP-1990 (DSB):
*        Original version.
*     26-APR-1991 (DSB):
*        Modified for version 2 of IRA
*     4-MAR-1992 (DSB):
*        "*" facility documented.
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
      CHARACTER ATEXT*(*)
      CHARACTER BTEXT*(*)
      CHARACTER SCS*(*)

*  Arguments Returned:
      DOUBLE PRECISION A
      DOUBLE PRECISION B

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER        BJ*1      ! The type of epoch (Besselian or
                                 ! Julian) held by variable EQU.
      DOUBLE PRECISION EQU       ! The epoch of the reference equinox
                                 ! specified in argument SCS.
      CHARACTER NAME*(IRA__SZSCS) ! The full name of the SCS (with no
                                 ! equinox specifier).

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Identify the sky co-ordinate system.
      CALL IRA1_CHSCS( SCS, NAME, EQU, BJ, STATUS )

*  Convert the first sky co-ordinate value.
      CALL IRA1_ICTD1( ATEXT, NAME, 1, A, STATUS )

*  Convert the second sky co-ordinate value.
      CALL IRA1_ICTD1( BTEXT, NAME, 2, B, STATUS )

*  If an error occurred, give a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'AT', ATEXT )
         CALL MSG_SETC( 'BT', BTEXT )
         CALL ERR_REP( 'IRA_CTOD_ERR1',
     :     'IRA_CTOD: Unable to read sky co-ordinate value from '//
     :     '"^AT" or "^BT"', STATUS )
      END IF

      END
